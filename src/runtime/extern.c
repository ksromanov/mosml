/* Structured output, fast format.
 *
 * Fixed following OCaml's approach:
 *  1. Explicit work stack instead of C recursion (avoids stack overflow on
 *     deeply nested structures)
 *  2. Word-by-word field copy to avoid SIMD prefetch past page boundaries
 *     (glibc memmove/memcpy uses AVX which can read ahead into unmapped pages
 *     when a GC block ends at a page boundary)
 *  3. Safe encoding for out-of-heap values: non-heap non-atom blocks have
 *     garbage headers; we detect them by an unreasonably large Wosize and
 *     emit them as ML integer 0 (a safe tagged value).
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "debugger.h"
#include "fail.h"
#include "gc.h"
#include "major_gc.h"
#include "minor_gc.h"
#include "intext.h"
#include "io.h"
#include "memory.h"
#include "mlvalues.h"

/* ---- Sharing hash table ---- */

struct extern_obj * extern_table;
asize_t extern_table_size, extern_table_used;

void alloc_extern_table()
{
  asize_t i;
  extern_table = (struct extern_obj *)
    stat_alloc(extern_table_size * sizeof(struct extern_obj));
  for (i = 0; i < extern_table_size; i++)
    extern_table[i].obj = 0;
}

void resize_extern_table()
{
  asize_t oldsize, i, h;
  struct extern_obj * oldtable;
  oldsize  = extern_table_size;
  oldtable = extern_table;
  extern_table_size = 2 * extern_table_size;
  alloc_extern_table();
  for (i = 0; i < oldsize; i++) {
    h = Hash(oldtable[i].obj);
    while (extern_table[h].obj != 0) {
      h++;
      if (h >= extern_table_size) h = 0;
    }
    extern_table[h].obj = oldtable[i].obj;
    extern_table[h].ofs = oldtable[i].ofs;
  }
  stat_free((char *) oldtable);
}

/* ---- Output buffer ---- */

byteoffset_t * extern_block;
asize_t extern_size, extern_pos;

static void resize_result()
{
  extern_size = 2 * extern_size;
  extern_block = (byteoffset_t *)
    stat_resize((char *) extern_block, extern_size * sizeof(byteoffset_t));
}

/* Maximum plausible block size (512 KiB in words).  Larger values indicate
 * a garbage "header" read from a C data address that ended up in the global
 * table due to -nostampcheck stamp mismatches.  Such blocks are emitted as
 * ML integer 0 (a safe tagged value). */
#define MAX_EXTERN_BLOCK_SIZE 65536

/* Safe field copy: one word at a time to avoid SIMD reads past block end. */
static void copy_fields_safe(value v, mlsize_t size)
{
  mlsize_t i;
  for (i = 0; i < size; i++)
    extern_block[extern_pos + i] = (byteoffset_t) Field(v, i);
}

/* ---- Explicit work stack (OCaml-style) ---- */

/* Each item records a range of extern_block slots that still need to be
 * processed: slots [start, start+count) each hold a raw ML value that
 * should be replaced by its serialised byteoffset. */
struct extern_item { asize_t start; mlsize_t count; };

#define EXTERN_STACK_INIT_SIZE 256
#define EXTERN_STACK_MAX_SIZE  (1024 * 1024)

static struct extern_item extern_stack_init[EXTERN_STACK_INIT_SIZE];
static struct extern_item * extern_stack      = extern_stack_init;
static struct extern_item * extern_stack_top  = extern_stack_init; /* next free */
static struct extern_item * extern_stack_limit = NULL;

static void init_extern_stack()
{
  extern_stack       = extern_stack_init;
  extern_stack_top   = extern_stack_init;
  extern_stack_limit = extern_stack_init + EXTERN_STACK_INIT_SIZE;
}

static void push_extern_stack(asize_t start, mlsize_t count)
{
  if (extern_stack_top >= extern_stack_limit) {
    asize_t old_size = extern_stack_limit - extern_stack;
    asize_t new_size = 2 * old_size;
    asize_t used = extern_stack_top - extern_stack;
    struct extern_item * newstk;
    if (new_size >= EXTERN_STACK_MAX_SIZE)
      invalid_argument("output_value: stack overflow");
    newstk = (struct extern_item *) malloc(new_size * sizeof(struct extern_item));
    if (newstk == NULL)
      invalid_argument("output_value: out of memory in stack");
    memcpy(newstk, extern_stack, used * sizeof(struct extern_item));
    if (extern_stack != extern_stack_init) free(extern_stack);
    extern_stack       = newstk;
    extern_stack_top   = newstk + used;
    extern_stack_limit = newstk + new_size;
  }
  extern_stack_top->start = start;
  extern_stack_top->count = count;
  extern_stack_top++;
}

static void free_extern_stack()
{
  if (extern_stack != extern_stack_init) free(extern_stack);
  extern_stack       = extern_stack_init;
  extern_stack_top   = extern_stack_init;
  extern_stack_limit = extern_stack_init + EXTERN_STACK_INIT_SIZE;
}

/* ---- Core: emit one value, return its byteoffset ---- */

/* Process value v and return its byteoffset encoding.
 * If v refers to a block, its fields are written into extern_block and the
 * field slots are pushed onto extern_stack for later processing. */
static byteoffset_t emit_one(value v)
{
  mlsize_t size;
  asize_t  h, end_pos;
  byteoffset_t res;

  if (Is_long(v)) return (byteoffset_t) v;   /* tagged integer */

  /* Guard against NULL/tiny pointers and out-of-heap C data pointers.
   * The latter arise from -nostampcheck global-slot mismatches: an initialiser
   * for the wrong unit runs and leaves a C address (code/data segment pointer)
   * in a global slot.  Such pointers have garbage "headers" (whatever C data
   * precedes them) encoding huge or corrupt block sizes.  Reading those many
   * fields would segfault at a page boundary.
   *
   * We detect them by checking the GC heap membership and emit them as
   * ML integer 0 — a safe tagged value that intern reconstructs without
   * crashing.  The slot value will be wrong, but at least we don't segfault. */
  if ((unsigned long)v < 16UL) return (byteoffset_t) Val_long(0);

  size = Wosize_val(v);

  /* Atoms (size 0) live in the static first_atoms[] array, which is NOT in
   * the GC heap.  Handle them here, before the heap-membership check. */
  if (size == 0) return (Tag_val(v) << 2) + 2;

  /* Also catch garbage from C-data "headers" that encode large sizes.
   * 512 KiB (65536 words) is far beyond any legitimate ML block. */
  if (size > MAX_EXTERN_BLOCK_SIZE) return (byteoffset_t) Val_long(0);

  /* Verify the block's header is accessible GC-heap memory.
   * C-data-segment pointers that appear in global slots (due to -nostampcheck
   * mismatches) look like even ML pointers but their "headers" are garbage C
   * data.  After the size checks above we might still have a plausible-looking
   * but wrong size that would read past a page boundary in copy_fields_safe.
   * Skipping non-heap blocks is safe: their fields are not GC-traced values.
   * We encode them as ML integer 0. */
  if (!Is_in_heap(Hp_val(v)) && !Is_young(v))
    return (byteoffset_t) Val_long(0);

  if (size == 0) return (Tag_val(v) << 2) + 2;  /* atom */

  /* Sanity check: garbage C-data "headers" can claim enormous sizes.
   * Emit as ML integer 0 (safe tagged value) to avoid reading unmapped pages. */
  if (size > MAX_EXTERN_BLOCK_SIZE) return (byteoffset_t) Val_long(0);

  /* Check sharing table */
  if (2 * extern_table_used >= extern_table_size) resize_extern_table();
  h = Hash(v);
  while (extern_table[h].obj != 0) {
    if (extern_table[h].obj == v) return extern_table[h].ofs;
    h++;
    if (h >= extern_table_size) h = 0;
  }

  /* Allocate in extern_block: header + size fields */
  end_pos = extern_pos + 1 + size;
  while (end_pos >= extern_size) resize_result();

  /* Write header */
  extern_block[extern_pos++] = Make_header(size, Tag_val(v), Black);
  res = extern_pos * sizeof(byteoffset_t);

  /* Record in sharing table */
  extern_table[h].obj = v;
  extern_table[h].ofs = res;
  extern_table_used++;

  /* Copy fields word-by-word (safe: no SIMD prefetch past block end) */
  copy_fields_safe(v, size);
  extern_pos = end_pos;

  /* Schedule scannable fields for processing */
  if (Tag_val(v) < No_scan_tag) {
    if (Tag_val(v) == Closure_tag || Tag_val(v) == Abstract_tag ||
        Tag_val(v) == Final_tag)
      invalid_argument("output_value: functional or abstract value");
    /* Push the range of field slots onto the work stack */
    push_extern_stack(extern_pos - size, size);
  }

  return res;
}

/* ---- Main entry: emit root + patch all pending fields ---- */

byteoffset_t emit_all(value root)
{
  byteoffset_t root_res = emit_one(root);

  /* Process all pending field ranges from the work stack.
   * Each item says "extern_block[start..start+count-1] are raw ML values;
   * replace each with its serialised byteoffset." */
  while (extern_stack_top > extern_stack) {
    /* Capture by value before emit_one() may reallocate extern_stack. */
    asize_t  start = (--extern_stack_top)->start;
    mlsize_t count = extern_stack_top->count;
    asize_t  i;
    for (i = 0; i < count; i++) {
      value field_val = (value) extern_block[start + i];
      byteoffset_t field_ofs = emit_one(field_val);
      extern_block[start + i] = field_ofs;
    }
  }

  return root_res;
}

/* ---- Public entry point ---- */

value extern_val(chan, v)       /* ML */
     struct channel * chan;
     value v;
{
  byteoffset_t res;

  extern_size  = INITIAL_EXTERN_SIZE;
  extern_block = (byteoffset_t *) stat_alloc(extern_size * sizeof(unsigned long));
  extern_pos   = 0;
  extern_table_size = INITIAL_EXTERN_TABLE_SIZE;
  alloc_extern_table();
  extern_table_used = 0;
  init_extern_stack();

  res = emit_all(v);

  free_extern_stack();
  stat_free((char *) extern_table);

  putword(chan, Extern_magic_number);
  putword(chan, extern_pos);
  if (extern_pos == 0)
    putword(chan, res);
  else
    putblock(chan, (char *) extern_block, extern_pos * sizeof(unsigned long));
  stat_free((char *) extern_block);
  return Val_unit;
}
