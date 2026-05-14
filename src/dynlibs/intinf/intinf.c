/* intinf.c -- GMP-backed arbitrary-precision integers for Moscow ML.
 *
 * Zarith-style dual representation:
 *   - Small integers: Val_long(n) tagged integer (zero allocation, fast path)
 *   - Large integers: Final_tag block with limbs stored ON the mosml heap
 *
 * Block layout for large integers:
 *   Field(0): no-op finalizer pointer   (required by Final_tag)
 *   Field(1): (sign_bit << 63) | nlimbs (sign+size word)
 *   Field(2..nlimbs+1): mp_limb_t limbs (stored directly in block)
 *
 * Because Final_tag > No_scan_tag, the GC does not scan fields as ML values,
 * so storing raw integers (limbs) in the block is safe.
 * No mpz_clear finalizer is needed -- the block is freed by the GC normally.
 * No malloc/free for limbs -- they live inside the mosml heap block.
 */

#include <gmp.h>
#include <stdlib.h>
#include <string.h>

#include <mlvalues.h>
#include <fail.h>
#include <alloc.h>
#include <globals.h>
#include <memory.h>
#include <str.h>

/* -----------------------------------------------------------------------
 * Block layout macros
 * ----------------------------------------------------------------------- */

#define ZINT_SIGN_BIT  ((unsigned long)1 << (8*sizeof(value)-1))
#define ZINT_HEAD(v)   (Field((v), 1))
#define ZINT_SIGN(v)   (ZINT_HEAD(v) & ZINT_SIGN_BIT)
#define ZINT_SIZE(v)   ((mp_size_t)(ZINT_HEAD(v) & ~ZINT_SIGN_BIT))
#define ZINT_LIMBS(v)  ((mp_limb_t*)(&Field((v), 2)))

/* Maximum limbs for stack-allocated temporaries.
 * 256 limbs = 256*64 = 16384 bits -- far more than needed for MLKit bootstrap. */
#define MAX_LIMBS 256

/* -----------------------------------------------------------------------
 * Finalizer (no-op: limbs are inside the block, freed by GC)
 * ----------------------------------------------------------------------- */

static void zint_finalize(value v) {
    /* Nothing: limbs are in the block itself, not malloc'd separately. */
    (void)v;
}

/* -----------------------------------------------------------------------
 * Allocation: put a big-int block on the major heap
 * ----------------------------------------------------------------------- */

static value zint_alloc(mp_size_t nlimbs) {
    /* wosize = nlimbs (limb words) + 1 (sign+size) + 1 (finalizer) = nlimbs+2 */
    value r = alloc_final(nlimbs + 2, zint_finalize, (nlimbs + 2) * sizeof(value), 1048576);
    ZINT_HEAD(r) = 0;
    return r;
}

/* -----------------------------------------------------------------------
 * Normalize: build the canonical representation of the result.
 * Trims leading zero limbs. Returns Val_long if it fits, block otherwise.
 * ----------------------------------------------------------------------- */

static value zint_normalize(mp_limb_t *limbs, mp_size_t sz, int negative) {
    while (sz > 0 && limbs[sz-1] == 0) sz--;
    if (sz == 0) return Val_long(0);
    if (sz == 1) {
        mp_limb_t top = limbs[0];
        if (!negative && top <= (mp_limb_t)Max_long)
            return Val_long((long)top);
        if (negative && top <= (mp_limb_t)Max_long + 1)
            return Val_long(-(long)top);
    }
    value r = zint_alloc(sz);
    memcpy(ZINT_LIMBS(r), limbs, sz * sizeof(mp_limb_t));
    ZINT_HEAD(r) = (value)sz | (negative ? ZINT_SIGN_BIT : 0);
    return r;
}

/* -----------------------------------------------------------------------
 * Decompose a value into (sign, size, limbs).
 * Handles both Is_long and block forms.
 * buf must have space for at least MAX_LIMBS limbs.
 * Returns the number of limbs (0 for zero).
 * ----------------------------------------------------------------------- */

static mp_size_t zint_decompose(value v, mp_limb_t *buf, int *negative) {
    if (Is_long(v)) {
        long n = Long_val(v);
        if (n == 0) { *negative = 0; return 0; }
        *negative = (n < 0);
        buf[0] = (mp_limb_t)(*negative ? -(unsigned long)n : (unsigned long)n);
        return 1;
    }
    *negative = (ZINT_SIGN(v) != 0);
    mp_size_t sz = ZINT_SIZE(v);
    memcpy(buf, ZINT_LIMBS(v), sz * sizeof(mp_limb_t));
    return sz;
}

/* -----------------------------------------------------------------------
 * Equality (registered with register_final_equal hook)
 * ----------------------------------------------------------------------- */

static int zint_equal_cmp(value v1, value v2) {
    if (v1 == v2) return 1;
    /* Both must be Final_tag blocks (Is_long case handled by sml_equal_aux) */
    if (ZINT_SIGN(v1) != ZINT_SIGN(v2)) return 0;
    mp_size_t s1 = ZINT_SIZE(v1), s2 = ZINT_SIZE(v2);
    if (s1 != s2) return 0;
    return mpn_cmp(ZINT_LIMBS(v1), ZINT_LIMBS(v2), s1) == 0;
}

extern void register_final_equal(void (*finalizer)(value), int (*eq_fn)(value, value));

value largeint_register_equal(value unit) {
    register_final_equal(zint_finalize, zint_equal_cmp);
    return Val_unit;
}

/* -----------------------------------------------------------------------
 * Construction
 * ----------------------------------------------------------------------- */

value largeint_make(value unit) {
    /* Create the zero value -- cheaply */
    return Val_long(0);
}

value largeint_make_si(value src) {
    return Val_long(Long_val(src));
}

value largeint_clear(value obj) {
    /* No-op: no external resources to free */
    return Val_unit;
}

value largeint_set(value dest, value src) {
    /* Unused in new functional API; kept for ABI compat */
    return Val_unit;
}

value largeint_set_si(value dest, value src) {
    return Val_unit;
}

/* -----------------------------------------------------------------------
 * Conversion
 * ----------------------------------------------------------------------- */

value largeint_to_si(value src) {
    if (Is_long(src)) return src;
    /* Block case: check it fits */
    mp_size_t sz = ZINT_SIZE(src);
    if (sz == 0) return Val_long(0);
    if (sz != 1) { raiseprimitive0(SYS__EXN_OVERFLOW); }
    mp_limb_t top = ZINT_LIMBS(src)[0];
    int neg = (ZINT_SIGN(src) != 0);
    if (!neg && top <= (mp_limb_t)Max_long) return Val_long((long)top);
    if (neg && top <= (mp_limb_t)Max_long + 1) return Val_long(-(long)top);
    raiseprimitive0(SYS__EXN_OVERFLOW);
    return Val_unit; /* unreachable */
}

/* -----------------------------------------------------------------------
 * Comparison
 * ----------------------------------------------------------------------- */

value largeint_cmp(value li1, value li2) {
    /* Fast path: both small */
    if (Is_long(li1) && Is_long(li2)) {
        long a = Long_val(li1), b = Long_val(li2);
        if (a < b) return Val_long(-1);
        if (a > b) return Val_long(1);
        return Val_long(0);
    }
    mp_limb_t la[MAX_LIMBS], lb[MAX_LIMBS];
    int sna, snb;
    mp_size_t sza = zint_decompose(li1, la, &sna);
    mp_size_t szb = zint_decompose(li2, lb, &snb);
    /* Different signs */
    if (sna != snb) return Val_long(sna ? -1 : 1);
    int cmp;
    if (sza != szb) cmp = (sza > szb) ? 1 : -1;
    else if (sza == 0) cmp = 0;
    else cmp = mpn_cmp(la, lb, sza);
    if (sna) cmp = -cmp;  /* negative: flip */
    return Val_long(cmp < 0 ? -1 : cmp > 0 ? 1 : 0);
}

value largeint_cmp_si(value li, value si) {
    value sibox = Val_long(Long_val(si));
    return largeint_cmp(li, sibox);
}

/* -----------------------------------------------------------------------
 * Negation and absolute value
 * ----------------------------------------------------------------------- */

value largeint_neg(value dest_ignored, value src) {
    if (Is_long(src)) {
        long n = Long_val(src);
        /* Handle Min_long: -Min_long overflows, produce big-int */
        if (n == Min_long) {
            mp_limb_t lim = (mp_limb_t)((unsigned long)Max_long + 1);
            return zint_normalize(&lim, 1, 0);
        }
        return Val_long(-n);
    }
    /* Flip sign bit on block */
    mp_size_t sz = ZINT_SIZE(src);
    value r = zint_alloc(sz);
    memcpy(ZINT_LIMBS(r), ZINT_LIMBS(src), sz * sizeof(mp_limb_t));
    ZINT_HEAD(r) = (value)sz | (ZINT_SIGN(src) ? 0 : ZINT_SIGN_BIT);
    return r;
}

/* -----------------------------------------------------------------------
 * Addition
 * ----------------------------------------------------------------------- */

value largeint_add(value dest_ignored, value a, value b) {
    /* Fast path: both small; result must fit in mosml's tagged int range [Min_long, Max_long] */
    if (Is_long(a) && Is_long(b)) {
        long x = Long_val(a), y = Long_val(b), z = x + y;
        if (z >= Min_long && z <= Max_long) return Val_long(z);
    }
    mp_limb_t la[MAX_LIMBS], lb[MAX_LIMBS], lr[MAX_LIMBS + 1];
    int sna, snb;
    mp_size_t sza = zint_decompose(a, la, &sna);
    mp_size_t szb = zint_decompose(b, lb, &snb);
    mp_size_t szr;
    int snr;
    if (sna == snb) {
        /* Same sign: add magnitudes; mpn_add requires first arg >= second in size */
        if (sza >= szb) {
            lr[sza] = mpn_add(lr, la, sza, lb, szb);
            szr = sza + (lr[sza] != 0);
        } else {
            lr[szb] = mpn_add(lr, lb, szb, la, sza);
            szr = szb + (lr[szb] != 0);
        }
        snr = sna;
    } else {
        /* Different signs: subtract smaller magnitude from larger */
        int cmp = (sza != szb) ? (sza > szb ? 1 : -1) : mpn_cmp(la, lb, sza);
        if (cmp >= 0) {
            mpn_sub(lr, la, sza, lb, szb);
            szr = sza;
            snr = sna;
        } else {
            mpn_sub(lr, lb, szb, la, sza);
            szr = szb;
            snr = snb;
        }
    }
    return zint_normalize(lr, szr, snr);
}

/* -----------------------------------------------------------------------
 * Subtraction
 * ----------------------------------------------------------------------- */

value largeint_sub(value dest_ignored, value a, value b) {
    if (Is_long(a) && Is_long(b)) {
        long x = Long_val(a), y = Long_val(b), z = x - y;
        if (z >= Min_long && z <= Max_long) return Val_long(z);
    }
    mp_limb_t la[MAX_LIMBS], lb[MAX_LIMBS], lr[MAX_LIMBS + 1];
    int sna, snb;
    mp_size_t sza = zint_decompose(a, la, &sna);
    mp_size_t szb = zint_decompose(b, lb, &snb);
    snb = !snb;  /* Negate b's sign for subtraction */
    /* Same sign after negation = subtraction, different = addition */
    mp_size_t szr; int snr;
    int eff_sna = sna, eff_snb = snb;
    mp_size_t eff_sza = sza, eff_szb = szb;
    mp_limb_t *pa = la, *pb = lb;
    if (eff_sza < eff_szb) {
        mp_limb_t *tmp = pa; pa = pb; pb = tmp;
        mp_size_t ts = eff_sza; eff_sza = eff_szb; eff_szb = ts;
        int tn = eff_sna; eff_sna = eff_snb; eff_snb = tn;
    }
    if (eff_sna == eff_snb) {
        lr[eff_sza] = mpn_add(lr, pa, eff_sza, pb, eff_szb);
        szr = eff_sza + (lr[eff_sza] != 0);
        snr = eff_sna;
    } else {
        if (eff_sza > eff_szb || mpn_cmp(pa, pb, eff_sza) >= 0) {
            mpn_sub(lr, pa, eff_sza, pb, eff_szb);
            szr = eff_sza; snr = eff_sna;
        } else {
            mpn_sub(lr, pb, eff_szb, pa, eff_sza);
            szr = eff_szb; snr = eff_snb;
        }
    }
    return zint_normalize(lr, szr, snr);
}

/* -----------------------------------------------------------------------
 * Multiplication
 * ----------------------------------------------------------------------- */

value largeint_mul(value dest_ignored, value a, value b) {
    if (Is_long(a) && Is_long(b)) {
        long x = Long_val(a), y = Long_val(b);
        /* Check if x*y fits: if |x| < 2^31 and |y| < 2^31, it definitely fits */
        long ax = x < 0 ? -x : x, ay = y < 0 ? -y : y;
        if (ax < (1L << 31) && ay < (1L << 31)) return Val_long(x * y);
        /* Else fall through */
    }
    mp_limb_t la[MAX_LIMBS], lb[MAX_LIMBS], lr[MAX_LIMBS * 2];
    int sna, snb;
    mp_size_t sza = zint_decompose(a, la, &sna);
    mp_size_t szb = zint_decompose(b, lb, &snb);
    if (sza == 0 || szb == 0) return Val_long(0);
    mp_size_t szr;
    if (sza >= szb) {
        mpn_mul(lr, la, sza, lb, szb);
    } else {
        mpn_mul(lr, lb, szb, la, sza);
    }
    szr = sza + szb;
    return zint_normalize(lr, szr, sna ^ snb);
}

/* -----------------------------------------------------------------------
 * Division helpers (shared by fdiv/tdiv)
 * ----------------------------------------------------------------------- */

static void do_tdiv(value a, value b, mp_limb_t *qout, mp_size_t *szq,
                    mp_limb_t *rout, mp_size_t *szr,
                    int *snq, int *snr) {
    mp_limb_t la[MAX_LIMBS], lb[MAX_LIMBS];
    int sna, snb;
    mp_size_t sza = zint_decompose(a, la, &sna);
    mp_size_t szb_v = zint_decompose(b, lb, &snb);
    if (szb_v == 0) raiseprimitive0(SYS__EXN_DIV);
    if (sza < szb_v || (sza == szb_v && mpn_cmp(la, lb, sza) < 0)) {
        /* |a| < |b|: quotient=0, remainder=a */
        *szq = 0; *snq = 0;
        memcpy(rout, la, sza * sizeof(mp_limb_t));
        *szr = sza; *snr = sna;
        return;
    }
    mp_size_t qsz = sza - szb_v + 1;
    mp_limb_t scratch[MAX_LIMBS * 2];
    mpn_tdiv_qr(qout, rout, 0, la, sza, lb, szb_v);
    *szq = qsz; *snq = sna ^ snb;
    *szr = szb_v; *snr = sna;
}

/* Truncating division (rounds toward 0) */
value largeint_tdiv(value dest_ignored, value a, value b) {
    mp_limb_t q[MAX_LIMBS], r[MAX_LIMBS];
    mp_size_t szq, szr; int snq, snr;
    do_tdiv(a, b, q, &szq, r, &szr, &snq, &snr);
    return zint_normalize(q, szq, snq);
}

value largeint_tmod(value dest_ignored, value a, value b) {
    mp_limb_t q[MAX_LIMBS], r[MAX_LIMBS];
    mp_size_t szq, szr; int snq, snr;
    do_tdiv(a, b, q, &szq, r, &szr, &snq, &snr);
    return zint_normalize(r, szr, snr);
}

value largeint_tdivmod(value qdest, value rdest, value a, value b) {
    mp_limb_t q[MAX_LIMBS], r[MAX_LIMBS];
    mp_size_t szq, szr; int snq, snr;
    do_tdiv(a, b, q, &szq, r, &szr, &snq, &snr);
    /* Return pair (q, r) -- caller ignores dest args in new API */
    value qv = zint_normalize(q, szq, snq);
    value rv = zint_normalize(r, szr, snr);
    /* Pack as a 2-tuple */
    Push_roots(roots, 2);
    roots[0] = qv; roots[1] = rv;
    value pair = alloc_tuple(2);
    Field(pair, 0) = roots[0];
    Field(pair, 1) = roots[1];
    Pop_roots();
    return pair;
}

/* Floor division (rounds toward -infinity) */
value largeint_fdiv(value dest_ignored, value a, value b) {
    mp_limb_t q[MAX_LIMBS], r[MAX_LIMBS];
    mp_size_t szq, szr; int snq, snr;
    /* Get signs of a and b before dividing (do_tdiv returns sign of quotient) */
    int sna_orig = Is_long(a) ? (Long_val(a) < 0) : (ZINT_SIGN(a) != 0);
    int snb_orig = Is_long(b) ? (Long_val(b) < 0) : (ZINT_SIGN(b) != 0);
    do_tdiv(a, b, q, &szq, r, &szr, &snq, &snr);
    value qv = zint_normalize(q, szq, snq);
    /* Floor adjustment: if remainder != 0 and sign(a) != sign(b), subtract 1 */
    mp_size_t rreal = szr;
    while (rreal > 0 && r[rreal-1] == 0) rreal--;
    if (rreal > 0 && sna_orig != snb_orig) {
        qv = largeint_sub(Val_unit, qv, Val_long(1));
    }
    return qv;
}

value largeint_fmod(value dest_ignored, value a, value b) {
    /* r_floor = a - b * floor(a/b) */
    value qv = largeint_fdiv(Val_unit, a, b);
    value bqv = largeint_mul(Val_unit, b, qv);
    return largeint_sub(Val_unit, a, bqv);
}

value largeint_fdivmod(value qdest, value rdest, value a, value b) {
    Push_roots(roots, 2);
    roots[0] = largeint_fdiv(Val_unit, a, b);
    roots[1] = largeint_fmod(Val_unit, a, b);
    value pair = alloc_tuple(2);
    Field(pair, 0) = roots[0];
    Field(pair, 1) = roots[1];
    Pop_roots();
    return pair;
}

/* -----------------------------------------------------------------------
 * String conversion
 * ----------------------------------------------------------------------- */

value largeint_sizeinbase(value src, value base) {
    if (Is_long(src)) {
        long n = Long_val(src);
        if (n == 0) return Val_long(1);
        if (n < 0) n = -n;
        mp_limb_t lim = (mp_limb_t)n;
        return Val_long((long)mpn_sizeinbase(&lim, 1, Long_val(base)));
    }
    mp_size_t sz = ZINT_SIZE(src);
    if (sz == 0) return Val_long(1);
    return Val_long((long)mpn_sizeinbase(ZINT_LIMBS(src), sz, Long_val(base)));
}

value largeint_get_str(value src, value base_v) {
    long base = Long_val(base_v);
    int negative;
    mp_limb_t limbs[MAX_LIMBS];
    mp_size_t sz = zint_decompose(src, limbs, &negative);

    if (sz == 0) {
        value s = alloc_string(1);
        Byte(s, 0) = '0';
        return s;
    }

    /* mpn_sizeinbase overestimates by at most 1; add 2 for sign and null */
    long maxlen = (long)mpn_sizeinbase(limbs, sz, base) + 2;
    /* raw: raw digit values from mpn_get_str; out: ASCII result */
    unsigned char *raw = (unsigned char*)malloc(maxlen);
    char *out = (char*)malloc(maxlen + 1);  /* +1 for sign */
    if (!raw || !out) { free(raw); free(out); failwith("largeint_get_str: out of memory"); }

    /* mpn_get_str modifies the limb array, so use a copy */
    mp_limb_t tmp[MAX_LIMBS];
    memcpy(tmp, limbs, sz * sizeof(mp_limb_t));

    /* Write raw digit values (0..base-1) into raw[] */
    mp_size_t slen = (mp_size_t)mpn_get_str(raw, base, tmp, sz);

    /* Trim leading zeros */
    mp_size_t start = 0;
    while (start < slen - 1 && raw[start] == 0) start++;
    slen -= start;

    /* Build ASCII output: optional sign, then digits */
    char *p = out;
    if (negative) *p++ = '~';
    for (mp_size_t i = 0; i < slen; i++) {
        unsigned char d = raw[start + i];
        *p++ = (d < 10) ? ('0' + d) : ('a' + d - 10);
    }
    *p = '\0';

    value s = copy_string(out);
    free(raw);
    free(out);
    return s;
}

value largeint_set_str(value dest_ignored, value str_v, value base_v) {
    /* Parse string into a zint, return it */
    const char *str = String_val(str_v);
    long base = Long_val(base_v);
    long len = (long)string_length(str_v);
    if (len == 0) failwith("largeint_set_str: empty string");

    int negative = (str[0] == '-' || str[0] == '~');
    const char *digits = str + (negative ? 1 : 0);
    long dlen = len - (negative ? 1 : 0);
    if (dlen == 0) failwith("largeint_set_str: no digits");

    /* Convert ASCII digits to values */
    unsigned char *ubuf = (unsigned char*)malloc(dlen);
    if (!ubuf) failwith("largeint_set_str: out of memory");
    for (long i = 0; i < dlen; i++) {
        char c = digits[i];
        unsigned char v;
        if (c >= '0' && c <= '9') v = c - '0';
        else if (c >= 'a' && c <= 'f') v = c - 'a' + 10;
        else if (c >= 'A' && c <= 'F') v = c - 'A' + 10;
        else { free(ubuf); failwith("largeint_set_str: invalid digit"); }
        if (v >= base) { free(ubuf); failwith("largeint_set_str: digit out of range"); }
        ubuf[i] = v;
    }

    /* Allocate limb buffer and convert */
    mp_size_t max_sz = (mp_size_t)(dlen * 4 / (sizeof(mp_limb_t) * 2) + 2);
    if (max_sz > MAX_LIMBS) max_sz = MAX_LIMBS;
    mp_limb_t limbs[MAX_LIMBS];
    mp_size_t sz = mpn_set_str(limbs, ubuf, dlen, base);
    free(ubuf);

    return zint_normalize(limbs, sz, negative);
}

/* -----------------------------------------------------------------------
 * Power
 * ----------------------------------------------------------------------- */

value largeint_pow_ui(value dest_ignored, value base_v, value exp_v) {
    long exp = Long_val(exp_v);
    if (exp == 0) return Val_long(1);
    if (exp == 1) return base_v;
    if (Is_long(base_v)) {
        long b = Long_val(base_v);
        if (b == 0) return Val_long(0);
        if (b == 1) return Val_long(1);
        if (b == -1) return Val_long((exp & 1) ? -1 : 1);
    }
    /* Binary exponentiation */
    value result = Val_long(1);
    value base = base_v;
    Push_roots(roots, 2);
    roots[0] = result; roots[1] = base;
    while (exp > 0) {
        if (exp & 1) roots[0] = largeint_mul(Val_unit, roots[0], roots[1]);
        roots[1] = largeint_mul(Val_unit, roots[1], roots[1]);
        exp >>= 1;
    }
    Pop_roots();
    return roots[0];
}

/* -----------------------------------------------------------------------
 * Bitwise operations (two's complement semantics for signed integers)
 * ----------------------------------------------------------------------- */

/* Helper: convert signed magnitude to two's complement limbs */
static mp_size_t to_twos_complement(mp_limb_t *limbs, mp_size_t sz, int negative) {
    if (!negative) return sz;
    /* negate: ~x + 1 = bitwise NOT then increment */
    mpn_com(limbs, limbs, sz);
    mp_limb_t carry = mpn_add_1(limbs, limbs, sz, 1);
    if (carry) { limbs[sz] = carry; sz++; }
    return sz;
}

/* Helper: convert two's complement limbs back to signed magnitude */
static mp_size_t from_twos_complement(mp_limb_t *limbs, mp_size_t sz, int *negative) {
    if (sz == 0 || !(limbs[sz-1] >> (GMP_NUMB_BITS-1))) {
        *negative = 0;
        return sz;
    }
    /* Negative two's complement: negate to get magnitude */
    *negative = 1;
    mpn_com(limbs, limbs, sz);
    mp_limb_t carry = mpn_add_1(limbs, limbs, sz, 1);
    if (carry) { limbs[sz] = carry; sz++; }
    return sz;
}

value largeint_andb(value a, value b) {
    if (Is_long(a) && Is_long(b))
        return Val_long(Long_val(a) & Long_val(b));
    mp_limb_t la[MAX_LIMBS+1], lb[MAX_LIMBS+1], lr[MAX_LIMBS+1];
    int sna, snb;
    mp_size_t sza = zint_decompose(a, la, &sna);
    mp_size_t szb = zint_decompose(b, lb, &snb);
    /* Extend to same size */
    mp_size_t sz = sza > szb ? sza : szb;
    for (mp_size_t i = sza; i <= sz; i++) la[i] = 0;
    for (mp_size_t i = szb; i <= sz; i++) lb[i] = 0;
    sza = to_twos_complement(la, sz+1, sna);
    szb = to_twos_complement(lb, sz+1, snb);
    mp_size_t szr = sza > szb ? sza : szb;
    for (mp_size_t i = sza; i < szr; i++) la[i] = sna ? ~(mp_limb_t)0 : 0;
    for (mp_size_t i = szb; i < szr; i++) lb[i] = snb ? ~(mp_limb_t)0 : 0;
    mpn_and_n(lr, la, lb, szr);
    int snr;
    szr = from_twos_complement(lr, szr, &snr);
    return zint_normalize(lr, szr, snr);
}

value largeint_orb(value a, value b) {
    if (Is_long(a) && Is_long(b))
        return Val_long(Long_val(a) | Long_val(b));
    mp_limb_t la[MAX_LIMBS+1], lb[MAX_LIMBS+1], lr[MAX_LIMBS+1];
    int sna, snb;
    mp_size_t sza = zint_decompose(a, la, &sna);
    mp_size_t szb = zint_decompose(b, lb, &snb);
    mp_size_t sz = sza > szb ? sza : szb;
    for (mp_size_t i = sza; i <= sz; i++) la[i] = 0;
    for (mp_size_t i = szb; i <= sz; i++) lb[i] = 0;
    sza = to_twos_complement(la, sz+1, sna);
    szb = to_twos_complement(lb, sz+1, snb);
    mp_size_t szr = sza > szb ? sza : szb;
    for (mp_size_t i = sza; i < szr; i++) la[i] = sna ? ~(mp_limb_t)0 : 0;
    for (mp_size_t i = szb; i < szr; i++) lb[i] = snb ? ~(mp_limb_t)0 : 0;
    mpn_ior_n(lr, la, lb, szr);
    int snr;
    szr = from_twos_complement(lr, szr, &snr);
    return zint_normalize(lr, szr, snr);
}

value largeint_xorb(value a, value b) {
    if (Is_long(a) && Is_long(b))
        return Val_long(Long_val(a) ^ Long_val(b));
    mp_limb_t la[MAX_LIMBS+1], lb[MAX_LIMBS+1], lr[MAX_LIMBS+1];
    int sna, snb;
    mp_size_t sza = zint_decompose(a, la, &sna);
    mp_size_t szb = zint_decompose(b, lb, &snb);
    mp_size_t sz = sza > szb ? sza : szb;
    for (mp_size_t i = sza; i <= sz; i++) la[i] = 0;
    for (mp_size_t i = szb; i <= sz; i++) lb[i] = 0;
    sza = to_twos_complement(la, sz+1, sna);
    szb = to_twos_complement(lb, sz+1, snb);
    mp_size_t szr = sza > szb ? sza : szb;
    for (mp_size_t i = sza; i < szr; i++) la[i] = sna ? ~(mp_limb_t)0 : 0;
    for (mp_size_t i = szb; i < szr; i++) lb[i] = snb ? ~(mp_limb_t)0 : 0;
    mpn_xor_n(lr, la, lb, szr);
    int snr;
    szr = from_twos_complement(lr, szr, &snr);
    return zint_normalize(lr, szr, snr);
}

value largeint_notb(value a) {
    /* ~a = -(a+1) */
    return largeint_sub(Val_unit, largeint_neg(Val_unit, a), Val_long(1));
}

value largeint_shl(value a, value n_v) {
    long n = Long_val(n_v);
    if (n <= 0) return a;
    if (Is_long(a)) {
        long x = Long_val(a);
        if (x == 0) return Val_long(0);
        /* Check if shift fits in tagged int range */
        if (n < 62) {
            long result = x << n;
            if ((result >> n) == x && result >= Min_long && result <= Max_long)
                return Val_long(result);
        }
    }
    mp_limb_t la[MAX_LIMBS];
    int sna;
    mp_size_t sza = zint_decompose(a, la, &sna);
    if (sza == 0) return Val_long(0);
    long full_words = n / GMP_NUMB_BITS;
    int bit_shift = n % GMP_NUMB_BITS;
    mp_size_t szr = sza + full_words + 1;
    mp_limb_t lr[MAX_LIMBS * 2];
    for (long i = 0; i < full_words; i++) lr[i] = 0;
    if (bit_shift > 0) {
        lr[sza + full_words] = mpn_lshift(lr + full_words, la, sza, bit_shift);
    } else {
        memcpy(lr + full_words, la, sza * sizeof(mp_limb_t));
        lr[szr - 1] = 0;
    }
    return zint_normalize(lr, szr, sna);
}

value largeint_shr(value a, value n_v) {
    long n = Long_val(n_v);
    if (n <= 0) return a;
    if (Is_long(a)) {
        long x = Long_val(a);
        if (n >= 63) return Val_long(0);
        /* Logical right shift (unsigned) */
        unsigned long ux = (unsigned long)x;
        return Val_long((long)(ux >> n));
    }
    mp_limb_t la[MAX_LIMBS];
    int sna;
    mp_size_t sza = zint_decompose(a, la, &sna);
    long full_words = n / GMP_NUMB_BITS;
    if (full_words >= sza) return Val_long(0);
    int bit_shift = n % GMP_NUMB_BITS;
    mp_size_t szr = sza - full_words;
    mp_limb_t lr[MAX_LIMBS];
    if (bit_shift > 0) {
        mpn_rshift(lr, la + full_words, szr, bit_shift);
    } else {
        memcpy(lr, la + full_words, szr * sizeof(mp_limb_t));
    }
    return zint_normalize(lr, szr, sna);
}

value largeint_ashr(value a, value n_v) {
    /* Arithmetic right shift: sign-extending */
    long n = Long_val(n_v);
    if (n <= 0) return a;
    if (Is_long(a)) {
        long x = Long_val(a);
        if (n >= 62) return Val_long(x < 0 ? -1 : 0);
        return Val_long(x >> n);
    }
    mp_limb_t la[MAX_LIMBS];
    int sna;
    mp_size_t sza = zint_decompose(a, la, &sna);
    long full_words = n / GMP_NUMB_BITS;
    if (full_words >= sza) return Val_long(sna ? -1 : 0);
    int bit_shift = n % GMP_NUMB_BITS;
    mp_size_t szr = sza - full_words;
    mp_limb_t lr[MAX_LIMBS];
    if (bit_shift > 0) {
        mpn_rshift(lr, la + full_words, szr, bit_shift);
    } else {
        memcpy(lr, la + full_words, szr * sizeof(mp_limb_t));
    }
    return zint_normalize(lr, szr, sna);
}
