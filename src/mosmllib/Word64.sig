(* Word64 -- SML Basis Library *)

type word = int * int

val wordSize   : int

val orb        : word * word -> word
val andb       : word * word -> word
val xorb       : word * word -> word
val notb       : word -> word
val ~          : word -> word

val <<         : word * Word.word -> word
val >>         : word * Word.word -> word
val ~>>        : word * Word.word -> word

val +          : word * word -> word
val -          : word * word -> word
val *          : word * word -> word
val div        : word * word -> word
val mod        : word * word -> word

val >          : word * word -> bool
val <          : word * word -> bool
val >=         : word * word -> bool
val <=         : word * word -> bool
val compare    : word * word -> order

val min        : word * word -> word
val max        : word * word -> word

val toString   : word -> string
val fromString : string -> word option
val scan       : StringCvt.radix
               -> (char, 'a) StringCvt.reader -> (word, 'a) StringCvt.reader
val fmt        : StringCvt.radix -> word -> string

val toInt      : word -> int
val toIntX     : word -> int            (* with sign extension *)
val fromInt    : int -> word

val toLargeInt    : word -> intinf
val toLargeIntX   : word -> intinf      (* with sign extension *)
val fromLargeInt  : intinf -> word

val toLarge   : word -> word
val toLargeX  : word -> word
val fromLarge : word -> word

val toLargeWord   : word -> word
val toLargeWordX  : word -> word
val fromLargeWord : word -> word

(*
   [word] is the type of 64-bit unsigned words, represented internally
   as a pair (hi, lo) of native ints holding the upper and lower 32 bits.

   [wordSize] equals 64.

   [orb(w1, w2)] returns the bitwise `or' of w1 and w2.

   [andb(w1, w2)] returns the bitwise `and' of w1 and w2.

   [xorb(w1, w2)] returns the bitwise `exclusive or' or w1 and w2.

   [notb w] returns the bitwise negation (one's complement) of w.

   [~ w] returns the two's complement negation of w, modulo 2^64.

   [<<(w, k)] shifts w left by k bit positions, filling with zeros.
   Result is 0 when k >= 64.

   [>>(w, k)] shifts w right by k bit positions, filling with zeros.
   Result is 0 when k >= 64.

   [~>>(w, k)] arithmetic right shift, replicating the sign bit.

   [+] [-] [*] [div] [mod] unsigned modular arithmetic, modulo 2^64.
   div and mod raise Div when the divisor is 0.

   [<] [<=] [>] [>=] unsigned comparison.

   [compare(w1, w2)] unsigned comparison returning order.

   [toInt w] returns the integer value of w. Raises Overflow if
   w >= 2^62 (does not fit in 63-bit signed native int).

   [toIntX w] treats w as a signed 64-bit value and returns the
   corresponding integer. Raises Overflow if the signed value does
   not fit in 63-bit native int.

   [fromInt i] returns the word holding the 64 least significant bits
   of the two's complement representation of i.

   [toString w] returns hex representation. Equivalent to (fmt HEX w).

   [fromString s] scans a hex word from a prefix of s.

   [fmt radix w] formats w in the given radix.

   [scan radix getc src] scans an unsigned word numeral.
*)
