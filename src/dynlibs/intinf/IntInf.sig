(* LargeInt -- arbitrary-precision integers backed by GMP
 * Uses Zarith-style dual representation:
 *   small ints as tagged values (no allocation), large ints as heap blocks.
 *)

eqtype int

val precision : int option      (* NONE -- no bound *)
val minInt    : int option      (* NONE *)
val maxInt    : int option      (* NONE *)

val ~    : int -> int
val +    : int * int -> int
val -    : int * int -> int
val *    : int * int -> int
val div  : int * int -> int
val mod  : int * int -> int
val quot : int * int -> int
val rem  : int * int -> int
val <    : int * int -> bool
val >    : int * int -> bool
val <=   : int * int -> bool
val >=   : int * int -> bool
val eq   : int * int -> bool
val ne   : int * int -> bool
val abs  : int -> int
val min  : int * int -> int
val max  : int * int -> int

val divMod  : int * int -> int * int
val quotRem : int * int -> int * int
val pow     : int * Int.int -> int
val log2    : int -> Int.int

val sign     : int -> Int.int
val sameSign : int * int -> bool
val compare  : int * int -> order

val fromInt    : Int.int -> int
val toInt      : int -> Int.int    (* raises Overflow if too large *)
val toLarge    : int -> int
val fromLarge  : int -> int

val fromString : string -> int option
val toString   : int -> string

val scan : StringCvt.radix
           -> (char, 'a) StringCvt.reader -> (int, 'a) StringCvt.reader
val fmt  : StringCvt.radix -> int -> string

(* Bitwise operations (two's complement semantics for signed integers) *)
val andb : int * int -> int
val orb  : int * int -> int
val xorb : int * int -> int
val notb : int -> int
val <<   : int * Int.int -> int    (* logical left shift *)
val >>   : int * Int.int -> int    (* logical right shift (unsigned) *)
val ~>>  : int * Int.int -> int    (* arithmetic right shift (sign-extending) *)
