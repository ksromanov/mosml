(* IntInf -- arbitrary-precision integers, backed by GMP.
   Uses a Zarith-style dual representation:
   - Small integers are returned as Val_long (tagged, zero allocation)
   - Large integers are Final_tag blocks with limbs on the mosml heap
   1995-09-04 sestoft -- original GMP interface
   2026-05-14 -- rewritten with mpn_* and small-int fast path *)

exception Domain

prim_eqtype int;
type largeint = int;

local
    open Dynlib
    val dlh = dlopen { lib = "libmgmp.so",
		       flag = RTLD_LAZY,
		       global = false }
in

(* Arithmetic: functional (a, b) -> result style *)
val largeint_make    : unit -> largeint               = app1 (dlsym dlh "largeint_make")
val largeint_make_si : Int.int -> largeint            = app1 (dlsym dlh "largeint_make_si")
val largeint_clear   : largeint -> unit               = app1 (dlsym dlh "largeint_clear")
val largeint_neg     : largeint -> largeint -> largeint
                                                      = app2 (dlsym dlh "largeint_neg")
val largeint_add     : largeint -> largeint -> largeint -> largeint
                                                      = app3 (dlsym dlh "largeint_add")
val largeint_sub     : largeint -> largeint -> largeint -> largeint
                                                      = app3 (dlsym dlh "largeint_sub")
val largeint_mul     : largeint -> largeint -> largeint -> largeint
                                                      = app3 (dlsym dlh "largeint_mul")
val largeint_tdiv    : largeint -> largeint -> largeint -> largeint
                                                      = app3 (dlsym dlh "largeint_tdiv")
val largeint_tmod    : largeint -> largeint -> largeint -> largeint
                                                      = app3 (dlsym dlh "largeint_tmod")
val largeint_fdiv    : largeint -> largeint -> largeint -> largeint
                                                      = app3 (dlsym dlh "largeint_fdiv")
val largeint_fmod    : largeint -> largeint -> largeint -> largeint
                                                      = app3 (dlsym dlh "largeint_fmod")
val largeint_fdivmod : largeint -> largeint -> largeint -> largeint -> largeint * largeint
                                                      = app4 (dlsym dlh "largeint_fdivmod")
val largeint_tdivmod : largeint -> largeint -> largeint -> largeint -> largeint * largeint
                                                      = app4 (dlsym dlh "largeint_tdivmod")
val largeint_cmp     : largeint -> largeint -> Int.int = app2 (dlsym dlh "largeint_cmp")
val largeint_cmp_si  : largeint -> Int.int -> Int.int  = app2 (dlsym dlh "largeint_cmp_si")
val largeint_sizeinbase : largeint -> Int.int -> Int.int
                                                      = app2 (dlsym dlh "largeint_sizeinbase")
val largeint_get_str : largeint -> Int.int -> string  = app2 (dlsym dlh "largeint_get_str")
val largeint_set_str : largeint -> string -> Int.int -> largeint
                                                      = app3 (dlsym dlh "largeint_set_str")
val largeint_pow_ui  : largeint -> largeint -> Int.int -> largeint
                                                      = app3 (dlsym dlh "largeint_pow_ui")
val largeint_to_si   : largeint -> Int.int            = app1 (dlsym dlh "largeint_to_si")
val largeint_andb    : largeint -> largeint -> largeint = app2 (dlsym dlh "largeint_andb")
val largeint_orb     : largeint -> largeint -> largeint = app2 (dlsym dlh "largeint_orb")
val largeint_xorb    : largeint -> largeint -> largeint = app2 (dlsym dlh "largeint_xorb")
val largeint_notb    : largeint -> largeint            = app1 (dlsym dlh "largeint_notb")
val largeint_shl     : largeint -> Int.int -> largeint = app2 (dlsym dlh "largeint_shl")
val largeint_shr     : largeint -> Int.int -> largeint = app2 (dlsym dlh "largeint_shr")
val largeint_ashr    : largeint -> Int.int -> largeint = app2 (dlsym dlh "largeint_ashr")
val largeint_register_equal : unit -> unit            = app1 (dlsym dlh "largeint_register_equal")

val _ = largeint_register_equal ()

end

val precision = NONE
val minInt    = NONE : int option
val maxInt    = NONE : int option

local
    val zero = largeint_make_si 0

    fun isZero li = largeint_cmp_si li 0 = 0

in

fun fromInt  i = largeint_make_si i
fun fromLarge x = x
fun toLarge x = x

fun toInt li =
    case (Int.minInt, Int.maxInt) of
	(SOME lo, SOME hi) =>
	    if largeint_cmp_si li lo <> ~1
	       andalso largeint_cmp_si li hi <> 1
	    then largeint_to_si li
	    else raise Overflow
      | _ => raise Fail "IntInf.toInt: internal error"

fun sign li = largeint_cmp_si li 0

fun sameSign (li1, li2) = sign li1 = sign li2

fun compare (li1, li2) =
    case largeint_cmp li1 li2 of
	~1 => LESS
      |  0 => EQUAL
      |  1 => GREATER
      |  _ => raise Fail "IntInf.compare: internal error"

fun op < (a, b)  = Int.< (largeint_cmp a b, 0)
fun op <= (a, b) = Int.<= (largeint_cmp a b, 0)
fun op > (a, b)  = Int.> (largeint_cmp a b, 0)
fun op >= (a, b) = Int.>= (largeint_cmp a b, 0)

fun eq (a, b) = largeint_cmp a b = 0
fun ne (a, b) = largeint_cmp a b <> 0

fun min (a, b) = if a < b then a else b
fun max (a, b) = if a < b then b else a

fun ~ li     = largeint_neg  zero li
fun abs li   = if Int.< (sign li, 0) then ~ li else li

fun op + (a, b) = largeint_add zero a b
fun op - (a, b) = largeint_sub zero a b
fun op * (a, b) = largeint_mul zero a b

fun op div (a, b) =
    if isZero b then raise Div
    else largeint_fdiv zero a b

fun op mod (a, b) =
    if isZero b then raise Div
    else largeint_fmod zero a b

fun quot (a, b) =
    if isZero b then raise Div
    else largeint_tdiv zero a b

fun rem (a, b) =
    if isZero b then raise Div
    else largeint_tmod zero a b

fun divMod (a, b) =
    if isZero b then raise Div
    else largeint_fdivmod zero zero a b

fun quotRem (a, b) =
    if isZero b then raise Div
    else largeint_tdivmod zero zero a b

fun log2 li =
    if Int.<= (sign li, 0) then raise Domain
    else Int.- (largeint_sizeinbase li 2, 1)

fun pow (li, exp) =
    if Int.< (exp, 0) then
	if isZero li                         then raise Div
	else if largeint_cmp_si li 1  = 0   then fromInt 1
	else if largeint_cmp_si li ~1 = 0   then fromInt (if Int.mod (exp, 2) = 0 then 1 else ~1)
	else fromInt 0
    else if exp = 0 then fromInt 1
    else largeint_pow_ui zero li exp

fun fmt radix li =
    let open StringCvt
    in case radix of
	   BIN => largeint_get_str li  2
	 | OCT => largeint_get_str li  8
	 | DEC => largeint_get_str li 10
	 | HEX => largeint_get_str li 16
    end

fun toString li = largeint_get_str li 10

local
    open StringCvt
    fun skipWSget getc source = getc (skipWS getc source)

    fun makelarge digits (base : Int.int) src =
	(SOME (largeint_set_str zero (String.implode (List.rev digits)) base, src))
	handle Fail _ => NONE

    fun dig1 getc digits (base : Int.int) isDigit NONE = NONE
      | dig1 getc digits base isDigit (SOME (c, rest)) =
	let fun digr digits src =
		case getc src of
		    NONE           => makelarge digits base src
		  | SOME (c, rest) =>
			if isDigit c then digr (c :: digits) rest
			else makelarge digits base src
	in if isDigit c then digr (c :: digits) rest
	   else NONE
	end

    fun sign_ getc base isDigit NONE = NONE
      | sign_ getc base isDigit (SOME (#"~", rest)) =
	    dig1 getc [#"-"] base isDigit (getc rest)
      | sign_ getc base isDigit (SOME (#"-", rest)) =
	    dig1 getc [#"-"] base isDigit (getc rest)
      | sign_ getc base isDigit (SOME (#"+", rest)) =
	    dig1 getc [] base isDigit (getc rest)
      | sign_ getc base isDigit inp =
	    dig1 getc [] base isDigit inp

in
    fun scan radix getc source =
	let open StringCvt
	    val (base, isDigit) =
		case radix of
		    BIN => (2,  fn c => c = #"0" orelse c = #"1")
		  | OCT => (8,  fn c => Char.>= (c, #"0") andalso Char.<= (c, #"7"))
		  | DEC => (10, Char.isDigit)
		  | HEX => (16, Char.isHexDigit)
	in sign_ getc base isDigit (skipWSget getc source)
	end

    val fromString = scanString (scan DEC)
end

(* Bitwise operations use two's complement semantics for negative integers *)
fun andb (a, b) = largeint_andb a b
fun orb  (a, b) = largeint_orb  a b
fun xorb (a, b) = largeint_xorb a b
fun notb a      = largeint_notb a
fun << (a, n)   = largeint_shl  a n
fun >> (a, n)   = largeint_shr  a n
fun ~>> (a, n)  = largeint_ashr a n

end
