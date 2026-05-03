(* Int32 -- 32-bit signed integers, 2026-05-03 *)

type int = int;

val precision = SOME 32;

val maxInt32 =  2147483647;
val minInt32 = ~2147483648;

val minInt = SOME minInt32;
val maxInt = SOME maxInt32;

fun checkOvf (x : int) =
    if x < minInt32 orelse x > maxInt32 then raise Overflow
    else x;

fun fromInt (x : Int.int) : int = checkOvf x;
fun toInt   (x : int) : Int.int = x;
fun fromLarge x = fromInt x;
fun toLarge   x = toInt x;

fun compare (x, y : int) =
    if x < y then LESS else if x > y then GREATER else EQUAL;
fun min (x, y) = if x < y then x else y : int;
fun max (x, y) = if x < y then y else x : int;
fun sign (x : int) = if x > 0 then 1 else if x < 0 then ~1 else 0;
fun sameSign (x, y) = sign x = sign y;

local
    open StringCvt
    fun hexval c =
	if #"0" <= c andalso c <= #"9" then Char.ord c - 48
	else (Char.ord c - 55) mod 32;
    fun prhex i = if i < 10 then Char.chr(i + 48) else Char.chr(i + 55)
    fun skipWSget getc source = getc (dropl Char.isSpace getc source)

    fun conv radix i =
	if i = minInt32 then
	    (case radix of
		 2  => "~10000000000000000000000000000000"
	       | 8  => "~20000000000"
	       | 10 => "~2147483648"
	       | 16 => "~80000000"
	       | _  => raise Fail "Int32.fmt")
	else
	    let fun h 0 res = res
		  | h n res = h (n div radix) (prhex (n mod radix) :: res)
		fun tostr n = h (n div radix) [prhex (n mod radix)]
	    in
		if i < 0 then
		    String.implode (#"~" :: tostr (~i))
		else
		    String.implode (tostr i)
	    end
in
    fun scan radix getc source =
	let open StringCvt
	    val (isDigit, factor) =
		case radix of
		    BIN => (fn c => (#"0" <= c andalso c <= #"1"),  2)
		  | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  8)
		  | DEC => (Char.isDigit,                          10)
		  | HEX => (Char.isHexDigit,                       16)
	    fun dig1 sgn NONE = NONE
	      | dig1 sgn (SOME (c, rest)) =
		let val next_val =
			if sgn = 1 then fn (res, hv) => factor * res + hv
			else            fn (res, hv) => factor * res - hv
		    fun digr res src =
			case getc src of
			    NONE           => SOME (checkOvf res, src)
			  | SOME (c, rest) =>
				if isDigit c then
				    digr (next_val(res, hexval c)) rest
				else
				    SOME (checkOvf res, src)
		in if isDigit c then digr (sgn * hexval c) rest else NONE end
	    fun getdigs sgn after0 inp =
		case dig1 sgn inp of
		    NONE => SOME(0, after0)
		  | res  => res
	    fun hexopt sgn NONE                 = NONE
	      | hexopt sgn (SOME(#"0", after0)) =
		if radix <> HEX then getdigs sgn after0 (getc after0)
		else
		    (case getc after0 of
			 NONE             => SOME(0, after0)
		       | SOME(#"x", rest) => getdigs sgn after0 (getc rest)
		       | SOME(#"X", rest) => getdigs sgn after0 (getc rest)
		       | inp              => getdigs sgn after0 inp)
	      | hexopt sgn inp = dig1 sgn inp
	    fun sign NONE                = NONE
	      | sign (SOME (#"~", rest)) = hexopt ~1 (getc rest)
	      | sign (SOME (#"-", rest)) = hexopt ~1 (getc rest)
	      | sign (SOME (#"+", rest)) = hexopt  1 (getc rest)
	      | sign inp                 = hexopt  1 inp
	in sign (skipWSget getc source) end;

    fun fmt BIN = conv 2
      | fmt OCT = conv 8
      | fmt DEC = conv 10
      | fmt HEX = conv 16;

    fun toString (i : int) : string = conv 10 i;

    val fromString = scanString (scan DEC)
end;

val op ~    : int -> int        = fn x => checkOvf (Int.~ x);
val abs     : int -> int        = fn x => checkOvf (Int.abs x);

val op +    : int * int -> int  = fn (x, y) => checkOvf (Int.+ (x, y));
val op -    : int * int -> int  = fn (x, y) => checkOvf (Int.- (x, y));
val op *    : int * int -> int  = fn (x, y) => checkOvf (Int.* (x, y));

fun op div (x, y) = checkOvf (Int.div (x, y));
fun op mod (x, y) = Int.mod (x, y);

local
    prim_val quot_ : int -> int -> int = 2 "quot";
    prim_val rem_  : int -> int -> int = 2 "rem"
in
    fun quot (x, y) = checkOvf (quot_ x y)
    fun rem  (x, y) = rem_ x y
end;

val op >    : int * int -> bool = op >;
val op >=   : int * int -> bool = op >=;
val op <    : int * int -> bool = op <;
val op <=   : int * int -> bool = op <=;
