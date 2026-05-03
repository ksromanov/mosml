(* Int64 -- 64-bit signed integers, 2026-05-03 *)

type int = Int.int * Int.int;

val precision = SOME 64;

local
    val base  = 4294967296    (* 2^32 *)
    val maxLo = 4294967295    (* 2^32 - 1 *)
    val hiMin = ~2147483648   (* ~2^31 *)
    val hiMax =  2147483647   (* 2^31 - 1 *)
    val half  = 65536         (* 2^16 *)
    val hbit  = 2147483648    (* 2^31 *)

    val maxInt64 = (hiMax, maxLo)
    val minInt64 = (hiMin, 0)
    val zero     = (0, 0) : int
    val one      = (0, 1) : int

    fun checkOvf (hi, lo) =
	if hi < hiMin orelse hi > hiMax then raise Overflow
	else (hi, lo) : int;

    fun isNeg (hi, _ : Int.int) = hi < 0;
    fun isZero (0, 0) = true | isZero (_ : int) = false;

    fun negat (hi, lo) : int =
	if lo = 0 then (Int.~ hi, 0)
	else (Int.~ hi - 1, base - lo);

    (* Unsigned magnitude comparison *)
    fun ucmp ((h1, l1) : int, (h2, l2) : int) =
	let val u1 = if h1 >= 0 then h1 else h1 + base
	    val u2 = if h2 >= 0 then h2 else h2 + base
	in if u1 < u2 then LESS
	   else if u1 > u2 then GREATER
	   else if l1 < l2 then LESS
	   else if l1 > l2 then GREATER
	   else EQUAL
	end;

    fun uge (a, b) = ucmp(a, b) <> LESS;

    (* Shift left by 1 bit (unsigned) *)
    fun shl1 (hi, lo) : int =
	(hi * 2 + lo div hbit, (lo mod hbit) * 2);

    (* Shift right by 1 bit (unsigned) *)
    fun shr1 (hi, lo) : int =
	(hi div 2, lo div 2 + (hi mod 2) * hbit);

    fun uadd ((h1,l1) : int, (h2,l2) : int) : int =
	let val lo = l1 + l2
	in (h1 + h2 + lo div base, lo mod base) end;

    fun usub ((h1,l1) : int, (h2,l2) : int) : int =
	let val lo = l1 - l2
	in if lo < 0 then (h1 - h2 - 1, lo + base)
	   else (h1 - h2, lo) end;

    (* Unsigned long division *)
    fun udivmod (n : int, d : int) =
	if isZero d then raise Div
	else if ucmp(n, d) = LESS then (zero, n)
	else
	    let fun countBits acc d' =
		    let val d'' = shl1 d'
		    in if isZero d'' orelse ucmp(d'', n) = GREATER
		       then acc
		       else countBits (acc + 1) d''
		    end
		val bits = countBits 1 d
		fun shiftN d' 0 = d' | shiftN d' n = shiftN (shl1 d') (n-1)
		val ds = shiftN d (bits - 1)
		fun loop _ q r 0 = (q, r)
		  | loop d' q r b =
		    if uge(r, d') then
			loop (shr1 d') (uadd(shl1 q, one)) (usub(r, d')) (b-1)
		    else
			loop (shr1 d') (shl1 q) r (b-1)
	    in loop ds zero n bits end;

in

val maxInt = SOME maxInt64;
val minInt = SOME minInt64;

(* Conversions *)

fun fromInt (x : Int.int) : int =
    if x >= 0 then (x div base, x mod base)
    else let val x' = x + base
	 in (~1 + (x' div base), x' mod base) end;

fun toInt ((hi, lo) : int) : Int.int =
    let val v = hi * base + lo
    in if (hi >= 0 andalso v >= 0) orelse (hi < 0 andalso v < 0)
       then v
       else raise Overflow
    end;

fun fromLarge x = fromInt x;
fun toLarge x   = toInt x;

(* Comparison *)

fun compare ((h1, l1) : int, (h2, l2) : int) =
    if h1 < h2 then LESS
    else if h1 > h2 then GREATER
    else if l1 < l2 then LESS
    else if l1 > l2 then GREATER
    else EQUAL;

fun sign ((hi, lo) : int) : Int.int =
    if hi > 0 orelse (hi = 0 andalso lo > 0) then 1
    else if hi = 0 andalso lo = 0 then 0
    else ~1;

fun sameSign (a, b) = sign a = sign b;

(* Arithmetic *)

fun add ((h1, l1) : int, (h2, l2) : int) =
    let val lo = l1 + l2
	val carry = lo div base
	val lo' = lo mod base
	val hi = h1 + h2 + carry
    in checkOvf (hi, lo') end;

fun sub ((h1, l1) : int, (h2, l2) : int) =
    let val lo = l1 - l2
	val (hi, lo') = if lo < 0 then (h1 - h2 - 1, lo + base)
			else (h1 - h2, lo)
    in checkOvf (hi, lo') end;

fun neg (hi, lo) : int =
    if lo = 0 then checkOvf (Int.~ hi, 0)
    else checkOvf (Int.~ hi - 1, base - lo);

fun absv x = if isNeg x then neg x else x;

fun mul (a : int, b : int) =
    if isZero a orelse isZero b then zero
    else
    let val aneg = isNeg a
	val bneg = isNeg b
	(* Get unsigned magnitudes. negat minInt64 produces (hiMax+1, 0)
	   which is out of normalized range but fine for unsigned math. *)
	val (ah, al) = if aneg then negat a else a
	val (bh, bl) = if bneg then negat b else b
	(* Unsigned multiply: (ah*base+al) * (bh*base+bl)
	   ah*bh must be 0 or overflow; ah*bl+bh*al goes to hi *)
	val _ = if ah > 0 andalso bh > 0 then raise Overflow else ()
	val cross = ah * bl + bh * al
	(* al*bl: split into 16-bit halves to avoid 63-bit overflow *)
	val alh = al div half
	val all = al mod half
	val blh = bl div half
	val bll = bl mod half
	val ll = all * bll
	val lm = alh * bll + all * blh
	val lh = alh * blh
	val rlo = ll + (lm mod half) * half
	val rhi = lh + lm div half + rlo div base + cross
	val rlo' = rlo mod base
	(* Check result fits. For negative results, magnitude can be hiMax+1
	   (i.e. minInt64's magnitude), so allow rhi = hiMax+1 when negating *)
    in if aneg <> bneg then
	   if rhi = hiMax + 1 andalso rlo' = 0 then minInt64
	   else if rhi > hiMax then raise Overflow
	   else negat (rhi, rlo')
       else checkOvf (rhi, rlo')
    end;

(* Division *)

fun quot (a, b) =
    if isZero b then raise Div
    else if isZero a then zero
    else if compare(a, minInt64) = EQUAL andalso compare(b, (~1, maxLo)) = EQUAL
    then raise Overflow
    else let val aneg = isNeg a
	     val bneg = isNeg b
	     val am = if aneg then negat a else a
	     val bm = if bneg then negat b else b
	     val (q, _) = udivmod (am, bm)
	 in if aneg <> bneg then checkOvf (negat q) else q end;

fun rem (a, b) =
    if isZero b then raise Div
    else if isZero a then zero
    else let val aneg = isNeg a
	     val am = if aneg then negat a else a
	     val bm = if isNeg b then negat b else b
	     val (_, r) = udivmod (am, bm)
	 in if aneg then negat r else r end;

fun divop (a, b) =
    if isZero b then raise Div
    else if isZero a then zero
    else if compare(a, minInt64) = EQUAL andalso compare(b, (~1, maxLo)) = EQUAL
    then raise Overflow
    else let val aneg = isNeg a
	     val bneg = isNeg b
	     val am = if aneg then negat a else a
	     val bm = if bneg then negat b else b
	     val (q, r) = udivmod (am, bm)
	 in if aneg = bneg then q
	    else if isZero r then checkOvf (negat q)
	    else checkOvf (negat (uadd (q, one)))
	 end;

fun modop (a, b) =
    if isZero b then raise Div
    else if isZero a then zero
    else let val aneg = isNeg a
	     val bneg = isNeg b
	     val am = if aneg then negat a else a
	     val bm = if bneg then negat b else b
	     val (_, r) = udivmod (am, bm)
	 in if isZero r then zero
	    else if aneg = bneg then
		(if aneg then negat r else r)
	    else
		(if bneg then negat (usub (bm, r)) else usub (bm, r))
	 end;

(* String conversion — must come before operator redefinitions *)

local
    open StringCvt
    fun hexval c =
	if #"0" <= c andalso c <= #"9" then Char.ord c - 48
	else (Char.ord c - 55) mod 32;
    fun prhex i = if i < 10 then Char.chr(i + 48) else Char.chr(i + 55)
    fun skipWSget getc source = getc (dropl Char.isSpace getc source)

    val i2  = fromInt 2
    val i8  = fromInt 8
    val i10 = fromInt 10
    val i16 = fromInt 16

    fun conv rad radix (i : int) =
	if compare(i, minInt64) = EQUAL then
	    (case rad of
		 BIN => "~1000000000000000000000000000000000000000000000000000000000000000"
	       | OCT => "~1000000000000000000000"
	       | DEC => "~9223372036854775808"
	       | HEX => "~8000000000000000")
	else
	    let val r = case rad of BIN => i2 | OCT => i8
				  | DEC => i10 | HEX => i16
		fun digit n = prhex (toInt (rem (n, r)))
		fun h n res =
		    if isZero n then res
		    else h (quot (n, r)) (digit n :: res)
		fun tostr n = h (quot(n, r)) [digit n]
	    in
		if isNeg i then
		    String.implode (#"~" :: tostr (neg i))
		else
		    String.implode (tostr i)
	    end
in
    fun scan radix getc source =
	let open StringCvt
	    val (isDigit, factor) =
		case radix of
		    BIN => (fn c => (#"0" <= c andalso c <= #"1"),  i2)
		  | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  i8)
		  | DEC => (Char.isDigit,                           i10)
		  | HEX => (Char.isHexDigit,                        i16)
	    fun dig1 sgn NONE = NONE
	      | dig1 sgn (SOME (c, rest)) =
		let val next_val =
			if sgn = 1 then fn (res, hv) => add(mul(res, factor), fromInt hv)
			else            fn (res, hv) => sub(mul(res, factor), fromInt hv)
		    fun digr res src =
			case getc src of
			    NONE           => SOME (res, src)
			  | SOME (c, rest) =>
				if isDigit c then
				    digr (next_val(res, hexval c)) rest
				else
				    SOME (res, src)
		in if isDigit c then
		       digr (fromInt (sgn * hexval c)) rest
		   else NONE
		end
	    fun getdigs sgn after0 inp =
		case dig1 sgn inp of
		    NONE => SOME(zero, after0)
		  | res  => res
	    fun hexopt sgn NONE                 = NONE
	      | hexopt sgn (SOME(#"0", after0)) =
		if radix <> HEX then getdigs sgn after0 (getc after0)
		else
		    (case getc after0 of
			 NONE             => SOME(zero, after0)
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

    fun fmt BIN = conv BIN 2
      | fmt OCT = conv OCT 8
      | fmt DEC = conv DEC 10
      | fmt HEX = conv HEX 16;

    fun toString (i : int) : string = conv DEC 10 i;

    val fromString = scanString (scan DEC)
end;

val op ~    : int -> int        = neg;
val abs     : int -> int        = absv;

val op +    : int * int -> int  = add;
val op -    : int * int -> int  = sub;
val op *    : int * int -> int  = mul;

val op div  : int * int -> int  = divop;
val op mod  : int * int -> int  = modop;

fun min (a, b : int) = if compare(a, b) = LESS then a else b;
fun max (a, b : int) = if compare(a, b) = GREATER then a else b;

val op >    : int * int -> bool = fn (a, b) => compare(a, b) = GREATER;
val op >=   : int * int -> bool = fn (a, b) => compare(a, b) <> LESS;
val op <    : int * int -> bool = fn (a, b) => compare(a, b) = LESS;
val op <=   : int * int -> bool = fn (a, b) => compare(a, b) <> GREATER;

end;
