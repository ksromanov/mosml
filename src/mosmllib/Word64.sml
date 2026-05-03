(* Word64 -- 64-bit unsigned words, 2026-05-03 *)

type word = int * int;

val wordSize = 64;

local
    val base  = 4294967296    (* 2^32 *)
    val maxLo = 4294967295    (* 0xFFFFFFFF *)
    val half  = 65536         (* 2^16 *)
    val hbit  = 2147483648    (* 2^31 *)

    val zero    = (0, 0) : word;
    val one     = (0, 1) : word;
    val allOnes = (maxLo, maxLo) : word;

    fun isZero (0, 0) = true | isZero (_ : word) = false;

    fun norm (hi, lo) : word =
	let val lo' = lo mod base
	    val c   = lo div base
	    val lo'' = if lo' < 0 then lo' + base else lo'
	    val c'   = if lo' < 0 then c - 1 else c
	    val hi'  = (hi + c') mod base
	    val hi'' = if hi' < 0 then hi' + base else hi'
	in (hi'', lo'') end;

    fun ucmp ((h1, l1) : word, (h2, l2) : word) =
	if h1 < h2 then LESS
	else if h1 > h2 then GREATER
	else if l1 < l2 then LESS
	else if l1 > l2 then GREATER
	else EQUAL;

    fun uge (a, b) = ucmp(a, b) <> LESS;

    fun shl1 (hi, lo) : word =
	let val hi' = (hi * 2 + lo div hbit) mod base
	    val hi'' = if hi' < 0 then hi' + base else hi'
	in (hi'', (lo mod hbit) * 2) end;

    fun shr1 (hi, lo) : word =
	(hi div 2, lo div 2 + (hi mod 2) * hbit);

    fun uadd ((h1,l1) : word, (h2,l2) : word) : word =
	let val lo = l1 + l2
	    val carry = lo div base
	    val lo' = lo mod base
	    val hi = (h1 + h2 + carry) mod base
	    val hi' = if hi < 0 then hi + base else hi
	in (hi', lo') end;

    fun usub ((h1,l1) : word, (h2,l2) : word) : word =
	let val lo = l1 - l2
	in if lo < 0 then
	       let val hi = (h1 - h2 - 1) mod base
		   val hi' = if hi < 0 then hi + base else hi
	       in (hi', lo + base) end
	   else
	       let val hi = (h1 - h2) mod base
		   val hi' = if hi < 0 then hi + base else hi
	       in (hi', lo) end
	end;

    fun umul ((h1, l1) : word, (h2, l2) : word) : word =
	let val l1h = l1 div half
	    val l1l = l1 mod half
	    val l2h = l2 div half
	    val l2l = l2 mod half
	    val ll = l1l * l2l
	    val lm = l1h * l2l + l1l * l2h
	    val lh = l1h * l2h
	    val rlo = ll + (lm mod half) * half
	    val rhi = lh + lm div half + rlo div base + h1 * l2 + h2 * l1
	    val rlo' = rlo mod base
	    val rhi' = rhi mod base
	    val rhi'' = if rhi' < 0 then rhi' + base else rhi'
	in (rhi'', rlo') end;

    fun udivmod (n : word, d : word) =
	if isZero d then raise Div
	else if ucmp(n, d) = LESS then (zero, n)
	else
	    let fun countBits acc d' =
		    let val d'' = shl1 d'
		    in if isZero d'' orelse ucmp(d'', n) = GREATER
			  orelse ucmp(d'', d') = LESS
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

    fun udiv (a, b) = let val (q, _) = udivmod(a, b) in q end;
    fun umod (a, b) = let val (_, r) = udivmod(a, b) in r end;

in

fun fromInt (x : int) : word =
    if x >= 0 then (x div base, x mod base)
    else let val x' = x + base
	 in norm (~1 + (x' div base), x' mod base) end;

fun toInt ((hi, lo) : word) : int =
    if hi >= base div 2 then raise Overflow
    else let val v = hi * base + lo
	 in if v < 0 then raise Overflow else v end;

fun toIntX ((hi, lo) : word) : int =
    let val signbit = hi >= hbit
    in if signbit then
	   let val shi = hi - base
	       val v = shi * base + lo
	   in if v >= 0 then raise Overflow else v end
       else
	   let val v = hi * base + lo
	   in if v < 0 then raise Overflow else v end
    end;

fun fromLargeInt x = fromInt x;
fun toLargeInt x   = toInt x;
fun toLargeIntX x  = toIntX x;

fun toLarge (w : word) : word    = w;
fun toLargeX (w : word) : word   = w;
fun fromLarge (w : word) : word  = w;
val toLargeWord  = toLarge;
val toLargeWordX = toLargeX;
val fromLargeWord = fromLarge;

fun compare (x, y : word) = ucmp(x, y);

fun orb  ((h1,l1) : word, (h2,l2) : word) : word =
    let prim_val or_ : int -> int -> int = 2 "or"
    in (or_ h1 h2, or_ l1 l2) end;

fun andb ((h1,l1) : word, (h2,l2) : word) : word =
    let prim_val and_ : int -> int -> int = 2 "and"
    in (and_ h1 h2, and_ l1 l2) end;

fun xorb ((h1,l1) : word, (h2,l2) : word) : word =
    let prim_val xor_ : int -> int -> int = 2 "xor"
    in (xor_ h1 h2, xor_ l1 l2) end;

fun notb w = xorb (w, allOnes);

fun << (w, k) =
    let val k' = Word.toInt k
    in if k' >= 64 orelse k' < 0 then zero
       else let fun sh w 0 = w | sh w n = sh (shl1 w) (n-1)
	    in sh w k' end
    end;

fun >> (w, k) =
    let val k' = Word.toInt k
    in if k' >= 64 orelse k' < 0 then zero
       else let fun sh w 0 = w | sh w n = sh (shr1 w) (n-1)
	    in sh w k' end
    end;

fun ~>> ((hi, lo), k) =
    let val k' = Word.toInt k
	val signbit = hi >= hbit
    in if k' >= 64 orelse k' < 0 then
	   (if signbit then allOnes else zero)
       else if k' = 0 then (hi, lo)
       else
	   let val shifted = >> ((hi, lo), k)
	   in if signbit then
		  let val signFill = << (allOnes, Word.fromInt (64 - k'))
		  in orb (shifted, signFill) end
	      else shifted
	   end
    end;

local
    open StringCvt
    fun hexval c =
	if #"0" <= c andalso c <= #"9" then Char.ord c - 48
	else (Char.ord c - 55) mod 32;
    fun prhex i = if i < 10 then Char.chr(i + 48) else Char.chr(i + 55);

    val w2  = fromInt 2
    val w8  = fromInt 8
    val w10 = fromInt 10
    val w16 = fromInt 16

    fun conv radix (w : word) =
	let val r = case radix of 2 => w2 | 8 => w8 | 10 => w10
				| 16 => w16 | _ => w10
	    fun digit n = prhex (#2 (umod(n, r)))
	    fun h n res =
		if isZero n then res
		else h (udiv(n, r)) (digit n :: res)
	    fun tostr n = h (udiv(n, r)) [digit n]
	in String.implode (tostr w) end
in
    fun scan radix getc source =
	let open StringCvt
	    val source = skipWS getc source
	    val (isDigit, factor) =
		case radix of
		    BIN => (fn c => (#"0" <= c andalso c <= #"1"),  w2)
		  | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  w8)
		  | DEC => (Char.isDigit,                           w10)
		  | HEX => (Char.isHexDigit,                        w16)
	    fun dig1 NONE             = NONE
	      | dig1 (SOME (c, rest)) =
		let fun digr res src =
		        case getc src of
			    NONE           => SOME (res, src)
			  | SOME (c, rest) =>
				if isDigit c then
				    let val res1 = umul(factor, res)
					val res2 = uadd(res1, fromInt (hexval c))
				    in if ucmp(res1, res) = LESS
					  orelse ucmp(res2, res1) = LESS
				       then raise Overflow
				       else digr res2 rest
				    end
				else SOME (res, src)
		in
		    if isDigit c then digr (fromInt (hexval c)) rest
		    else NONE
		end
	    fun getdigs after0 src =
		case dig1 (getc src) of
		    NONE => SOME(zero, after0)
		  | res  => res
	    fun hexprefix after0 src =
		if radix <> HEX then getdigs after0 src
		else
		    case getc src of
			SOME(#"x", rest) => getdigs after0 rest
		      | SOME(#"X", rest) => getdigs after0 rest
		      | SOME _           => getdigs after0 src
		      | NONE => SOME(zero, after0)
    in
	case getc source of
	    SOME(#"0", after0) =>
		(case getc after0 of
		     SOME(#"w", src2) => hexprefix after0 src2
		   | SOME _           => hexprefix after0 after0
		   | NONE             => SOME(zero, after0))
	  | SOME _ => dig1 (getc source)
	  | NONE   => NONE
    end;

    fun fmt BIN = conv  2
      | fmt OCT = conv  8
      | fmt DEC = conv 10
      | fmt HEX = conv 16;

    fun toString w   = conv 16 w;
    fun fromString s = scanString (scan HEX) s
end; (* local for string functions *)

val op ~    : word -> word        = fn w => uadd(notb w, one);

fun (w1 : word) +   (w2 : word) = uadd(w1, w2);
fun (w1 : word) -   (w2 : word) = usub(w1, w2);
fun (w1 : word) *   (w2 : word) = umul(w1, w2);
val op div  : word * word -> word = udiv;
val op mod  : word * word -> word = umod;

fun min(w1 : word, w2) = if ucmp(w1, w2) = GREATER then w2 else w1;
fun max(w1 : word, w2) = if ucmp(w1, w2) = GREATER then w1 else w2;
val op >    : word * word -> bool = fn (a, b) => ucmp(a, b) = GREATER;
val op >=   : word * word -> bool = fn (a, b) => ucmp(a, b) <> LESS;
val op <    : word * word -> bool = fn (a, b) => ucmp(a, b) = LESS;
val op <=   : word * word -> bool = fn (a, b) => ucmp(a, b) <> GREATER;

end
