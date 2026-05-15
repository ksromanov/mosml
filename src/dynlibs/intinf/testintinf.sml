(* testintinf.sml -- IntInf test suite for Moscow ML
   Consolidates tests from four sources:
   1. Original mosml testintinf.sml (PS 1995-09-05, 1998-04-12)
   2. MLKit test suite (mlkit/test/intinf.sml, mael 2005-12-13, ported 2026)
   3. Zarith stress tests (zarith/tests/bi.ml, Antoine Miné, LGPL 2, ported 2026)
   4. OCaml Num library tests (num/test/test_big_ints.ml, INRIA, LGPL, ported 2026) *)

load "IntInf";

(* =========================================================================
   Section 1: Original mosml tests (testintinf.sml)
   ========================================================================= *)

use "../../mosmllib/test/auxil.sml";

local
    open IntInf
    fun divmod1 (i, d, q, r)  =
	check'(fn () => (toInt (fromInt i div fromInt d) = q
			 andalso toInt(fromInt i mod fromInt d) = r));
    fun quotrem1 (i, d, q, r) =
	check'(fn () => (toInt (quot(fromInt i, fromInt d)) = q
			 andalso toInt (rem(fromInt i, fromInt d)) = r));

    fun divmod2 (i, d, q, r)  =
	check'(fn () => let val (q', r') = divMod(fromInt i, fromInt d)
			in toInt q' = q andalso toInt r' = r end);
    fun quotrem2 (i, d, q, r) =
	check'(fn () => let val (q', r') = quotRem(fromInt i, fromInt d)
			in toInt q' = q andalso toInt r' = r end);

    fun add1 (x, y, sum) =
	check'(fn () => (toInt (fromInt x + fromInt y) = sum));
    fun sub1 (x, y, diff) =
	check'(fn () => (toInt (fromInt x - fromInt y) = diff));
    fun mul1 (x, y, prod) =
	check'(fn () => (toInt (fromInt x * fromInt y) = prod));
in

val test1a = divmod1(10, 3, 3, 1);
val test1b = divmod1(~10, 3, ~4, 2);
val test1c = divmod1(~10, ~3, 3, ~1);
val test1d = divmod1(10, ~3, ~4, ~2);

val test2a = quotrem1(10, 3, 3, 1);
val test2b = quotrem1(~10, 3, ~3, ~1);
val test2c = quotrem1(~10, ~3, 3, ~1);
val test2d = quotrem1(10, ~3, ~3, 1);

val test3a = divmod2(10, 3, 3, 1);
val test3b = divmod2(~10, 3, ~4, 2);
val test3c = divmod2(~10, ~3, 3, ~1);
val test3d = divmod2(10, ~3, ~4, ~2);

val test4a = quotrem2(10, 3, 3, 1);
val test4b = quotrem2(~10, 3, ~3, ~1);
val test4c = quotrem2(~10, ~3, 3, ~1);
val test4d = quotrem2(10, ~3, ~3, 1);

val test5a = ((fromInt 1 div fromInt 0) seq "WRONG")
	     handle Div => "OK" | _ => "WRONG";
val test5b = ((fromInt 1 mod fromInt 0) seq "WRONG")
	     handle Div => "OK" | _ => "WRONG";
val test5c = (quot(fromInt 1, fromInt 0) seq "WRONG")
	     handle Div => "OK" | _ => "WRONG";
val test5d = (rem(fromInt 1, fromInt 0) seq "WRONG")
	     handle Div => "OK" | _ => "WRONG";
val test5e = (divMod(fromInt 1, fromInt 0) seq "WRONG")
	     handle Div => "OK" | _ => "WRONG";
val test5f = (quotRem(fromInt 1, fromInt 0) seq "WRONG")
	     handle Div => "OK" | _ => "WRONG";

val test6a =
    List.map add1 [(12,17,29), (~12,17,5), (12,~17,~5), (~12,~17,~29)];
val test6b =
    List.map sub1 [(12,17,~5), (~12,17,~29), (12,~17,29), (~12,~17,5)];
val test6c =
    List.map mul1 [(12,17,204), (~12,17,~204), (12,~17,~204), (~12,~17,204)];

fun chkToString (i, s) = check'(fn _ => toString(fromInt i) = s);

val test12a =
    List.map chkToString [(0, "0"), (~1, "~1"),
			  (12345678, "12345678"), (~12345678, "~12345678")];

fun chk f (s, r) =
    check'(fn _ =>
	   case f s of
	       SOME res => toInt res = r
	     | NONE     => false)

fun chkScan fmt = chk (StringCvt.scanString (scan fmt))

val test13a =
    List.map (chk fromString)
	     [("10789", 10789), ("+10789", 10789), ("~10789", ~10789),
	      ("-10789", ~10789), (" \n\t10789crap", 10789),
	      (" \n\t+10789crap", 10789), (" \n\t~10789crap", ~10789),
	      (" \n\t-10789crap", ~10789)];

val test13b =
    List.map (fn s => case fromString s of NONE => "OK" | _ => "WRONG")
	     ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	      "+ 1", "~ 1", "- 1", "ff"];

val test14a =
    List.map (chkScan StringCvt.DEC)
	     [("10789", 10789), ("+10789", 10789), ("~10789", ~10789),
	      ("-10789", ~10789), (" \n\t10789crap", 10789),
	      (" \n\t+10789crap", 10789), (" \n\t~10789crap", ~10789),
	      (" \n\t-10789crap", ~10789)];

val test14b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.DEC) s
		      of NONE => "OK" | _ => "WRONG")
	     ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	      "+ 1", "~ 1", "- 1", "ff"];

val test15a =
    List.map (chkScan StringCvt.BIN)
	     [("10010", 18), ("+10010", 18), ("~10010", ~18), ("-10010", ~18),
	      (" \n\t10010crap", 18), (" \n\t+10010crap", 18),
	      (" \n\t~10010crap", ~18), (" \n\t-10010crap", ~18)];

val test15b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.BIN) s
		      of NONE => "OK" | _ => "WRONG")
	     ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	      "+ 1", "~ 1", "- 1", "2", "8", "ff"];

val test16a =
    List.map (chkScan StringCvt.OCT)
	     [("2071", 1081), ("+2071", 1081), ("~2071", ~1081), ("-2071", ~1081),
	      (" \n\t2071crap", 1081), (" \n\t+2071crap", 1081),
	      (" \n\t~2071crap", ~1081), (" \n\t-2071crap", ~1081)];

val test16b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.OCT) s
		      of NONE => "OK" | _ => "WRONG")
	     ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	      "+ 1", "~ 1", "- 1", "8", "ff"];

val test17a =
    List.map (chkScan StringCvt.HEX)
	     [("20Af", 8367), ("+20Af", 8367), ("~20Af", ~8367), ("-20Af", ~8367),
	      (" \n\t20AfGrap", 8367), (" \n\t+20AfGrap", 8367),
	      (" \n\t~20AfGrap", ~8367), (" \n\t-20AfGrap", ~8367)];

val test17b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.HEX) s
		      of NONE => "OK" | _ => "WRONG")
	     ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	      "+ 1", "~ 1", "- 1"];

val test18 =
    check'(fn _ =>
	   toInt(pow(fromInt 12, 3)) = 1728
	   andalso toInt(pow(fromInt  0,  1)) = 0
	   andalso toInt(pow(fromInt  1,  0)) = 1
	   andalso toInt(pow(fromInt  0,  0)) = 1
	   andalso toInt(pow(fromInt  1, ~1)) = 1
	   andalso toInt(pow(fromInt ~1, ~1)) = ~1
	   andalso toInt(pow(fromInt  2, ~1)) = 0
	   andalso toInt(pow(fromInt ~2, ~1)) = 0)

(* Equality and inequality *)
val test19a = check'(fn _ => fromInt 42 = fromInt 42);
val test19b = check'(fn _ => not (fromInt 42 = fromInt 99));
val test19c = check'(fn _ => fromInt 0 = fromInt 0);
val test19d = check'(fn _ => fromInt ~1 = fromInt ~1);
val test19e = check'(fn _ => not (fromInt 1 = fromInt ~1));

val test20a = check'(fn _ => pow(fromInt 2, 100) = pow(fromInt 2, 100));
val test20b = check'(fn _ => not (pow(fromInt 2, 100) = pow(fromInt 2, 99)));
val test20c = check'(fn _ => let val x = pow(fromInt 2, 200) in x = x end);

val test21a = check'(fn _ => fromInt 42 <> fromInt 99);
val test21b = check'(fn _ => not (fromInt 42 <> fromInt 42));

end; (* section 1 *)

(* =========================================================================
   Section 2: MLKit tests (mlkit/test/intinf.sml)
   ========================================================================= *)

local
    open IntInf
    fun ptest t s = print(t ^ ": " ^ s ^ "\n")
    fun ptestl t nil = ()
      | ptestl t (s::ss) = (ptest t s; ptestl t ss)
    infix 1 seq
    fun e1 seq e2 = e2
    fun check b = if b then "OK" else "WRONG"
    fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN"

    fun divmod1 (i, d, q, r)  =
	check'(fn () => (toInt (fromInt i div fromInt d) = q
			 andalso toInt(fromInt i mod fromInt d) = r));
    fun quotrem1 (i, d, q, r) =
	check'(fn () => (toInt (quot(fromInt i, fromInt d)) = q
			 andalso toInt (rem(fromInt i, fromInt d)) = r));
    fun divmod2 (i, d, q, r)  =
	check'(fn () => let val (q', r') = divMod(fromInt i, fromInt d)
			in toInt q' = q andalso toInt r' = r end);
    fun quotrem2 (i, d, q, r) =
	check'(fn () => let val (q', r') = quotRem(fromInt i, fromInt d)
			in toInt q' = q andalso toInt r' = r end);
    fun add1 (x, y, sum) =
	check'(fn () => (toInt (fromInt x + fromInt y) = sum));
    fun sub1 (x, y, diff) =
	check'(fn () => (toInt (fromInt x - fromInt y) = diff));
    fun mul1 (x, y, prod) =
	check'(fn () => (toInt (fromInt x * fromInt y) = prod));
in

val _ = List.app (ptest "divmod")
    [divmod1(10,3,3,1), divmod1(~10,3,~4,2),
     divmod1(~10,~3,3,~1), divmod1(10,~3,~4,~2)];
val _ = List.app (ptest "quotrem")
    [quotrem1(10,3,3,1), quotrem1(~10,3,~3,~1),
     quotrem1(~10,~3,3,~1), quotrem1(10,~3,~3,1)];
val _ = List.app (ptest "divmod2")
    [divmod2(10,3,3,1), divmod2(~10,3,~4,2),
     divmod2(~10,~3,3,~1), divmod2(10,~3,~4,~2)];
val _ = List.app (ptest "quotrem2")
    [quotrem2(10,3,3,1), quotrem2(~10,3,~3,~1),
     quotrem2(~10,~3,3,~1), quotrem2(10,~3,~3,1)];

val _ = ptest "div_zero"  (((fromInt 1 div fromInt 0) seq "WRONG") handle Div => "OK" | _ => "WRONG");
val _ = ptest "mod_zero"  (((fromInt 1 mod fromInt 0) seq "WRONG") handle Div => "OK" | _ => "WRONG");
val _ = ptest "quot_zero" ((quot(fromInt 1,fromInt 0) seq "WRONG")  handle Div => "OK" | _ => "WRONG");
val _ = ptest "rem_zero"  ((rem(fromInt 1,fromInt 0) seq "WRONG")   handle Div => "OK" | _ => "WRONG");
val _ = ptest "divMod_zero"  ((divMod(fromInt 1,fromInt 0) seq "WRONG") handle Div => "OK" | _ => "WRONG");
val _ = ptest "quotRem_zero" ((quotRem(fromInt 1,fromInt 0) seq "WRONG") handle Div => "OK" | _ => "WRONG");

val _ = ptestl "add" (List.map add1 [(12,17,29),(~12,17,5),(12,~17,~5),(~12,~17,~29)]);
val _ = ptestl "sub" (List.map sub1 [(12,17,~5),(~12,17,~29),(12,~17,29),(~12,~17,5)]);
val _ = ptestl "mul" (List.map mul1 [(12,17,204),(~12,17,~204),(12,~17,~204),(~12,~17,204)]);

fun chkToString (i, s) = check'(fn _ => toString(fromInt i) = s);
val _ = ptestl "toString"
    (List.map chkToString [(0,"0"),(~1,"~1"),(12345678,"12345678"),(~12345678,"~12345678")]);

fun chk f (s, r) = check'(fn _ => case f s of SOME res => toInt res = r | NONE => false)
fun chkScan fmt  = chk (StringCvt.scanString (scan fmt))

val _ = ptestl "fromString"
    (List.map (chk fromString)
	[("10789",10789),("+10789",10789),("~10789",~10789),("-10789",~10789),
	 (" \n\t10789crap",10789),(" \n\t+10789crap",10789),
	 (" \n\t~10789crap",~10789),(" \n\t-10789crap",~10789)]);
val _ = ptestl "fromString_none"
    (List.map (fn s => case fromString s of NONE => "OK" | _ => "WRONG")
	["","-","~","+"," \n\t"," \n\t-"," \n\t~"," \n\t+","+ 1","~ 1","- 1","ff"]);
val _ = ptestl "scan_bin"
    (List.map (chkScan StringCvt.BIN)
	[("10010",18),("+10010",18),("~10010",~18),("-10010",~18),
	 (" \n\t10010crap",18),(" \n\t+10010crap",18),
	 (" \n\t~10010crap",~18),(" \n\t-10010crap",~18)]);
val _ = ptestl "scan_oct"
    (List.map (chkScan StringCvt.OCT)
	[("2071",1081),("+2071",1081),("~2071",~1081),("-2071",~1081),
	 (" \n\t2071crap",1081),(" \n\t+2071crap",1081),
	 (" \n\t~2071crap",~1081),(" \n\t-2071crap",~1081)]);
val _ = ptestl "scan_hex"
    (List.map (chkScan StringCvt.HEX)
	[("20Af",8367),("+20Af",8367),("~20Af",~8367),("-20Af",~8367),
	 (" \n\t20AfGrap",8367),(" \n\t+20AfGrap",8367),
	 (" \n\t~20AfGrap",~8367),(" \n\t-20AfGrap",~8367)]);

val _ = ptest "pow" (check'(fn _ =>
    toInt(pow(fromInt 12, 3)) = 1728 andalso toInt(pow(fromInt 1, ~1)) = 1
    andalso toInt(pow(fromInt 0, 0)) = 1 andalso toInt(pow(fromInt 2, ~1)) = 0));

(* Bitwise (from MLKit test19-20) *)
fun testbin cvt opr a1 a2 r =
    check(case (StringCvt.scanString (scan cvt) a1,
               StringCvt.scanString (scan cvt) a2) of
	    (SOME i1, SOME i2) => r = fmt cvt (opr(i1, i2))
	  | _ => false)

val _ = ptest "orb_bin_small" (testbin StringCvt.BIN (op orb) "01" "10" "11");
val _ = ptest "orb_bin_large" (testbin StringCvt.BIN (op orb)
    "001001001001001001001001001001" "100100100100100100100100100100"
    "101101101101101101101101101101");
val _ = ptest "orb_hex"  (testbin StringCvt.HEX (op orb) "ffffffffffff" "0" "ffffffffffff");
val _ = ptest "andb_bin" (testbin StringCvt.BIN (op andb) "101" "110" "100");
val _ = ptest "andb_bin_large" (testbin StringCvt.BIN (op andb)
    "1001001001001001001001001001001" "1100100100100100100100100100100"
    "1000000000000000000000000000000");
val _ = ptest "andb_hex" (testbin StringCvt.HEX (op andb) "ffffffffffff" "ffaffffffffa" "ffaffffffffa");

(* Large numbers *)
val _ = ptest "pow2_100"
    (check'(fn _ => toString (pow(fromInt 2, 100)) = "1267650600228229401496703205376"));
val _ = ptest "add_large"
    (check'(fn _ => let val a = valOf(fromString "123456789012345678901234567890")
			and b = valOf(fromString "987654321098765432109876543210")
		    in  toString(a+b) = "1111111110111111111011111111100" end));
val _ = ptest "shift_2_100"
    (check'(fn _ => let val a = << (fromInt 1, 100)
		    in  compare (>> (a, 100), fromInt 1) = EQUAL end));

end; (* section 2 *)

(* =========================================================================
   Section 3: Zarith algebraic law tests
   ========================================================================= *)

local
    open IntInf

    fun ptest t s = print(t ^ ": " ^ s ^ "\n")
    fun check' f = (if f () then "OK" else "WRONG") handle _ => "EXN"

    local
	val state = ref (Word.fromInt 42)
	fun xorw a b = Word.xorb (a, b)
	fun shlw w n = Word.<< (w, n)
	fun shrw w n = Word.>> (w, n)
    in
	fun rand () : Int.int =
	    let val x = !state
		val x = xorw x (shlw x 0w13)
		val x = xorw x (shrw x 0w7)
		val x = xorw x (shlw x 0w17)
		val _ = state := x
	    in Word.toIntX x
	    end
    end

    fun randBig () =
	let val r = rand ()
	    val nbits = Int.mod (if Int.>= (r, 0) then r else Int.~ r handle Overflow => 1, 200)
	    fun bits 0 acc = acc
	      | bits n acc = bits (Int.- (n, 1))
				  (orb (<< (acc, 1), fromInt (if Int.mod (rand (), 2) = 0 then 1 else 0)))
	in  if Int.mod (rand (), 2) = 0 then bits nbits (fromInt 0)
	    else ~ (bits nbits (fromInt 0))
	end

    fun pow2list n =
	if Int.< (n, 0) then []
	else << (fromInt 1, n) :: pow2list (Int.- (n, 1))

    val powers = pow2list 130
    val negs    = List.map ~ powers
    val succs   = List.map (fn x => x + fromInt 1) powers
    val preds   = List.map (fn x => x - fromInt 1) powers
    val nsteps  = List.map (fn x => x + fromInt 1) negs
    val nstepd  = List.map (fn x => x - fromInt 1) negs
    val smalls  = List.map fromInt [0,1,~1,2,~2,100,~100,1073741823,~1073741824]
		  @ [fromInt (valOf Int.maxInt), fromInt (valOf Int.minInt)]
    val corners = smalls @ powers @ negs @ succs @ preds @ nsteps @ nstepd
    val inputs  = corners @ List.tabulate (50, fn _ => randBig ())
    val nonneg  = List.filter (fn x => Int.>= (sign x, 0)) inputs

    fun chkAll p xs = check' (fn () => List.all p xs)
    fun chkAll2 p xs = check' (fn () => List.all (fn a => List.all (fn b => p a b) xs) xs)
    fun chkAll2s p xs = (* small subset for O(n^3) tests *)
	let val s = List.take (xs, Int.min (15, List.length xs))
	in check' (fn () => List.all (fn a => List.all (fn b => List.all (fn c => p a b c) s) s) s)
	end
in

val _ = ptest "roundtrip_toString"
    (chkAll (fn x => case fromString (toString x) of SOME y => compare(x,y) = EQUAL | NONE => false) inputs);
val _ = ptest "add_commutative"
    (chkAll2 (fn a b => compare(a+b,b+a) = EQUAL) inputs);
val _ = ptest "add_associative"
    (chkAll2s (fn a b c => compare((a+b)+c, a+(b+c)) = EQUAL) inputs);
val _ = ptest "add_zero_identity"
    (chkAll (fn a => compare(a + fromInt 0, a) = EQUAL) inputs);
val _ = ptest "sub_self_zero"
    (chkAll (fn a => compare(a - a, fromInt 0) = EQUAL) inputs);
val _ = ptest "add_sub_inverse"
    (chkAll2 (fn a b => compare(a+b-b, a) = EQUAL) inputs);
val _ = ptest "mul_commutative"
    (chkAll2 (fn a b => compare(a*b,b*a) = EQUAL) inputs);
val _ = ptest "mul_distributive"
    (chkAll2s (fn a b c => compare(a*(b+c), a*b+a*c) = EQUAL) inputs);
val _ = ptest "mul_zero"
    (chkAll (fn a => compare(a * fromInt 0, fromInt 0) = EQUAL) inputs);
val _ = ptest "mul_one_identity"
    (chkAll (fn a => compare(a * fromInt 1, a) = EQUAL) inputs);
val _ = ptest "quotRem_law"
    (chkAll2 (fn a b =>
	if compare(b, fromInt 0) = EQUAL then true
	else let val (q,r) = quotRem(a,b)
	     in compare(b*q+r,a) = EQUAL
		andalso (compare(r,fromInt 0) = EQUAL orelse sign r = sign a)
	     end) inputs);
val _ = ptest "divMod_law"
    (chkAll2 (fn a b =>
	if compare(b, fromInt 0) = EQUAL then true
	else let val (q,r) = divMod(a,b)
	     in compare(b*q+r,a) = EQUAL
		andalso (if Int.>(sign b,0) then Int.>=(sign r,0) andalso compare(r,abs b)=LESS
			 else Int.<=(sign r,0) andalso compare(abs r,abs b)=LESS)
	     end) inputs);
val _ = ptest "abs_neg"
    (chkAll (fn a => compare(abs(~a), abs a) = EQUAL) inputs);
val _ = ptest "neg_involutive"
    (chkAll (fn a => compare(~(~a), a) = EQUAL) inputs);
val _ = ptest "andb_idempotent"
    (chkAll (fn a => compare(andb(a,a),a) = EQUAL) nonneg);
val _ = ptest "orb_idempotent"
    (chkAll (fn a => compare(orb(a,a),a) = EQUAL) nonneg);
val _ = ptest "andb_commutative"
    (chkAll2 (fn a b => compare(andb(a,b),andb(b,a)) = EQUAL) nonneg);
val _ = ptest "orb_commutative"
    (chkAll2 (fn a b => compare(orb(a,b),orb(b,a)) = EQUAL) nonneg);
val _ = ptest "xorb_self_zero"
    (chkAll (fn a => compare(xorb(a,a), fromInt 0) = EQUAL) nonneg);
val _ = ptest "shift_left_1_doubles"
    (chkAll (fn a => compare(<< (a,1), a+a) = EQUAL) nonneg);
val _ = ptest "shift_right_1_halves"
    (chkAll (fn a => if compare(a,fromInt 0) = EQUAL then true
		     else compare(>>(a,1), a div fromInt 2) = EQUAL) nonneg);
val _ = ptest "compare_reflexive"
    (chkAll (fn a => compare(a,a) = EQUAL) inputs);
val _ = ptest "pow_2_is_square"
    (chkAll (fn a => compare(pow(a,2), a*a) = EQUAL) inputs);
val _ = ptest "pi_20_digits"
    (check'(fn () =>
	let fun arctanS (x : Int.int) scale =
		let val x2 = fromInt (Int.*(x,x))
		    fun loop t k neg s =
			let val term = t div fromInt k
			in if compare(term,fromInt 0) = EQUAL then s
			   else loop (t div x2) (Int.+(k,2)) (not neg)
				     (if neg then s-term else s+term)
			end
		in loop (scale div fromInt x) 1 false (fromInt 0)
		end
	    val sc  = pow(fromInt 10, 25)
	    val pi4 = arctanS 5 sc * fromInt 4 - arctanS 239 sc
	    val pi  = pi4 * fromInt 4 div pow(fromInt 10, 5)
	in  String.isPrefix "31415926535897932384" (toString pi)
	end));

end; (* section 3 *)

(* =========================================================================
   Section 4: Num library tests (num/test/test_big_ints.ml)
   ========================================================================= *)

local
    open IntInf

    val errs = ref 0
    fun ptest t s = print (t ^ ": " ^ s ^ "\n")
    fun ck tag expected actual =
	(if compare(expected, actual) = EQUAL then ptest tag "OK"
	 else (errs := Int.+ (!errs, 1);
	       print ("FAIL " ^ tag ^ ": expected " ^ toString expected ^
		      " got " ^ toString actual ^ "\n")))
    fun ckb tag (expected : bool) actual =
	(if expected = actual then ptest tag "OK"
	 else (errs := Int.+ (!errs, 1);
	       print ("FAIL " ^ tag ^ ": expected " ^ Bool.toString expected ^
		      " got " ^ Bool.toString actual ^ "\n")))
    fun cki tag (expected : Int.int) actual =
	(if expected = actual then ptest tag "OK"
	 else (errs := Int.+ (!errs, 1);
	       print ("FAIL " ^ tag ^ ": expected " ^ Int.toString expected ^
		      " got " ^ Int.toString actual ^ "\n")))
    fun cks tag (expected : string) actual =
	(if expected = actual then ptest tag "OK"
	 else (errs := Int.+ (!errs, 1);
	       print ("FAIL " ^ tag ^ ": expected " ^ expected ^ " got " ^ actual ^ "\n")))
    fun raises f = (f (); false) handle _ => true
    fun i n    = fromInt n
    fun s str  = valOf (fromString str)
    val z = fromInt 0
    val u = fromInt 1
    val n1 = fromInt ~1

    fun cmp_order expected actual =
	let val got = case actual of LESS => ~1 | EQUAL => 0 | GREATER => 1
	in  cki "compare" expected got
	end

    fun gcd_ii a b =
	let fun g a b = if compare(b,z) = EQUAL then a else g b (abs a mod abs b)
	in  g (abs a) (abs b)
	end

    fun shr_tz (x : IntInf.int) (n : Int.int) = quot(x, << (u, n))

    fun extract x o l =
	let val mask = << (u, l) - u
	in  andb (~>> (x, o), mask)
	end

    fun big128 hi lo = s hi * s "18446744073709551616" + s lo
in

(* compare *)
val _ = cmp_order 0  (compare (z,  z ));
val _ = cmp_order ~1 (compare (z,  i 1));
val _ = cmp_order 1  (compare (z,  n1));
val _ = cmp_order 1  (compare (u,  z ));
val _ = cmp_order ~1 (compare (n1, z ));
val _ = cmp_order 0  (compare (u,  u ));
val _ = cmp_order 1  (compare (u,  n1));
val _ = cmp_order ~1 (compare (n1, u ));
val _ = cmp_order ~1 (compare (u,  i 2));
val _ = cmp_order 1  (compare (n1, i ~2));

(* pred / succ *)
val _ = ck "pred_0"  n1 (z - u);
val _ = ck "pred_1"  z  (u - u);
val _ = ck "succ_0"  u  (z + u);
val _ = ck "succ_m1" z  (n1 + u);

(* add *)
val _ = ck "add_0_0"   z      (z  + z );
val _ = ck "add_1_1"   (i 2)  (u  + u );
val _ = ck "add_1_m1"  z      (u  + n1);
val _ = ck "add_m1_2"  u      (n1 + i 2);
val _ = ck "add_m1_m2" (i ~3) (n1 + i ~2);

(* sub *)
val _ = ck "sub_0_0"   z      (z  - z );
val _ = ck "sub_0_1"   n1     (z  - u );
val _ = ck "sub_2_1"   u      (i 2 - u );
val _ = ck "sub_1_m1"  (i 2)  (u  - n1);
val _ = ck "sub_m2_1"  (i ~3) (i ~2 - u );

(* mul *)
val _ = ck "mul_0_0"   z      (z  * z );
val _ = ck "mul_2_3"   (i 6)  (i 2 * i 3);
val _ = ck "mul_2_m3"  (i ~6) (i 2 * i ~3);
val _ = ck "mul_large" (s "1040259735709286400") (s "12724951" * s "81749606400");
val _ = ck "mul_large2"(s "2169804593037312000") (s "26542080" * s "81749606400");

(* divMod (floor division) *)
val _ = (let val (q,r) = divMod(i 1, i 1)   in ck "divmod_1_1"  u q; ck "divmod_1_1r"  z r end);
val _ = (let val (q,r) = divMod(i 1, i ~1)  in ck "divmod_1_m1" n1 q; ck "divmod_1_m1r" z r end);
val _ = (let val (q,r) = divMod(i ~5, i 3)  in ck "divmod_m5_3" (i ~2) q; ck "divmod_m5_3r" u r end);
val _ = (let val (q,r) = divMod(i ~1, i 3)  in ck "divmod_m1_3" n1 q; ck "divmod_m1_3r" (i 2) r end);
val _ = ckb "divmod_div0" true (raises (fn () => divMod(u, z)));
val _ = (let val (q,r) = divMod(i 10, i ~20)  in ck "divmod_10_m20"  n1 q; ck "divmod_10_m20r"  (i ~10) r end);
val _ = (let val (q,r) = divMod(i ~10, i ~20) in ck "divmod_m10_m20" z  q; ck "divmod_m10_m20r" (i ~10) r end);

(* GCD *)
val _ = ck "gcd_0_0"   z      (gcd_ii z z);
val _ = ck "gcd_0_1"   u      (gcd_ii z u);
val _ = ck "gcd_9_16"  u      (gcd_ii (i 9) (i 16));
val _ = ck "gcd_12_16" (i 4)  (gcd_ii (i 12) (i 16));
val _ = ck "gcd_12_18" (i 6)  (gcd_ii (i 12) (i 18));

(* toInt *)
val _ = cki "toInt_1"   1  (toInt u);
val _ = cki "toInt_m1" ~1  (toInt n1);
val _ = cki "toInt_0"   0  (toInt z);
val _ = ckb "toInt_ovf_hi" true (raises (fn () => toInt (fromInt (valOf Int.maxInt) + u)));
val _ = ckb "toInt_ovf_lo" true (raises (fn () => toInt (fromInt (valOf Int.minInt) - u)));

(* toString / fromString *)
val _ = cks "toString_1"   "1"  (toString u);
val _ = cks "toString_m1"  "~1" (toString n1);
val _ = cks "toString_123" "123456789012345678901234567890"
    (toString (s "123456789012345678901234567890"));
val _ = ck  "fromString_1"   u   (s "1");
val _ = ck  "fromString_m1"  n1  (valOf (fromString "~1"));
val _ = ck  "fromString_123" (i 123) (s "123");
val _ = ckb "fromString_bad" false (isSome (fromString "abc"));

(* Large number roundtrip *)
val bignum_str =
    "174679877494298468451661416292903906557638850173895426081611831060970135303" ^
    "044177587617233125776581034213405720474892937404345377707655788096850784519" ^
    "539374048533324740018513057210881137248587265169064879918339714405948322501" ^
    "445922724181830422326068913963858377101914542266807281471620827145038901025" ^
    "322784396182858865537924078131032036927586614781817695777639491934361211399" ^
    "888524140253852859555118862284235219972858420374290985423899099648066366558" ^
    "238523612660414395240146528009203942793935957539186742012316630755300111472" ^
    "852707974927265572257203394961525316215198438466177260614187266288417996647" ^
    "132974072337956513457924431633191471716899014677585762010115338540738783163" ^
    "739223806648361958204720897858193606022290696766988489073354139289154127309" ^
    "916985231051926209439373780384293513938376175026016587144157313996556653811" ^
    "793187841050456120649717382553450099049321059330947779485538381272648295449" ^
    "847188233356805715432460040567660999184007627415398722991790542115164516290" ^
    "619821378529926683447345857832940144982437162642295073360087284113248737998" ^
    "046564369129742074737760485635495880623324782103052289938185453627547195245" ^
    "688272436219215066430533447287305048225780425168823659431607654712261368560" ^
    "702129351210471250717394128044019490336608558608922841794819375031757643448" ^
    "32";
val _ = cks "bignum_roundtrip" bignum_str (toString (s bignum_str));

(* Bitwise *)
val _ = ck "andb_0"    z  (andb (u, z));
val _ = ck "andb_1"    u  (andb (u, u));
val _ = ck "orb_0"     u  (orb  (u, z));
val _ = ck "orb_1"     u  (orb  (u, u));
val _ = ck "xorb_0"    u  (xorb (u, z));
val _ = ck "xorb_self" z  (xorb (u, u));
(* 128-bit verified algebraically: andb(a,b) orb not_part = b *)
val _ = let val a = big128 "9214229513298721272" "7260474793543286381"
	    and b = big128 "6361613651834839280" "2153029484783181510"
	in  ck "andb_algebraic" b (orb(andb(a,b), andb(orb(a,b)-a, b)))
	end;

(* Shifts *)
val _ = ck "shl_0"  u             (<< (u, 0));
val _ = ck "shl_1"  (i 2)         (<< (u, 1));
val _ = ck "shl_64" (s "18446744073709551616") (<< (u, 64));
val _ = ck "shl_95" (s "39614081257132168796771975168") (<< (u, 95));
val _ = ck "shl_neg"(s "~5846006549323611672814739330865132078623730171904")
    (<< (s "~39614081257132168796771975168", 67));
val _ = ck "ashr_pos" (i 1543209)   (~>> (i 12345678, 3));
val _ = ck "ashr_neg" (i ~1235)     (~>> (s "~5299989648942", 32));
val _ = ck "trunc_shr" (i ~1234)    (shr_tz (s "~5299989648942") 32);

(* Extract *)
val _ = ck "extract_1" (i 6589) (extract (s "81985529216486895") 3 13);
val _ = ck "extract_2" (i 65535)(extract n1 0 16);

(* Power *)
val _ = ck "pow_1728"  (i 1728) (pow (i 12, 3));
val _ = ck "pow_2_100" (s "1267650600228229401496703205376") (pow (i 2, 100));
val _ = ck "pow_m2_128"(s "340282366920938463463374607431768211456") (pow (i ~2, 128));

val _ = if !errs = 0 then print "Num section: ALL OK\n"
	else print ("Num section: " ^ Int.toString (!errs) ^ " FAILURES\n");

end; (* section 4 *)

val _ = quit();
