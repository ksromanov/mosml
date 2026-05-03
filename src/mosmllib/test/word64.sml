(* test/word64.sml -- test cases for Word64 structure
   PS 2026-05-03, based on MLKit test/word64.sml *)

use "auxil.sml";
load "Word64";

local
    val [gt, lt, ge, le] =
	[op>, op<, op>=, op<=] : (int * int -> bool) list;
    val [add, sub, mul, idiv, imod] =
	[op+, op-, op*, op div, op mod] : (int * int -> int) list;
    open Word64
    val op > = gt and op < = lt and op >= = ge and op <= = le;
    val op + = add and op - = sub and op * = mul
    and op div = idiv and op mod = imod;
    fun i2w i = fromInt i
    and w2i w = toIntX w
    (* Use 32-bit bounds for arithmetic tests that convert back via toIntX,
       since toIntX can only return values that fit in 63-bit native int *)
    val maxposint32 = 2147483647          (* 2^31 - 1 *)
    val maxnegint32 = ~2147483648         (* ~2^31 *)
    val maxposint = 4611686018427387903   (* Int.maxInt on 63-bit = 2^62-1 *)
    val maxnegint = ~4611686018427387904  (* Int.minInt on 63-bit = -2^62 *)
    fun pwr2 0 = 1
      | pwr2 n = 2 * pwr2 (n-1);
    fun rwp i 0 = i
      | rwp i n = rwp i (n-1) div 2;
in

(* toInt/fromInt roundtrip *)

val test1 = checkrange (0, 1025)
    (fn i => i = w2i (i2w i));

(* toLargeIntX roundtrip for small values *)

val test3 = checkrange (~1000, 1000)
    (fn i => i = toLargeIntX (i2w i));

(* Bitwise operations *)

val test5a = checkrange (0,15)
    (fn i => (i+960) div 2 * 2 + 1
             = w2i (orb (i2w i, i2w 961)));
val test5b = checkrange (0,513)
    (fn i => i = w2i (orb (i2w i, i2w i)));
val test6a = checkrange (0,15)
    (fn i => i div 2 * 2 = w2i (andb (i2w i, i2w ~2)));
val test6b = checkrange (0,513)
    (fn i => i = w2i (andb (i2w i, i2w i)));
val test7a = checkrange (0,15)
    (fn i => i+960 = w2i (xorb (i2w i, i2w 960)));
val test7b = checkrange (0, 513)
    (fn i => 0 = w2i (xorb (i2w i, i2w i)));
val test8a = check (~1 = w2i (notb (i2w 0)));
val test8b = check (0 = w2i (notb (i2w ~1)));

(* Shift operations *)

val test9a = checkrange (0,1)
    (fn k => pwr2 k = w2i (Word64.<< (i2w 1, Word.fromInt k)));
val test9d = checkrange (0, 1025)
    (fn i => 2 * i = w2i (Word64.<< (i2w i, Word.fromInt 1)));
val test9e = checkrange (0, 1025)
    (fn i => i div 2 = w2i (Word64.>> (i2w i, Word.fromInt 1)));
val test9h = checkrange (1,65)
    (fn k => 0 = w2i (Word64.>> (i2w 1, Word.fromInt k)));

(* Shift left by 64+ gives 0 *)
val test9b = checkrange (64,80)
    (fn k => Word64.<< (i2w 1, Word.fromInt k) = i2w 0);
(* Shift left by 63 gives high bit *)
val test9c = check(Word64.toString (Word64.<< (i2w 1, Word.fromInt 63))
		   = "8000000000000000");
(* Progressive right shift of maxposint *)
val test9f = checkrange (0,65)
    (fn k => rwp maxposint k = w2i (Word64.>> (i2w maxposint, Word.fromInt k)));
(* Shift left of ~1 by 64+ gives 0 *)
val test9g = checkrange (64,80)
    (fn k => Word64.<< (i2w ~1, Word.fromInt k) = i2w 0);

(* Arithmetic shift right *)

val test10a = checkrange (1,65)
    (fn k => 0 = w2i (Word64.~>> (i2w 1, Word.fromInt k)));
val test10b = checkrange (1,65)
    (fn k => ~1 = w2i (Word64.~>> (i2w ~1, Word.fromInt k)));
val test10c = checkrange (~513, 513)
    (fn i => i div 2 = toLargeIntX (Word64.~>> (i2w i, Word.fromInt 1)));
(* Progressive arithmetic shift right of maxnegint *)
val test10d = checkrange (0,65)
    (fn k => rwp maxnegint k = toLargeIntX (Word64.~>> (i2w maxnegint, Word.fromInt k)));

(* Unsigned comparisons *)

local
    open Word64
in
val test11a = check (i2w 256 > i2w 255);
val test11b = check (i2w 0 < i2w ~1);
val test11c = check (i2w maxposint >= i2w maxposint);
val test11d = check (i2w maxnegint >= i2w 127);
val test11e = check (i2w 1 <= i2w 1);
val test11f = check (i2w 0 <= i2w 1);
val test11g = check (i2w 0 < i2w maxposint);
val test11h = check (i2w maxposint < i2w maxnegint);
val test11i = check (i2w maxnegint < i2w ~1);
end;

(* Modular arithmetic *)

local
    open Word64
in
val test12a = checkrange(0, 300) (fn k => w2i (i2w k + i2w 17) = add(k, 17));
val test12b = checkrange(0, 300) (fn k => w2i (i2w k - i2w 17) = sub(k, 17));
val test12c = checkrange(0, 300) (fn k => w2i (i2w k * i2w 17) = mul(k, 17));
val test12d = checkrange(0, 300)
    (fn k => w2i (i2w k div i2w 17) = idiv(k, 17));
val test12e = checkrange(0, 300)
    (fn k => w2i (i2w k mod i2w 17) = imod(k, 17));
(* Wrap-around: 0 - 1 = maxword *)
val test12f = check(Word64.toString (i2w 0 - i2w 1) = "FFFFFFFFFFFFFFFF");
(* Wrap-around: maxword + 1 = 0 *)
val test12g = check(i2w 0 - i2w 1 + i2w 1 = i2w 0);
(* Modular multiplication *)
val test12h = checkrange(0, 300)
    (fn k => w2i (i2w k * i2w 1) = k);
(* Small value division by large word *)
val test12i = checkrange(0, 300)
    (fn k => Word64.compare(i2w k div i2w ~1, i2w 0) = EQUAL);
(* Small value mod large word = itself *)
val test12j = checkrange(0, 300)
    (fn k => w2i (i2w k mod i2w ~1) = k);
(* maxword div 2 *)
val test12k = check(Word64.toString (i2w ~1 div i2w 2)
		    = "7FFFFFFFFFFFFFFF");
(* maxword mod 2 *)
val test12l = check(w2i (i2w ~1 mod i2w 2) = 1);
(* maxword mod 10 *)
val test12m = check(Word64.fmt StringCvt.DEC (i2w ~1 mod i2w 10) = "5");
(* Two's complement negation *)
val test12n = check(~ (i2w 1) = i2w ~1);
val test12o = check(~ (i2w 0) = i2w 0);
val test12r = (i2w 17 div i2w 0 seq "WRONG")
              handle Div => "OK" | _ => "WRONG";
val test12s = (i2w 17 mod i2w 0 seq "WRONG")
              handle Div => "OK" | _ => "WRONG";
end;

(* String functions *)

local
    open Word64
    fun chk f (s, r) =
	check'(fn _ =>
	       case f s of
		   SOME res => res = i2w r
		 | NONE     => false)
    fun chkScan fmt = chk (StringCvt.scanString (scan fmt))
in

val test13a =
    List.map (chk fromString)
             [("20Af", 8367),
	      (" \n\t20AfGrap", 8367),
	      ("0w20Af", 8367),
	      (" \n\t0w20AfGrap", 8367),
	      ("0", 0),
	      ("0w", 0),
	      ("0W1", 0),
	      ("0w ", 0),
	      ("0wx", 0),
	      ("0wX", 0),
	      ("0wx1", 1),
	      ("0wX1", 1),
	      ("0wx ", 0),
	      ("0wX ", 0)];
val test13b =
    List.map (fn s => case fromString s of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+1", "~1", "-1", "GG"];

val test14a =
    List.map (chkScan StringCvt.DEC)
             [("10789", 10789),
	      (" \n\t10789crap", 10789),
	      ("0w10789", 10789),
	      (" \n\t0w10789crap", 10789),
	      ("0", 0),
	      ("0w", 0),
	      ("0W1", 0),
	      ("0w ", 0),
	      ("0wx", 0),
	      ("0wX", 0),
	      ("0wx1", 0),
	      ("0wX1", 0),
	      ("0wx ", 0),
	      ("0wX ", 0)];
val test14b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.DEC) s
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+1", "~1", "-1", "ff"];

val test15a =
    List.map (chkScan StringCvt.BIN)
             [("10010", 18),
	      (" \n\t10010crap", 18),
	      ("0w10010", 18),
	      (" \n\t0w10010crap", 18),
	      ("0", 0),
	      ("0w", 0),
	      ("0W1", 0),
	      ("0w ", 0),
	      ("0wx", 0),
	      ("0wX", 0),
	      ("0wx1", 0),
	      ("0wX1", 0),
	      ("0wx ", 0),
	      ("0wX ", 0)];
val test15b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.BIN) s
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+1", "~1", "-1", "2", "8", "ff"];

val test16a =
    List.map (chkScan StringCvt.OCT)
             [("2071", 1081),
	      (" \n\t2071crap", 1081),
	      ("0w2071", 1081),
	      (" \n\t0w2071crap", 1081),
	      ("0", 0),
	      ("0w", 0),
	      ("0W1", 0),
	      ("0w ", 0),
	      ("0wx", 0),
	      ("0wX", 0),
	      ("0wx1", 0),
	      ("0wX1", 0),
	      ("0wx ", 0),
	      ("0wX ", 0)];
val test16b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.OCT) s
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+1", "~1", "-1", "8", "ff"];

val test17a =
    List.map (chkScan StringCvt.HEX)
             [("20Af", 8367), (" \n\t20AfGrap", 8367),
	      ("0wx20Af", 8367), (" \n\t0wx20AfGrap", 8367),
	      ("0wX20Af", 8367), (" \n\t0wX20AfGrap", 8367),
	      ("0x20Af", 8367), (" \n\t0x20AfGrap", 8367),
	      ("0X20Af", 8367), (" \n\t0X20AfGrap", 8367),
	      ("0", 0),
	      ("0w", 0),
	      ("0w ", 0),
	      ("0w1", 1),
	      ("0W1", 0),
	      ("0wx", 0),
	      ("0wX", 0),
	      ("0wx1", 1),
	      ("0wX1", 1)];
val test17b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.HEX) s
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+1", "~1", "-1"];
end;

(* fmt/toString/scan roundtrip *)

local
    fun fromToString i =
	Word64.fromString (Word64.toString (Word64.fromInt i))
	= SOME (Word64.fromInt i);
    fun scanFmt radix i =
	let val w = Word64.fromInt i
	    val s = Word64.fmt radix w
	in StringCvt.scanString (Word64.scan radix) s = SOME w end;
in
val test18 =
    check'(fn _ => range (0, 1200) fromToString);
val test19 =
    check'(fn _ => range (0, 1200) (scanFmt StringCvt.BIN));
val test20 =
    check'(fn _ => range (0, 1200) (scanFmt StringCvt.OCT));
val test21 =
    check'(fn _ => range (0, 1200) (scanFmt StringCvt.DEC));
val test22 =
    check'(fn _ => range (0, 1200) (scanFmt StringCvt.HEX));
end;

(* Boundary values and toInt/toIntX *)

val test23a = check'(fn _ => (Word64.toInt (Word64.fromInt ~1); false)
		     handle Overflow => true);
val test23b = check'(fn _ => Word64.toIntX (Word64.fromInt ~1) = ~1);
val test23e = check'(fn _ => Word64.toIntX (Word64.fromInt 4611686018427387903)
		     = 4611686018427387903);
val test23f = check'(fn _ => Word64.toIntX (Word64.fromInt ~4611686018427387904)
		     = ~4611686018427387904);

(* Boundary string representations *)

val test24a = check'(fn _ => Word64.toString (Word64.fromInt ~1)
		     = "FFFFFFFFFFFFFFFF");
val test24b = check'(fn _ => Word64.fmt StringCvt.DEC (Word64.fromInt ~1)
		     = "18446744073709551615");

(* Scan overflow *)

val test25a = check'(fn _ => (Word64.fromString "10000000000000000"; false)
		     handle Overflow => true);

end
