(* test/int32.sml -- test cases for Int32 structure
   PS 2026-05-03, based on MLKit test/int32.sml *)

use "auxil.sml";
load "Int32";

local
    open Int32
    infix 7 quot rem
    fun divmod (i, d, q, r)  = check(i div d = q andalso i mod d = r);
    fun quotrem (i, d, q, r) = check(i quot d = q andalso i rem d = r);
in

(* Precision and bounds *)

val test0a = check(precision = SOME 32);
val test0b = check(valOf maxInt = 2147483647);
val test0c = check(valOf minInt = ~2147483648);
val test0d = check(sign (valOf minInt) = ~1 andalso sign (valOf maxInt) = 1
		   andalso sameSign(valOf minInt, ~1)
		   andalso sameSign(valOf maxInt, 1));

(* Conversion: toInt, fromInt *)

val test1a = check(toInt 34 = 34);
val test1b = check(toInt 12 = 12);
val test1c = check(toInt ~12 = ~12);
val test1d = check(fromInt ~12 = ~12);
val test1e = check(fromInt ~12 <> ~3);
val test1f = check(toInt (valOf maxInt) = 2147483647);
val test1g = check(toInt (valOf minInt) = ~2147483648);
val test1h = check(fromInt 2147483647 = valOf maxInt);
val test1i = check(fromInt ~2147483648 = valOf minInt);
val test1j = check'(fn _ => (fromInt 2147483648; false) handle Overflow => true);
val test1k = check'(fn _ => (fromInt ~2147483649; false) handle Overflow => true);

(* toLarge, fromLarge *)

val test1l = check(toLarge 42 = 42);
val test1m = check(fromLarge 42 = 42);
val test1n = check'(fn _ => (fromLarge 2147483648; false) handle Overflow => true);

(* Basic arithmetic *)

val test2a = check(~12 = ~ 12);
val test2b = check(~24 = ~ 12 * 2);
val test2c = check(abs ~24 = 24);
val test2d = check(abs 12 = 12);
val test2e = check(~12 = ~12);
val test2f = check(~(~14) = 14);
val test2g = check(abs ~12 <> ~12);

(* Comparisons *)

val test3a = check(~12 < 12);
val test3b = check(~12 <= 12);
val test3c = check(13 > 4);
val test3d = check(13 >= 12);
val test3e = check(12 >= 12);
val test3f = check(12 <= 12);

(* Arithmetic ops *)

val test4a = check(~12 + 25 = 13);
val test4b = check(~12 - 13 = ~25);
val test4c = check(1222 * 10 = 12220);

(* div and mod *)

val test5a = divmod(10, 3, 3, 1);
val test5b = divmod(~10, 3, ~4, 2);
val test5c = divmod(~10, ~3, 3, ~1);
val test5d = divmod(10, ~3, ~4, ~2);

(* quot and rem *)

val test6a = quotrem(10, 3, 3, 1);
val test6b = quotrem(~10, 3, ~3, ~1);
val test6c = quotrem(~10, ~3, 3, ~1);
val test6d = quotrem(10, ~3, ~3, 1);

(* min, max *)

val test7a = check(max(~5, 2) = 2 andalso max(5, 2) = 5);
val test7b = check(min(~5, 3) = ~5 andalso min(5, 2) = 2);

(* sign, sameSign *)

val test8a = check(sign ~57 = ~1 andalso sign 99 = 1 andalso sign 0 = 0);
val test8b = check(sameSign(~255, ~256) andalso sameSign(255, 256)
		   andalso sameSign(0, 0));

(* compare *)

val test9a = check(compare(~5, 2) = LESS);
val test9b = check(compare(5, 5) = EQUAL);
val test9c = check(compare(5, 2) = GREATER);

(* div/mod invariant: (i div d) * d + (i mod d) = i *)

local
    fun checkDivMod i d =
	let val q = i div d
	    val r = i mod d
	in
	    (d * q + r = i) andalso
	    ((0 <= r andalso r < d) orelse (d < r andalso r <= 0))
	end
in
val test10a = check(checkDivMod 23 10);
val test10b = check(checkDivMod ~23 10);
val test10c = check(checkDivMod 23 ~10);
val test10d = check(checkDivMod ~23 ~10);
val test10e = check(checkDivMod 100 10);
val test10f = check(checkDivMod ~100 10);
val test10g = check(checkDivMod 100 ~10);
val test10h = check(checkDivMod ~100 ~10);
val test10i = check(checkDivMod 100 1);
val test10j = check(checkDivMod 100 ~1);
val test10k = check(checkDivMod 0 1);
val test10l = check(checkDivMod 0 ~1);
end;

(* Div and Overflow exceptions *)

val test11a = check'(fn _ => (100 div 0; false) handle Div => true);
val test11b = check'(fn _ => (100 mod 0; false) handle Div => true);
val test11c = check'(fn _ => (valOf minInt div ~1; false) handle Overflow => true);

(* Overflow at boundaries *)

val test12a = check'(fn _ => (~(valOf minInt); false) handle Overflow => true);
val test12b = check'(fn _ => (abs(valOf minInt); false) handle Overflow => true);
val test12c = check'(fn _ => (valOf maxInt + 1; false) handle Overflow => true);
val test12d = check'(fn _ => (valOf minInt - 1; false) handle Overflow => true);
val test12e = check'(fn _ => (valOf minInt * ~1; false) handle Overflow => true);

(* Overflow via function values *)

local
    val sum = (op +) : int * int -> int
    val diff = (op -) : int * int -> int
    val prod = (op * ) : int * int -> int
in
val test12f = check'(fn _ => (sum(valOf maxInt, 1); false) handle Overflow => true);
val test12g = check'(fn _ => (diff(valOf minInt, 1); false) handle Overflow => true);
val test12h = check'(fn _ => (prod(valOf minInt, ~1); false) handle Overflow => true);
end;

(* fromString *)

fun chk f (s, r) =
    check'(fn _ =>
	   case f s of
	       SOME res => res = r
	     | NONE     => false)

fun chkScan fmt = chk (StringCvt.scanString (scan fmt))

val test13a =
    List.map (chk fromString)
             [("10789", 10789),
	      ("+10789", 10789),
	      ("~10789", ~10789),
	      ("-10789", ~10789),
	      (" \n\t10789crap", 10789),
	      (" \n\t+10789crap", 10789),
	      (" \n\t~10789crap", ~10789),
	      (" \n\t-10789crap", ~10789),
	      ("0w123", 0),
	      ("0W123", 0),
	      ("0x123", 0),
	      ("0X123", 0),
	      ("0wx123", 0),
	      ("0wX123", 0)];

val test13b =
    List.map (fn s => case fromString s of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1", "ff"];

(* scan DEC *)

val test14a =
    List.map (chkScan StringCvt.DEC)
             [("10789", 10789),
	      ("+10789", 10789),
	      ("~10789", ~10789),
	      ("-10789", ~10789),
	      (" \n\t10789crap", 10789),
	      (" \n\t+10789crap", 10789),
	      (" \n\t~10789crap", ~10789),
	      (" \n\t-10789crap", ~10789),
	      ("0w123", 0),
	      ("0W123", 0),
	      ("0x123", 0),
	      ("0X123", 0),
	      ("0wx123", 0),
	      ("0wX123", 0)];

val test14b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.DEC) s
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1", "ff"];

(* scan BIN *)

val test15a =
    List.map (chkScan StringCvt.BIN)
             [("10010", 18),
	      ("+10010", 18),
	      ("~10010", ~18),
	      ("-10010", ~18),
	      (" \n\t10010crap", 18),
	      (" \n\t+10010crap", 18),
	      (" \n\t~10010crap", ~18),
	      (" \n\t-10010crap", ~18),
	      ("0w101", 0),
	      ("0W101", 0),
	      ("0x101", 0),
	      ("0X101", 0),
	      ("0wx101", 0),
	      ("0wX101", 0)];

val test15b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.BIN) s
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1", "2", "8", "ff"];

(* scan OCT *)

val test16a =
    List.map (chkScan StringCvt.OCT)
             [("2071", 1081),
	      ("+2071", 1081),
	      ("~2071", ~1081),
	      ("-2071", ~1081),
	      (" \n\t2071crap", 1081),
	      (" \n\t+2071crap", 1081),
	      (" \n\t~2071crap", ~1081),
	      (" \n\t-2071crap", ~1081),
	      ("0w123", 0),
	      ("0W123", 0),
	      ("0x123", 0),
	      ("0X123", 0),
	      ("0wx123", 0),
	      ("0wX123", 0)];

val test16b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.OCT) s
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1", "8", "ff"];

(* scan HEX *)

val test17a =
    List.map (chkScan StringCvt.HEX)
             [("20Af", 8367),
	      ("+20Af", 8367),
	      ("~20Af", ~8367),
	      ("-20Af", ~8367),
	      (" \n\t20AfGrap", 8367),
	      (" \n\t+20AfGrap", 8367),
	      (" \n\t~20AfGrap", ~8367),
	      (" \n\t-20AfGrap", ~8367),
	      ("0w123", 0),
	      ("0W123", 0),
	      ("0x", 0),
	      ("0x ", 0),
	      ("0xG", 0),
	      ("0X", 0),
	      ("0XG", 0),
	      ("0x123", 291),
	      ("0X123", 291),
	      ("-0x123", ~291),
	      ("-0X123", ~291),
	      ("~0x123", ~291),
	      ("~0X123", ~291),
	      ("+0x123", 291),
	      ("+0X123", 291),
	      ("0wx123", 0),
	      ("0wX123", 0)];

val test17b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.HEX) s
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1"];

(* fmt/toString/scan roundtrip *)

local
    fun fromToString i =
	fromString (toString i) = SOME i;

    fun scanFmt radix i =
	StringCvt.scanString (scan radix) (fmt radix i) = SOME i;
in
val test18 =
    check'(fn _ => range (~1200, 1200) fromToString);

val test19 =
    check'(fn _ => range (~1200, 1200) (scanFmt StringCvt.BIN));

val test20 =
    check'(fn _ => range (~1200, 1200) (scanFmt StringCvt.OCT));

val test21 =
    check'(fn _ => range (~1200, 1200) (scanFmt StringCvt.DEC));

val test22 =
    check'(fn _ => range (~1200, 1200) (scanFmt StringCvt.HEX));

(* roundtrip at boundaries *)

val test23a = check'(fn _ => scanFmt StringCvt.HEX (valOf maxInt));
val test23b = check'(fn _ => scanFmt StringCvt.DEC (valOf maxInt));
val test23c = check'(fn _ => scanFmt StringCvt.OCT (valOf maxInt));
val test23d = check'(fn _ => scanFmt StringCvt.BIN (valOf maxInt));

val test24a = check'(fn _ => scanFmt StringCvt.HEX (valOf minInt));
val test24b = check'(fn _ => scanFmt StringCvt.DEC (valOf minInt));
val test24c = check'(fn _ => scanFmt StringCvt.OCT (valOf minInt));
val test24d = check'(fn _ => scanFmt StringCvt.BIN (valOf minInt));

val test25a = check'(fn _ => scanFmt StringCvt.HEX (valOf minInt + 10));
val test25b = check'(fn _ => scanFmt StringCvt.DEC (valOf minInt + 10));
val test25c = check'(fn _ => scanFmt StringCvt.OCT (valOf minInt + 10));
val test25d = check'(fn _ => scanFmt StringCvt.BIN (valOf minInt + 10));
end;

(* scan overflow *)

local
    fun chkScanOvf fmt s =
	check'(fn _ => (StringCvt.scanString (scan fmt) s; false) handle Overflow => true)
in
val test26a = chkScanOvf StringCvt.HEX "~80000001";
val test26b = chkScanOvf StringCvt.DEC "~2147483649";
val test26c = chkScanOvf StringCvt.OCT "~20000000001";
val test26d = chkScanOvf StringCvt.BIN "~10000000000000000000000000000001";

val test27a = chkScanOvf StringCvt.HEX "80000000";
val test27b = chkScanOvf StringCvt.DEC "2147483648";
val test27c = chkScanOvf StringCvt.OCT "20000000000";
val test27d = chkScanOvf StringCvt.BIN "10000000000000000000000000000000";
end;

(* toString at boundaries *)

val test28a = check'(fn _ => toString (valOf maxInt) = "2147483647");
val test28b = check'(fn _ => toString (valOf minInt) = "~2147483648");

(* fromString at boundaries *)

val test29a = check'(fn _ => fromString "2147483647" = maxInt);
val test29b = check'(fn _ => fromString "~2147483648" = minInt);

(* fmt in various radices at boundaries *)

val test30a = check'(fn _ => fmt StringCvt.HEX (valOf maxInt) = "7FFFFFFF");
val test30b = check'(fn _ => fmt StringCvt.HEX (valOf minInt) = "~80000000");
val test30c = check'(fn _ => fmt StringCvt.OCT (valOf maxInt) = "17777777777");
val test30d = check'(fn _ => fmt StringCvt.OCT (valOf minInt) = "~20000000000");
val test30e = check'(fn _ => fmt StringCvt.BIN (valOf maxInt) = "1111111111111111111111111111111");
val test30f = check'(fn _ => fmt StringCvt.BIN (valOf minInt) = "~10000000000000000000000000000000");

(* Hex literal syntax *)

val test31a = check(valOf maxInt = 0x7fffffff);
val test31b = check(valOf maxInt = 0x7FFFFFFF);
val test31c = check(valOf minInt = ~0x80000000);

(* Pattern matching on Int32 values *)

val test32a = check(case 34 : int of 34 => true | _ => false);
val test32b = check(case 34 : int of 32 => false | _ => true);
val test32c = check(case ~34 : int of ~34 => true | _ => false);

end
