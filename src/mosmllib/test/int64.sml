(* test/int64.sml -- test cases for Int64 structure
   PS 2026-05-03, based on MLKit test/int64.sml *)

use "auxil.sml";
load "Int64";

local
    open Int64
    infix 7 quot rem

    fun i x = fromInt x
    fun chkI x = toInt x

    fun divmod (a, b, q, r) =
	check(chkI(i a div i b) = q andalso chkI(i a mod i b) = r);
    fun quotrem (a, b, q, r) =
	check(chkI(i a quot i b) = q andalso chkI(i a rem i b) = r);

    fun range64 (from, to) p =
	(Int.>(from, to)) orelse (p (i from))
	andalso (range64 (Int.+(from,1), to) p);
in

(* Precision and bounds *)

val test0a = check(precision = SOME 64);
val test0b = check(toString (valOf maxInt) = "9223372036854775807");
val test0c = check(toString (valOf minInt) = "~9223372036854775808");
val test0d = check(sign (valOf minInt) = ~1 andalso sign (valOf maxInt) = 1
		   andalso sameSign(valOf minInt, i ~1)
		   andalso sameSign(valOf maxInt, i 1));

(* Conversion: toInt, fromInt *)

val test1a = check(toInt (i 34) = 34);
val test1b = check(toInt (i 12) = 12);
val test1c = check(toInt (i ~12) = ~12);
val test1d = check(toInt (i 0) = 0);
val test1e = check(fromInt ~12 <> fromInt ~3);
val test1f = check(i ~12 = ~ (i 12));
val test1g = check(i ~24 = ~ (i 12) * i 2);
val test1h = check(abs (i ~24) = i 24);
val test1i = check(abs (i 12) = i 12);
val test1j = check(~ (i 12) = i ~12);
val test1k = check(~ (~ (i 14)) = i 14);
val test1l = check(abs (i ~12) <> i ~12);

(* Comparisons *)

val test2a = check(i ~12 < i 12);
val test2b = check(i ~12 <= i 12);
val test2c = check(i 13 > i 4);
val test2d = check(i 13 >= i 12);
val test2e = check(i 12 >= i 12);
val test2f = check(i 12 <= i 12);

(* Arithmetic ops *)

val test3a = check(i ~12 + i 25 = i 13);
val test3b = check(i ~12 - i 13 = i ~25);
val test3c = check(i 1222 * i 10 = i 12220);

(* div and mod *)

val test4a = divmod(10, 3, 3, 1);
val test4b = divmod(~10, 3, ~4, 2);
val test4c = divmod(~10, ~3, 3, ~1);
val test4d = divmod(10, ~3, ~4, ~2);

(* quot and rem *)

val test5a = quotrem(10, 3, 3, 1);
val test5b = quotrem(~10, 3, ~3, ~1);
val test5c = quotrem(~10, ~3, 3, ~1);
val test5d = quotrem(10, ~3, ~3, 1);

(* min, max *)

val test6a = check(max(i ~5, i 2) = i 2 andalso max(i 5, i 2) = i 5);
val test6b = check(min(i ~5, i 3) = i ~5 andalso min(i 5, i 2) = i 2);

(* sign, sameSign *)

val test7a = check(sign (i ~57) = ~1 andalso sign (i 99) = 1
		   andalso sign (i 0) = 0);
val test7b = check(sameSign(i ~255, i ~256) andalso sameSign(i 255, i 256)
		   andalso sameSign(i 0, i 0));

(* compare *)

val test8a = check(compare(i ~5, i 2) = LESS);
val test8b = check(compare(i 5, i 5) = EQUAL);
val test8c = check(compare(i 5, i 2) = GREATER);

(* div/mod invariant: (i div d) * d + (i mod d) = i *)

local
    fun checkDivMod a b =
	let val ia = i a
	    val ib = i b
	    val q = ia div ib
	    val r = ia mod ib
	in ia = q * ib + r andalso
	   ((r = i 0) orelse
	    (compare(ib, i 0) = GREATER andalso compare(r, i 0) = GREATER) orelse
	    (compare(ib, i 0) = LESS andalso compare(r, i 0) = LESS))
	end
in
val test9a = check(checkDivMod 23 10);
val test9b = check(checkDivMod ~23 10);
val test9c = check(checkDivMod 23 ~10);
val test9d = check(checkDivMod ~23 ~10);
val test9e = check(checkDivMod 100 10);
val test9f = check(checkDivMod ~100 10);
val test9g = check(checkDivMod 100 ~10);
val test9h = check(checkDivMod ~100 ~10);
val test9i = check(checkDivMod 100 1);
val test9j = check(checkDivMod 100 ~1);
val test9k = check(checkDivMod 0 1);
val test9l = check(checkDivMod 0 ~1);
end;

(* Div and Overflow exceptions *)

val test10a = check'(fn _ => (i 100 div i 0; false) handle Div => true);
val test10b = check'(fn _ => (i 100 mod i 0; false) handle Div => true);
val test10c = check'(fn _ => (valOf minInt div i ~1; false) handle Overflow => true);

(* Overflow at boundaries *)

val test11a = check'(fn _ => (~(valOf minInt); false) handle Overflow => true);
val test11b = check'(fn _ => (abs(valOf minInt); false) handle Overflow => true);
val test11c = check'(fn _ => (valOf maxInt + i 1; false) handle Overflow => true);
val test11d = check'(fn _ => (valOf minInt - i 1; false) handle Overflow => true);
val test11e = check'(fn _ => (valOf minInt * i ~1; false) handle Overflow => true);

(* Large value arithmetic *)

val test12a = check(toString (i 1000000 * i 1000000) = "1000000000000");
val test12b = check(toString (i 1000000000 * i 1000) = "1000000000000");
val test12c = check(let val big = i 1000000000 * i 1000000000
		    in toString big = "1000000000000000000" end);

(* fromString *)

fun chk f (s, r) =
    check'(fn _ =>
	   case f s of
	       SOME res => res = r
	     | NONE     => false);

fun chkScan fmt = chk (StringCvt.scanString (scan fmt));

val test13a =
    List.map (chk fromString)
             [("10789", i 10789),
	      ("+10789", i 10789),
	      ("~10789", i ~10789),
	      ("-10789", i ~10789),
	      (" \n\t10789crap", i 10789),
	      (" \n\t+10789crap", i 10789),
	      (" \n\t~10789crap", i ~10789),
	      (" \n\t-10789crap", i ~10789),
	      ("0w123", i 0),
	      ("0W123", i 0),
	      ("0x123", i 0),
	      ("0X123", i 0),
	      ("0wx123", i 0),
	      ("0wX123", i 0)];

val test13b =
    List.map (fn s => case fromString s of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1", "ff"];

(* scan DEC *)

val test14a =
    List.map (chkScan StringCvt.DEC)
             [("10789", i 10789),
	      ("+10789", i 10789),
	      ("~10789", i ~10789),
	      ("-10789", i ~10789),
	      (" \n\t10789crap", i 10789),
	      (" \n\t+10789crap", i 10789),
	      (" \n\t~10789crap", i ~10789),
	      (" \n\t-10789crap", i ~10789),
	      ("0w123", i 0),
	      ("0W123", i 0),
	      ("0x123", i 0),
	      ("0X123", i 0),
	      ("0wx123", i 0),
	      ("0wX123", i 0)];

val test14b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.DEC) s
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1", "ff"];

(* scan BIN *)

val test15a =
    List.map (chkScan StringCvt.BIN)
             [("10010", i 18),
	      ("+10010", i 18),
	      ("~10010", i ~18),
	      ("-10010", i ~18),
	      (" \n\t10010crap", i 18),
	      (" \n\t+10010crap", i 18),
	      (" \n\t~10010crap", i ~18),
	      (" \n\t-10010crap", i ~18),
	      ("0w101", i 0),
	      ("0W101", i 0),
	      ("0x101", i 0),
	      ("0X101", i 0),
	      ("0wx101", i 0),
	      ("0wX101", i 0)];

val test15b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.BIN) s
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1", "2", "8", "ff"];

(* scan OCT *)

val test16a =
    List.map (chkScan StringCvt.OCT)
             [("2071", i 1081),
	      ("+2071", i 1081),
	      ("~2071", i ~1081),
	      ("-2071", i ~1081),
	      (" \n\t2071crap", i 1081),
	      (" \n\t+2071crap", i 1081),
	      (" \n\t~2071crap", i ~1081),
	      (" \n\t-2071crap", i ~1081),
	      ("0w123", i 0),
	      ("0W123", i 0),
	      ("0x123", i 0),
	      ("0X123", i 0),
	      ("0wx123", i 0),
	      ("0wX123", i 0)];

val test16b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.OCT) s
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1", "8", "ff"];

(* scan HEX *)

val test17a =
    List.map (chkScan StringCvt.HEX)
             [("20Af", i 8367),
	      ("+20Af", i 8367),
	      ("~20Af", i ~8367),
	      ("-20Af", i ~8367),
	      (" \n\t20AfGrap", i 8367),
	      (" \n\t+20AfGrap", i 8367),
	      (" \n\t~20AfGrap", i ~8367),
	      (" \n\t-20AfGrap", i ~8367),
	      ("0w123", i 0),
	      ("0W123", i 0),
	      ("0x", i 0),
	      ("0x ", i 0),
	      ("0xG", i 0),
	      ("0X", i 0),
	      ("0XG", i 0),
	      ("0x123", i 291),
	      ("0X123", i 291),
	      ("-0x123", i ~291),
	      ("-0X123", i ~291),
	      ("~0x123", i ~291),
	      ("~0X123", i ~291),
	      ("+0x123", i 291),
	      ("+0X123", i 291),
	      ("0wx123", i 0),
	      ("0wX123", i 0)];

val test17b =
    List.map (fn s => case StringCvt.scanString (scan StringCvt.HEX) s
	              of NONE => "OK" | _ => "WRONG")
	   ["", "-", "~", "+", " \n\t", " \n\t-", " \n\t~", " \n\t+",
	    "+ 1", "~ 1", "- 1"];

(* fmt/toString/scan roundtrip *)

local
    fun fromToString x =
	fromString (toString x) = SOME x;

    fun scanFmt radix x =
	StringCvt.scanString (scan radix) (fmt radix x) = SOME x;
in
val test18 =
    check'(fn _ => range64 (~1200, 1200) fromToString);

val test19 =
    check'(fn _ => range64 (~1200, 1200) (scanFmt StringCvt.BIN));

val test20 =
    check'(fn _ => range64 (~1200, 1200) (scanFmt StringCvt.OCT));

val test21 =
    check'(fn _ => range64 (~1200, 1200) (scanFmt StringCvt.DEC));

val test22 =
    check'(fn _ => range64 (~1200, 1200) (scanFmt StringCvt.HEX));

(* roundtrip at boundaries *)

val test23a = check'(fn _ => scanFmt StringCvt.HEX (valOf maxInt));
val test23b = check'(fn _ => scanFmt StringCvt.DEC (valOf maxInt));
val test23c = check'(fn _ => scanFmt StringCvt.OCT (valOf maxInt));
val test23d = check'(fn _ => scanFmt StringCvt.BIN (valOf maxInt));

val test24a = check'(fn _ => scanFmt StringCvt.HEX (valOf minInt));
val test24b = check'(fn _ => scanFmt StringCvt.DEC (valOf minInt));
val test24c = check'(fn _ => scanFmt StringCvt.OCT (valOf minInt));
val test24d = check'(fn _ => scanFmt StringCvt.BIN (valOf minInt));

val test25a = check'(fn _ => scanFmt StringCvt.HEX (valOf minInt + i 10));
val test25b = check'(fn _ => scanFmt StringCvt.DEC (valOf minInt + i 10));
val test25c = check'(fn _ => scanFmt StringCvt.OCT (valOf minInt + i 10));
val test25d = check'(fn _ => scanFmt StringCvt.BIN (valOf minInt + i 10));
end;

(* scan overflow *)

local
    fun chkScanOvf fmt s =
	check'(fn _ => (StringCvt.scanString (scan fmt) s; false)
	       handle Overflow => true)
in
val test26a = chkScanOvf StringCvt.HEX "~8000000000000001";
val test26b = chkScanOvf StringCvt.DEC "~9223372036854775809";
val test26c = chkScanOvf StringCvt.OCT "~1000000000000000000001";
val test26d = chkScanOvf StringCvt.BIN "~1000000000000000000000000000000000000000000000000000000000000001";

val test27a = chkScanOvf StringCvt.HEX "10000000000000000";
val test27b = chkScanOvf StringCvt.DEC "9223372036854775808";
val test27c = chkScanOvf StringCvt.OCT "1000000000000000000000";
val test27d = chkScanOvf StringCvt.BIN "1000000000000000000000000000000000000000000000000000000000000000";
end;

(* toString at boundaries *)

val test28a = check'(fn _ => toString (valOf maxInt) = "9223372036854775807");
val test28b = check'(fn _ => toString (valOf minInt) = "~9223372036854775808");

(* fromString at boundaries *)

val test29a = check'(fn _ => fromString "9223372036854775807" = maxInt);
val test29b = check'(fn _ => fromString "~9223372036854775808" = minInt);

(* fmt in various radices at boundaries *)

val test30a = check'(fn _ => fmt StringCvt.HEX (valOf maxInt) = "7FFFFFFFFFFFFFFF");
val test30b = check'(fn _ => fmt StringCvt.HEX (valOf minInt) = "~8000000000000000");
val test30c = check'(fn _ => fmt StringCvt.OCT (valOf maxInt) = "777777777777777777777");
val test30d = check'(fn _ => fmt StringCvt.OCT (valOf minInt) = "~1000000000000000000000");

end
