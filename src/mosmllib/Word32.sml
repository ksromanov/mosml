(* Word32 -- 32-bit unsigned words, 2026-05-03 *)

type word = word;
val wordSize = 32;

local
    prim_val orb_       : word -> word -> word      = 2 "or";
    prim_val andb_      : word -> word -> word      = 2 "and";
    prim_val xorb_      : word -> word -> word      = 2 "xor";
    prim_val lshift_    : word -> Word.word -> word = 2 "shift_left";
    prim_val rshiftsig_ : word -> Word.word -> word = 2 "shift_right_signed";
    prim_val rshiftuns_ : word -> Word.word -> word = 2 "shift_right_unsigned";

    prim_val fromInt_ : int -> word = 1 "identity";
    prim_val toInt_   : word -> int = 1 "identity";
    prim_val largeToWord_ : Word.word -> word = 1 "identity";

    val mask = fromInt_ 4294967295;     (* 0xFFFFFFFF = 2^32-1 *)
    val signbit = fromInt_ 2147483648;  (* 0x80000000 = 2^31 *)
    fun norm w = andb_ w mask;

    prim_val word2int : Word.word -> int = 1 "identity";
in
    prim_val toInt : word -> int = 1 "identity";
    fun toIntX w = if toInt_ (andb_ w signbit) <> 0 then
		       toInt_ (orb_ w (fromInt_ ~4294967296))
		   else toInt_ w;
    fun fromInt i = norm (fromInt_ i);

    prim_val toLargeInt : word -> int = 1 "identity";
    val toLargeIntX = toIntX;
    val fromLargeInt = fromInt;

    prim_val toLargeWord   : word -> Word.word = 1 "identity";
    prim_val toLarge       : word -> Word.word = 1 "identity";

    fun toLargeX w = if toInt_ (andb_ w signbit) <> 0 then
			 toLarge (orb_ w (fromInt_ ~4294967296))
		     else toLarge w;
    fun fromLarge w = norm (largeToWord_ w);

    val toLargeWordX = toLargeX;
    val fromLargeWord = fromLarge;

    fun orb (x, y)  = andb_ (orb_ x y) mask;
    fun andb (x, y) = andb_ x y;
    fun xorb (x, y) = andb_ (xorb_ x y) mask;
    fun notb x      = andb_ (xorb_ x (fromInt_ ~1)) mask;

    val ~ = fn w => norm (fromInt_ (~ (toInt_ w)));

    fun << (w, k) =
	if word2int k >= 32 orelse word2int k < 0 then fromInt_ 0
	else norm (lshift_ w k);

    fun >> (w, k) =
	if word2int k >= 32 orelse word2int k < 0 then fromInt_ 0
	else rshiftuns_ w k;

    fun ~>> (w, k) =
	if toInt_ (andb_ w signbit) = 0 then
	    if word2int k >= 32 orelse word2int k < 0 then fromInt_ 0
	    else rshiftuns_ w k
	else
	    if word2int k >= 32 orelse word2int k < 0 then
		mask
	    else norm (rshiftsig_ (orb_ w (fromInt_ ~4294967296)) k);

    local
      open StringCvt
      fun skipWSget getc source = getc (skipWS getc source)

      fun hexval c =
	  if #"0" <= c andalso c <= #"9" then
	      Char.ord c - 48
	  else
	      (Char.ord c - 55) mod 32;

      fun prhex i =
	  if i < 10 then Char.chr(i + 48) else Char.chr(i + 55);

      fun conv radix w =
	  let fun h n res =
		  if n = 0 then res
		  else h (n div radix) (prhex (n mod radix) :: res)
	      fun tostr n = h (n div radix) [prhex (n mod radix)]
	  in String.implode (tostr (toInt w)) end
    in
      fun scan radix getc source =
	  let open StringCvt
	      val source = skipWS getc source
	      val (isDigit, factor) =
		  case radix of
		      BIN => (fn c => (#"0" <= c andalso c <= #"1"),  2)
		    | OCT => (fn c => (#"0" <= c andalso c <= #"7"),  8)
		    | DEC => (Char.isDigit,                          10)
		    | HEX => (Char.isHexDigit,                       16)
	      fun return res src =
		  if res < 4294967296 then SOME (fromInt_ res, src)
		  else raise Overflow
	      fun dig1 NONE             = NONE
		| dig1 (SOME (c, rest)) =
		  let
		      fun digr res src =
		          case getc src of
			      NONE           => return res src
			    | SOME (c, rest) =>
				  if isDigit c then
				      digr(factor*res+hexval c) rest
				  else
				      return res src
		  in
		      if isDigit c then digr (hexval c) rest else NONE
		  end
	      fun getdigs after0 src =
		  case dig1 (getc src) of
		      NONE => return 0 after0
		    | res  => res
	      fun hexprefix after0 src =
		  if radix <> HEX then getdigs after0 src
		  else
		      case getc src of
			  SOME(#"x", rest) => getdigs after0 rest
			| SOME(#"X", rest) => getdigs after0 rest
			| SOME _           => getdigs after0 src
			| NONE => return 0 after0
	  in
	      case getc source of
		  SOME(#"0", after0) =>
		      (case getc after0 of
			   SOME(#"w", src2) => hexprefix after0 src2
			 | SOME _           => hexprefix after0 after0
			 | NONE             => return 0 after0)
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

    fun (w1 : word) + (w2 : word) =
	norm (fromInt_ (Int.+ (toInt_ w1, toInt_ w2)));
    fun (w1 : word) - (w2 : word) =
	norm (fromInt_ (Int.- (toInt_ w1, toInt_ w2)));
    fun (w1 : word) * (w2 : word) =
	norm (fromInt_ (Int.* (toInt_ w1, toInt_ w2)));
    val op div  : word * word -> word = op div;
    val op mod  : word * word -> word = op mod;

    fun min(w1 : word, w2) = if w1 > w2 then w2 else w1;
    fun max(w1 : word, w2) = if w1 > w2 then w1 else w2;
    fun compare (x, y: word) =
	if x<y then LESS else if x>y then GREATER else EQUAL;
    val op >    : word * word -> bool = op >;
    val op >=   : word * word -> bool = op >=;
    val op <    : word * word -> bool = op <;
    val op <=   : word * word -> bool = op <=;
end
