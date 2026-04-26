(* Primitive type shim for Moscow ML.
 * Maps MLton's built-in primitive types to Moscow ML equivalents.
 * Compiled automatically when _prim is encountered in an .mlb file. *)

type char8 = char
type char16 = int
type char32 = int

type int1 = int   type int2 = int   type int3 = int   type int4 = int
type int5 = int   type int6 = int   type int7 = int   type int8 = int
type int9 = int   type int10 = int  type int11 = int  type int12 = int
type int13 = int  type int14 = int  type int15 = int  type int16 = int
type int17 = int  type int18 = int  type int19 = int  type int20 = int
type int21 = int  type int22 = int  type int23 = int  type int24 = int
type int25 = int  type int26 = int  type int27 = int  type int28 = int
type int29 = int  type int30 = int  type int31 = int  type int32 = int
type int64 = int
type intInf = int

type real32 = real
type real64 = real

type word1 = word   type word2 = word   type word3 = word   type word4 = word
type word5 = word   type word6 = word   type word7 = word   type word8 = word
type word9 = word   type word10 = word  type word11 = word  type word12 = word
type word13 = word  type word14 = word  type word15 = word  type word16 = word
type word17 = word  type word18 = word  type word19 = word  type word20 = word
type word21 = word  type word22 = word  type word23 = word  type word24 = word
type word25 = word  type word26 = word  type word27 = word  type word28 = word
type word29 = word  type word30 = word  type word31 = word  type word32 = word
type word64 = word

type cpointer = word
type thread = unit
type 'a weak = 'a option ref
