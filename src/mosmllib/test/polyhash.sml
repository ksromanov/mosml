(* polyhash.sml -- tests for Polyhash.clear and Polyhash.peekSameHash
 * Adapted from MLKit src/Kitlib usage patterns (RegAlloc, pickle). *)

use "auxil.sml";
load "Polyhash";

val _ = print "Polyhash tests...\n";

exception NotFound;

(* ---- clear ---- *)

(* clear empties a non-empty table *)
val test_clear1 = check'(fn _ =>
    let val ht = Polyhash.mkPolyTable (16, NotFound)
        val _  = Polyhash.insert ht (1, "one")
        val _  = Polyhash.insert ht (2, "two")
        val _  = Polyhash.insert ht (3, "three")
        val _  = Polyhash.clear ht
    in Polyhash.numItems ht = 0 end);

(* after clear, all previous keys are gone *)
val test_clear2 = check'(fn _ =>
    let val ht = Polyhash.mkPolyTable (16, NotFound)
        val _  = Polyhash.insert ht ("a", 1)
        val _  = Polyhash.insert ht ("b", 2)
        val _  = Polyhash.clear ht
    in Polyhash.peek ht "a" = NONE andalso
       Polyhash.peek ht "b" = NONE end);

(* clear on empty table is a no-op *)
val test_clear3 = check'(fn _ =>
    let val ht = Polyhash.mkPolyTable (16, NotFound)
        val _  = Polyhash.clear ht
    in Polyhash.numItems ht = 0 end);

(* after clear, insert still works *)
val test_clear4 = check'(fn _ =>
    let val ht = Polyhash.mkPolyTable (16, NotFound)
        val _  = Polyhash.insert ht (1, "x")
        val _  = Polyhash.insert ht (2, "y")
        val _  = Polyhash.clear ht
        val _  = Polyhash.insert ht (3, "z")
    in Polyhash.numItems ht = 1 andalso
       Polyhash.find ht 3 = "z" end);

(* clear then find raises the table's exception *)
val test_clear5 = check'(fn _ =>
    let val ht = Polyhash.mkPolyTable (16, NotFound)
        val _  = Polyhash.insert ht (42, "answer")
        val _  = Polyhash.clear ht
    in (Polyhash.find ht 42; false)
       handle NotFound => true end);

(* clear with many items *)
val test_clear6 = check'(fn _ =>
    let val ht = Polyhash.mkPolyTable (8, NotFound)
        fun ins i = if i > 1000 then ()
                    else (Polyhash.insert ht (i, i*i); ins (i+1))
        val _  = ins 0
        val n1 = Polyhash.numItems ht
        val _  = Polyhash.clear ht
    in n1 = 1001 andalso Polyhash.numItems ht = 0 end);

(* ---- peekSameHash ---- *)

(* peekSameHash returns (bucket_size, hash_value) *)
val test_psh1 = check'(fn _ =>
    let val ht = Polyhash.mkPolyTable (16, NotFound)
        val (n, h) = Polyhash.peekSameHash ht "hello"
    in n = 0 andalso h = Polyhash.hash "hello" end);

(* after inserting a key, bucket has >= 1 item *)
val test_psh2 = check'(fn _ =>
    let val ht = Polyhash.mkPolyTable (1024, NotFound)
        val _  = Polyhash.insert ht ("hello", 1)
        val (n, h) = Polyhash.peekSameHash ht "hello"
    in n >= 1 andalso h = Polyhash.hash "hello" end);

(* hash value is consistent *)
val test_psh3 = check'(fn _ =>
    let val ht = Polyhash.mkPolyTable (16, NotFound)
        val (_, h1) = Polyhash.peekSameHash ht "test"
        val (_, h2) = Polyhash.peekSameHash ht "test"
    in h1 = h2 end);

(* different keys produce their correct hashes *)
val test_psh4 = check'(fn _ =>
    let val ht = Polyhash.mkPolyTable (16, NotFound)
        val (_, h1) = Polyhash.peekSameHash ht "aaa"
        val (_, h2) = Polyhash.peekSameHash ht "bbb"
    in h1 = Polyhash.hash "aaa" andalso h2 = Polyhash.hash "bbb" end);

(* bucket count reflects actual collisions *)
val test_psh5 = check'(fn _ =>
    let val ht = Polyhash.mkPolyTable (16, NotFound)
        val _  = Polyhash.insert ht (1, "a")
        val _  = Polyhash.insert ht (2, "b")
        val _  = Polyhash.insert ht (3, "c")
        val sizes = Polyhash.bucketSizes ht
        val total = List.foldl (op +) 0 sizes
    in total = 3 end);

(* peekSameHash on empty table returns 0 count *)
val test_psh6 = check'(fn _ =>
    let val ht = Polyhash.mkPolyTable (16, NotFound)
        val (n, _) = Polyhash.peekSameHash ht 42
    in n = 0 end);

(* peekSameHash after clear returns 0 *)
val test_psh7 = check'(fn _ =>
    let val ht = Polyhash.mkPolyTable (16, NotFound)
        val _  = Polyhash.insert ht (1, "x")
        val _  = Polyhash.clear ht
        val (n, _) = Polyhash.peekSameHash ht 1
    in n = 0 end);

(* custom hash: all keys collide, bucket should have all items *)
val test_psh8 = check'(fn _ =>
    let fun myHash _ = 7
        val ht = Polyhash.mkTable (myHash, op =) (16, NotFound)
        val _  = Polyhash.insert ht ("a", 1)
        val _  = Polyhash.insert ht ("b", 2)
        val _  = Polyhash.insert ht ("c", 3)
        val (n, h) = Polyhash.peekSameHash ht "x"
    in n = 3 andalso h = 7 end);

val _ = print "All Polyhash tests done.\n";
