(* test/unix.sml -- test Unix.fork, Unix.waitpid, Unix.getpid, Unix.exit
   Ported from OCaml testsuite/tests/lib-unix/common/fork_cleanup.ml
   and testsuite/tests/lib-systhreads/testfork.ml (non-threaded parts)
*)

use "auxil.sml";

load "Unix";
load "Int";

local
    open Unix
in

(* Test 1: basic fork, child exits, parent waits *)

val test1 = check'(fn _ =>
    case fork () of
	NONE   => (exit 0; false)
      | SOME pid =>
	let val st = waitpid pid
	in st = 0 end);

(* Test 2: child state is isolated from parent *)

val shared = ref 42

val test2 = check'(fn _ =>
    case fork () of
	NONE   => (shared := 99; exit 0; false)
      | SOME pid =>
	let val _ = waitpid pid
	in !shared = 42 end);

(* Test 3: parent reads child exit code *)

val test3 = check'(fn _ =>
    case fork () of
	NONE   => (exit 7; false)
      | SOME pid => waitpid pid = 7);

(* Test 4: multiple forks *)

val test4 = check'(fn _ =>
    let val pids = List.tabulate(5, fn i =>
	    case fork () of
		NONE   => (exit i; 0)
	      | SOME pid => pid)
	val sts = List.map waitpid pids
    in sts = [0, 1, 2, 3, 4] end);

(* Test 5: getpid differs between parent and child *)

val test5 = check'(fn _ =>
    let val ppid = getpid ()
    in case fork () of
	NONE =>
	  (if getpid () <> ppid then exit 0 else exit 1; false)
      | SOME pid => waitpid pid = 0
    end);

(* Test 6: child can do file I/O *)

val test6 = check'(fn _ =>
    let val tmp = "/tmp/mosml_test_fork_" ^ Int.toString (getpid ())
    in case fork () of
	NONE =>
	  let val os = TextIO.openOut tmp
	  in TextIO.output(os, "hello from child");
	     TextIO.closeOut os;
	     exit 0; false
	  end
      | SOME pid =>
	  let val _ = waitpid pid
	      val is = TextIO.openIn tmp
	      val s = TextIO.inputAll is
	  in TextIO.closeIn is;
	     OS.FileSys.remove tmp;
	     s = "hello from child"
	  end
    end);

(* Test 7: GC survives in child process *)

val test7 = check'(fn _ =>
    case fork () of
	NONE =>
	  let fun alloc 0 = ()
		| alloc n = (ignore (List.tabulate(100, fn i => ref i));
			     alloc (n - 1))
	  in alloc 100; exit 0; false end
      | SOME pid => waitpid pid = 0);

(* Test 8: kill_pid SIGTERM terminates a spinning child *)

val test8 = check'(fn _ =>
    case fork () of
	NONE    => (let fun spin () = spin () in spin (); false end)
      | SOME pid =>
	  (kill_pid pid 15;      (* SIGTERM *)
	   waitpid pid <> 0));

(* Test 9: kill_pid SIGKILL terminates a spinning child *)

val test9 = check'(fn _ =>
    case fork () of
	NONE    => (let fun spin () = spin () in spin (); false end)
      | SOME pid =>
	  (kill_pid pid 9;       (* SIGKILL *)
	   waitpid pid <> 0));

(* Test 10: kill_pid on a reaped pid raises Fail (ESRCH) *)

val test10 = check'(fn _ =>
    let val pid = case fork () of
		      NONE   => (exit 0; 0)
		    | SOME p => p
	val _ = waitpid pid
    in (kill_pid pid 15; false)
       handle Fail _ => true
    end);

end
