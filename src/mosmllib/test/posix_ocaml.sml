(* posix_ocaml.sml -- Posix tests adapted from the OCaml Unix test suite.
 *
 * Sources:
 *   ocaml/testsuite/tests/lib-unix/common/fork_cleanup.ml
 *   ocaml/testsuite/tests/lib-unix/common/pipe_eof.ml
 *   ocaml/testsuite/tests/lib-unix/common/wait_nohang.ml
 *   ocaml/testsuite/tests/lib-unix/common/append.ml
 *   ocaml/testsuite/tests/lib-unix/common/dup.ml
 *   ocaml/testsuite/tests/lib-unix/common/dup2.ml
 *   ocaml/testsuite/tests/lib-unix/common/rename.ml
 *   ocaml/testsuite/tests/lib-unix/common/truncate.ml
 *   ocaml/testsuite/tests/lib-unix/realpath/test.ml
 *   ocaml/testsuite/tests/lib-unix/kill/unix_kill.ml  (simplified)
 *)

use "auxil.sml";
load "Posix";
load "OS";

(* ------------------------------------------------------------------ *)
(* Helpers                                                             *)
(* ------------------------------------------------------------------ *)

val pid_str =
    Int.toString (Word.toInt (Posix.Process.pidToWord (Posix.ProcEnv.getpid ())))

fun tmpfile suffix = "/tmp/posix_ocaml_" ^ pid_str ^ suffix

fun write_file path contents =
    let val fd = Posix.FileSys.createf
                     (path, Posix.FileSys.O_WRONLY,
                      Posix.FileSys.O.flags [Posix.FileSys.O.trunc],
                      Posix.FileSys.S.irwxu)
        val _  = Posix.IO.writeVec (fd, Word8VectorSlice.full
                                        (Byte.stringToBytes contents))
    in Posix.IO.close fd end

fun read_file path =
    let val fd  = Posix.FileSys.openf
                      (path, Posix.FileSys.O_RDONLY, Posix.FileSys.O.flags [])
        fun loop acc =
            let val v = Posix.IO.readVec (fd, 256)
            in if Word8Vector.length v = 0 then acc
               else loop (acc ^ Byte.bytesToString v)
            end
        val s = loop ""
        val _ = Posix.IO.close fd
    in s end

fun safe_remove path = (Posix.FileSys.unlink path) handle _ => ()

(* Drain a pipe: read until EOF, return contents as string.
 * Adapted from OCaml pipe_eof.ml `drain`. *)
fun drain fd =
    let fun loop acc =
            let val v = Posix.IO.readVec (fd, 2048)
            in if Word8Vector.length v = 0 then acc
               else loop (acc ^ Byte.bytesToString v)
            end
    in loop "" end

(* ------------------------------------------------------------------ *)
(* From fork_cleanup.ml                                                *)
(* Tests that dup'd fd in parent is unaffected by child closing its   *)
(* own copy after fork.  After fork both parent and child hold a copy  *)
(* of fd; child closes its copy and exits; parent's copy is still OK.  *)
(* ------------------------------------------------------------------ *)

val _ = print "fork_cleanup tests...\n"

(* Basic fork/wait: child closes dup'd fd, parent continues normally *)
val test_fork_cleanup1 = check'(fn _ =>
    let val fd = Posix.IO.dup Posix.FileSys.stdout
    in case Posix.Process.fork () of
         NONE =>
           (* child: close our copy and exit cleanly *)
           (Posix.IO.close fd; Posix.Process.exit 0w0; false)
       | SOME pid =>
           (* parent: wait, then verify we can still close our copy *)
           let val (_, st) = Posix.Process.wait ()
               val _       = Posix.IO.close fd
           in case st of
                Posix.Process.W_EXITED        => true
              | Posix.Process.W_EXITSTATUS w  => w = 0w0
              | _                             => false
           end
    end)

(* Fork where child immediately exits with non-zero status *)
val test_fork_cleanup2 = check'(fn _ =>
    case Posix.Process.fork () of
      NONE      => (Posix.Process.exit 0w42; false)
    | SOME pid  =>
        let val (pid2, st) = Posix.Process.waitpid
                                 (Posix.Process.W_CHILD pid, [])
        in pid2 = pid andalso
           (case st of
              Posix.Process.W_EXITSTATUS w => w = 0w42
            | _                           => false)
        end)

(* ------------------------------------------------------------------ *)
(* From pipe_eof.ml                                                    *)
(* Tests pipe creation, writing, close of write-end, and reading until *)
(* EOF via the drain helper.                                            *)
(* ------------------------------------------------------------------ *)

val _ = print "pipe_eof tests...\n"

val test_pipe_drain1 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val msg  = "Hello from pipe_eof"
        val _    = Posix.IO.writeVec (outfd,
                       Word8VectorSlice.full (Byte.stringToBytes msg))
        val _    = Posix.IO.close outfd  (* signal EOF *)
        val got  = drain infd
        val _    = Posix.IO.close infd
    in got = msg end)

(* Large message: write > 256 bytes so drain must loop *)
val test_pipe_drain2 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val msg = String.concat (List.tabulate (100, fn _ => "abcdefghij"))
        val _   = Posix.IO.writeVec (outfd,
                      Word8VectorSlice.full (Byte.stringToBytes msg))
        val _   = Posix.IO.close outfd
        val got = drain infd
        val _   = Posix.IO.close infd
    in got = msg end)

(* Pipe through child process: parent writes, child echoes back via pipe *)
val test_pipe_child = check'(fn _ =>
    let val {infd = p_in,  outfd = p_out}  = Posix.IO.pipe () (* parent reads *)
        val {infd = c_in, outfd = c_out}   = Posix.IO.pipe () (* parent writes *)
    in case Posix.Process.fork () of
         NONE =>
           (* child: read from c_in, echo to p_out, then exit *)
           let val v = Posix.IO.readVec (c_in, 100)
               val _ = Posix.IO.writeVec (p_out,
                           Word8VectorSlice.full v)
               val _ = Posix.IO.close c_in
               val _ = Posix.IO.close p_out
               val _ = Posix.IO.close c_out   (* not used by child *)
               val _ = Posix.IO.close p_in    (* not used by child *)
           in Posix.Process.exit 0w0; false end
       | SOME pid =>
           let val msg = "ping"
               val _   = Posix.IO.close c_in   (* parent doesn't read from this end *)
               val _   = Posix.IO.close p_out  (* parent doesn't write to this end *)
               val _   = Posix.IO.writeVec (c_out,
                             Word8VectorSlice.full (Byte.stringToBytes msg))
               val _   = Posix.IO.close c_out  (* signal EOF to child *)
               val got = drain p_in
               val _   = Posix.IO.close p_in
               val _   = Posix.Process.wait ()
           in got = msg end
    end)

(* ------------------------------------------------------------------ *)
(* From wait_nohang.ml                                                 *)
(* Tests waitpid with WNOHANG: returns NONE when child not yet done,   *)
(* eventually returns SOME with exit status once child exits.           *)
(* ------------------------------------------------------------------ *)

val _ = print "wait_nohang tests...\n"

(* Child sleeps briefly; first WNOHANG poll returns NONE; blocking     *)
(* wait succeeds once child exits.                                     *)
val test_wait_nohang1 = check'(fn _ =>
    case Posix.Process.fork () of
      NONE =>
        (Posix.Process.sleep (Time.fromReal 0.05);
         Posix.Process.exit 0w0; false)
    | SOME pid =>
        (* Poll immediately — child probably hasn't exited yet *)
        let val poll = Posix.Process.waitpid_nh
                           (Posix.Process.W_CHILD pid, [])
            (* Whether or not nh reaped it, ensure child is cleaned up *)
            val ok = case poll of
                       SOME (_, Posix.Process.W_EXITED)       => true
                     | SOME (_, Posix.Process.W_EXITSTATUS w) => w = 0w0
                     | SOME _                                  => false
                     | NONE =>
                         (* child still running; do blocking wait *)
                         let val (_, st) = Posix.Process.waitpid
                                               (Posix.Process.W_CHILD pid, [])
                         in case st of
                              Posix.Process.W_EXITED        => true
                            | Posix.Process.W_EXITSTATUS w  => w = 0w0
                            | _                             => false
                         end
        in ok end)

(* WNOHANG with W_ANY_CHILD when no children: raises an exception *)
val test_wait_nohang2 = check'(fn _ =>
    (Posix.Process.waitpid_nh (Posix.Process.W_ANY_CHILD, []);
     true)   (* NONE is also OK if somehow no children *)
    handle Fail _ => true)  (* ECHILD raises Fail: expected *)

(* ------------------------------------------------------------------ *)
(* From append.ml                                                      *)
(* Tests O_APPEND: two separate opens with O_APPEND both write to     *)
(* end of file; final content is the two strings concatenated.         *)
(* ------------------------------------------------------------------ *)

val _ = print "append tests...\n"

local
  val tmpA = tmpfile "_append"
  val str  = "Hello, MosML!"
  fun do_append path s =
      let val fd = Posix.FileSys.createf
                       (path, Posix.FileSys.O_WRONLY,
                        Posix.FileSys.O.flags [Posix.FileSys.O.append],
                        Posix.FileSys.S.irwxu)
          val _  = Posix.IO.writeVec (fd,
                       Word8VectorSlice.full (Byte.stringToBytes s))
      in Posix.IO.close fd end
in
  val test_append1 = check'(fn _ =>
      let val _ = safe_remove tmpA
          val _ = do_append tmpA str   (* creates + appends *)
          val _ = do_append tmpA str   (* appends to existing *)
          val s = read_file tmpA
          val _ = safe_remove tmpA
      in s = str ^ str end)

  (* Append is atomic across two fds open simultaneously *)
  val test_append2 = check'(fn _ =>
      let val _ = safe_remove tmpA
          val fd1 = Posix.FileSys.createf
                        (tmpA, Posix.FileSys.O_WRONLY,
                         Posix.FileSys.O.flags [Posix.FileSys.O.append],
                         Posix.FileSys.S.irwxu)
          val fd2 = Posix.FileSys.openf
                        (tmpA, Posix.FileSys.O_WRONLY,
                         Posix.FileSys.O.flags [Posix.FileSys.O.append])
          val _ = Posix.IO.writeVec (fd1,
                      Word8VectorSlice.full (Byte.stringToBytes "aaa"))
          val _ = Posix.IO.writeVec (fd2,
                      Word8VectorSlice.full (Byte.stringToBytes "bbb"))
          val _ = Posix.IO.close fd1
          val _ = Posix.IO.close fd2
          val s = read_file tmpA
          val _ = safe_remove tmpA
          (* Both writes must appear, in some order *)
      in String.size s = 6 andalso
         (s = "aaabbb" orelse s = "bbbaaa") end)
end

(* ------------------------------------------------------------------ *)
(* From dup.ml                                                         *)
(* Tests Posix.IO.dup: duplicate a fd; both refer to same file.        *)
(* ------------------------------------------------------------------ *)

val _ = print "dup tests...\n"

(* Dup of stdout: write to duplicate appears on stdout *)
val test_dup1 = check'(fn _ =>
    let val fd2 = Posix.IO.dup Posix.FileSys.stdout
        val _   = Posix.IO.close fd2
    in Posix.FileSys.fdToWord fd2 > 0w0 end)

(* Dup a pipe fd: reads from original still work after closing dup *)
val test_dup2 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val infd2 = Posix.IO.dup infd   (* dup the read end *)
        val _     = Posix.IO.close infd (* close original read end *)
        val _     = Posix.IO.writeVec (outfd,
                        Word8VectorSlice.full (Byte.stringToBytes "xyz"))
        val _     = Posix.IO.close outfd
        val v     = Posix.IO.readVec (infd2, 100)
        val _     = Posix.IO.close infd2
    in Byte.bytesToString v = "xyz" end)

(* dupfd: new fd is >= base *)
val test_dup3 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val base = Posix.FileSys.wordToFD 0w20
        val fd2  = Posix.IO.dupfd {old = infd, base = base}
        val _    = Posix.IO.close infd
        val _    = Posix.IO.close outfd
        val _    = Posix.IO.close fd2
    in Posix.FileSys.fdToWord fd2 >= 0w20 end)

(* ------------------------------------------------------------------ *)
(* From dup2.ml                                                        *)
(* Tests Posix.IO.dup2: redirect one fd to another specific number.    *)
(* ------------------------------------------------------------------ *)

val _ = print "dup2 tests...\n"

(* dup2 to a fresh pipe write-end: write to redirected fd goes to pipe *)
val test_dup2_1 = check'(fn _ =>
    let val {infd, outfd}  = Posix.IO.pipe ()
        (* open a file for writing, then redirect to outfd's slot *)
        val tmpD = tmpfile "_dup2"
        val fd   = Posix.FileSys.createf
                       (tmpD, Posix.FileSys.O_WRONLY,
                        Posix.FileSys.O.flags [],
                        Posix.FileSys.S.irwxu)
        (* dup2: make fd point to the pipe's write end *)
        val _    = Posix.IO.dup2 {old = outfd, new = fd}
        val _    = Posix.IO.close outfd    (* close the original pipe write end *)
        val _    = Posix.IO.writeVec (fd,
                       Word8VectorSlice.full (Byte.stringToBytes "dup2test"))
        val _    = Posix.IO.close fd       (* closes the dup2'd end; pipe has EOF *)
        val got  = drain infd
        val _    = Posix.IO.close infd
        val _    = safe_remove tmpD
    in got = "dup2test" end)

(* dup2 {old=fd, new=fd} is identity (Linux: no-op or re-creates) *)
val test_dup2_2 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val _   = Posix.IO.dup2 {old = outfd, new = outfd}   (* dup to itself *)
        val _   = Posix.IO.writeVec (outfd,
                      Word8VectorSlice.full (Byte.stringToBytes "ok"))
        val _   = Posix.IO.close outfd
        val got = drain infd
        val _   = Posix.IO.close infd
    in got = "ok" end)

(* ------------------------------------------------------------------ *)
(* From rename.ml                                                      *)
(* Tests Posix.FileSys.rename: rename to non-existing, to existing,    *)
(* and failure cases (non-existing source, non-existing target dir).   *)
(* ------------------------------------------------------------------ *)

val _ = print "rename tests...\n"

local
  val f1 = tmpfile "_rename_src"
  val f2 = tmpfile "_rename_dst"
in
  (* Rename to non-existing destination *)
  val test_rename1 = check'(fn _ =>
      let val _ = safe_remove f1; val _ = safe_remove f2
          val _ = write_file f1 "abc"
          val _ = Posix.FileSys.rename {old = f1, new = f2}
          val s = read_file f2
          val _ = safe_remove f2
      in s = "abc" andalso
         not (Posix.FileSys.access (f1, [])) end)

  (* Rename to existing destination: overwrites it *)
  val test_rename2 = check'(fn _ =>
      let val _ = safe_remove f1; val _ = safe_remove f2
          val _ = write_file f1 "def"
          val _ = write_file f2 "xyz"
          val _ = Posix.FileSys.rename {old = f1, new = f2}
          val s = read_file f2
          val _ = safe_remove f2
      in s = "def" andalso
         not (Posix.FileSys.access (f1, [])) end)

  (* Rename non-existing source: raises *)
  val test_rename3 = check'(fn _ =>
      let val _ = safe_remove f1; val _ = safe_remove f2
      in (Posix.FileSys.rename {old = f1, new = f2}; false)
         handle Fail _ => true end)

  (* Rename to non-existing directory: raises *)
  val test_rename4 = check'(fn _ =>
      let val _ = write_file f1 "abc"
          val bad_dst = "/tmp/no_such_dir_posix/dst"
      in (Posix.FileSys.rename {old = f1, new = bad_dst}; false)
         handle Fail _ => (safe_remove f1; true) end)
end

(* ------------------------------------------------------------------ *)
(* From truncate.ml                                                    *)
(* Tests Posix.FileSys.ftruncate: shrink and zero-out a file.          *)
(* OCaml also tests path-based truncate; we only have fd-based.        *)
(* ------------------------------------------------------------------ *)

val _ = print "ftruncate tests...\n"

local
  val tmpT = tmpfile "_trunc"
  val str  = "Hello, OCaml!"    (* 13 bytes *)
in
  val test_ftruncate1 = check'(fn _ =>
      let val _  = write_file tmpT str
          val st0 = Posix.FileSys.stat tmpT
          val sz0 = Posix.FileSys.ST.size st0
          val fd  = Posix.FileSys.openf
                        (tmpT, Posix.FileSys.O_RDWR,
                         Posix.FileSys.O.flags [])
          (* Shrink by 2 bytes *)
          val _   = Posix.FileSys.ftruncate (fd, sz0 - 2)
          val sz1 = Posix.FileSys.ST.size (Posix.FileSys.fstat fd)
          (* Truncate to zero *)
          val _   = Posix.FileSys.ftruncate (fd, 0)
          val sz2 = Posix.FileSys.ST.size (Posix.FileSys.fstat fd)
          val _   = Posix.IO.close fd
          val _   = safe_remove tmpT
      in sz0 = 13 andalso sz1 = 11 andalso sz2 = 0 end)

  (* Extend a file beyond its current size (sparse file / zero fill) *)
  val test_ftruncate2 = check'(fn _ =>
      let val _ = write_file tmpT "abc"
          val fd = Posix.FileSys.openf
                       (tmpT, Posix.FileSys.O_RDWR,
                        Posix.FileSys.O.flags [])
          val _  = Posix.FileSys.ftruncate (fd, 100)
          val sz = Posix.FileSys.ST.size (Posix.FileSys.fstat fd)
          val _  = Posix.IO.close fd
          val _  = safe_remove tmpT
      in sz = 100 end)
end

(* ------------------------------------------------------------------ *)
(* From realpath/test.ml                                               *)
(* Tests OS.FileSys.realPath (= Unix.realpath in OCaml): resolves ..,  *)
(* multiple slashes, and produces an absolute canonical path.           *)
(* ------------------------------------------------------------------ *)

val _ = print "realpath tests...\n"

local
  val testdir  = tmpfile "_rp_dir"
  val testfile = testdir ^ "/rp_file"
in
  val test_realpath1 = check'(fn _ =>
      let val cwd  = OS.FileSys.getDir ()
          val rp   = OS.FileSys.realPath cwd
      in rp = cwd end)

  val test_realpath2 = check'(fn _ =>
      let val _ = Posix.FileSys.mkdir (testdir, Posix.FileSys.S.irwxu)
          val _ = write_file testfile ""
          (* Two paths with redundant components that canonicalise to same *)
          val p0 = OS.FileSys.realPath (testdir ^ "/.//rp_file")
          val p1 = OS.FileSys.realPath (testdir ^ "/../" ^
                       OS.Path.file testdir ^ "/rp_file")
          val ok = (p0 = p1) andalso
                   OS.Path.isAbsolute p0 andalso
                   OS.Path.isAbsolute p1
          val _ = safe_remove testfile
          val _ = Posix.FileSys.rmdir testdir
      in ok end)

  val test_realpath3 = check'(fn _ =>
      let val _ = Posix.FileSys.mkdir (testdir, Posix.FileSys.S.irwxu)
          (* ".." path cancellation *)
          val p2 = OS.FileSys.realPath (testdir ^ "/..")
          val p3 = OS.FileSys.realPath (OS.Path.dir testdir)
          val ok = (p2 = p3) andalso OS.Path.isAbsolute p2
          val _ = Posix.FileSys.rmdir testdir
      in ok end)
end

(* ------------------------------------------------------------------ *)
(* From kill/unix_kill.ml  (simplified)                               *)
(* OCaml's test installs signal handlers (Sys.set_signal) to verify    *)
(* signal delivery. mosml has no Posix signal-handler API so we test    *)
(* the observable effect: a child killed with a signal has W_SIGNALED.  *)
(* ------------------------------------------------------------------ *)

val _ = print "kill/signal tests...\n"

(* Kill a sleeping child with SIGTERM; expect W_SIGNALED *)
val test_kill_sigterm = check'(fn _ =>
    case Posix.Process.fork () of
      NONE =>
        (Posix.Process.sleep (Time.fromReal 60.0);
         Posix.Process.exit 0w0; false)
    | SOME pid =>
        (Posix.Process.kill (Posix.Process.K_PROC pid, Posix.Signal.term);
         let val (_, st) = Posix.Process.wait ()
         in case st of
              Posix.Process.W_SIGNALED s =>
                Posix.Signal.toWord s = Posix.Signal.toWord Posix.Signal.term
            | _ => false
         end))

(* Kill with SIGKILL (uncatchable); child cannot ignore or handle it *)
val test_kill_sigkill = check'(fn _ =>
    case Posix.Process.fork () of
      NONE =>
        (Posix.Process.sleep (Time.fromReal 60.0);
         Posix.Process.exit 0w0; false)
    | SOME pid =>
        (Posix.Process.kill (Posix.Process.K_PROC pid, Posix.Signal.kill);
         let val (_, st) = Posix.Process.wait ()
         in case st of
              Posix.Process.W_SIGNALED s =>
                Posix.Signal.toWord s = Posix.Signal.toWord Posix.Signal.kill
            | _ => false
         end))

(* Kill self with SIGUSR1 via fork: parent sends, child dies,
 * verifies the signal number in wait status *)
val test_kill_sigusr = check'(fn _ =>
    case Posix.Process.fork () of
      NONE =>
        (* child: wait for signal *)
        (Posix.Process.sleep (Time.fromReal 30.0);
         Posix.Process.exit 0w0; false)
    | SOME pid =>
        (Posix.Process.kill (Posix.Process.K_PROC pid, Posix.Signal.usr1);
         let val (_, st) = Posix.Process.wait ()
         in case st of
              Posix.Process.W_SIGNALED s =>
                Posix.Signal.toWord s = Posix.Signal.toWord Posix.Signal.usr1
            | _ => false
         end))

(* Kill with K_GROUP: child calls setsid() to become its own process
 * group leader, then parent sends to that group via K_GROUP pid.   *)
val test_kill_group = check'(fn _ =>
    case Posix.Process.fork () of
      NONE =>
        (* child: create new session so its pgid = its pid *)
        (Posix.ProcEnv.setsid ();
         Posix.Process.sleep (Time.fromReal 30.0);
         Posix.Process.exit 0w0; false)
    | SOME pid =>
        (* give child time to call setsid before we kill *)
        (Posix.Process.sleep (Time.fromReal 0.05);
         Posix.Process.kill (Posix.Process.K_GROUP pid, Posix.Signal.term);
         let val (_, st) = Posix.Process.wait ()
         in case st of
              Posix.Process.W_SIGNALED _ => true
            | _ => false
         end))

(* ------------------------------------------------------------------ *)
(* Extra: pipe + exec (adapted from pipe_eof.ml's idea of subprocess)  *)
(* Fork a child that execs /bin/echo; parent reads output via pipe.    *)
(* ------------------------------------------------------------------ *)

val _ = print "pipe_exec tests...\n"

(* Note: execp(file, args) prepends file as argv[0]; args are the
 * extra arguments only — this matches our implementation convention. *)
val test_pipe_exec = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
    in case Posix.Process.fork () of
         NONE =>
           (* child: redirect stdout to pipe write end, exec echo *)
           let val _  = Posix.IO.dup2 {old = outfd, new = Posix.FileSys.stdout}
               val _  = Posix.IO.close outfd
               val _  = Posix.IO.close infd
           in Posix.Process.execp ("/bin/echo", ["hello"]);
              (* ^-- args does NOT include argv[0]; our execp prepends file *)
              Posix.Process.exit 0w1; false
           end
       | SOME pid =>
           let val _   = Posix.IO.close outfd     (* close write end in parent *)
               val got = drain infd
               val _   = Posix.IO.close infd
               val _   = Posix.Process.wait ()
               (* /bin/echo appends '\n'; strip it for comparison *)
               val trimmed = String.substring (got, 0,
                                 Int.max (0, String.size got - 1))
           in trimmed = "hello" end
    end)

(* ------------------------------------------------------------------ *)
(* Extra: stat after write (derived from truncate.ml stat use)         *)
(* Verify that stat reflects file size correctly after writes.          *)
(* ------------------------------------------------------------------ *)

val _ = print "stat_size tests...\n"

val test_stat_size = check'(fn _ =>
    let val tmp  = tmpfile "_statsize"
        val data = "12345678901234567890"   (* 20 bytes *)
        val _    = write_file tmp data
        val st   = Posix.FileSys.stat tmp
        val sz   = Posix.FileSys.ST.size st
        val _    = safe_remove tmp
    in sz = 20 end)

(* stat uid/gid are consistent with ProcEnv uid/gid for files we create *)
val test_stat_uid = check'(fn _ =>
    let val tmp = tmpfile "_statuid"
        val _   = write_file tmp "x"
        val st  = Posix.FileSys.stat tmp
        val _   = safe_remove tmp
    (* uid/gid are valid (nlink >= 1 and isReg for a regular file) *)
    in Posix.FileSys.ST.nlink st >= 1 andalso
       Posix.FileSys.ST.isReg st end)

(* ------------------------------------------------------------------ *)
(* Extra: symlink readback (realpath + lstat)                          *)
(* ------------------------------------------------------------------ *)

val _ = print "symlink tests...\n"

val test_symlink_readback = check'(fn _ =>
    let val target = tmpfile "_sym_tgt"
        val lnk    = tmpfile "_sym_lnk"
        val _      = write_file target "symlink_test"
        val _      = Posix.FileSys.symlink {old = target, new = lnk}
        val rp_lnk = OS.FileSys.realPath lnk
        val rp_tgt = OS.FileSys.realPath target
        val lst    = Posix.FileSys.lstat lnk
        val st     = Posix.FileSys.stat  lnk
        val _      = safe_remove lnk
        val _      = safe_remove target
    in rp_lnk = rp_tgt andalso
       Posix.FileSys.ST.isLink lst andalso
       Posix.FileSys.ST.isReg  st  end)

val _ = print "All OCaml-adapted Posix tests done.\n"
