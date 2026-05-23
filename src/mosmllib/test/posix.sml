(* test/posix.sml -- tests for the Posix module
   Covers: Signal, Process, FileSys, IO, ProcEnv, SysDB, Error
   Adapted from MLKit test suite and MLton posix tests. *)

use "auxil.sml";

load "Posix";
load "OS";

(* ------------------------------------------------------------------ *)
(* Signal                                                              *)
(* ------------------------------------------------------------------ *)

val _ = print "Signal tests...\n"

(* Signal constants are non-zero words *)
val test_sig1 = check'(fn _ => Posix.Signal.toWord Posix.Signal.kill <> 0w0)
val test_sig2 = check'(fn _ => Posix.Signal.toWord Posix.Signal.term <> 0w0)
val test_sig3 = check'(fn _ => Posix.Signal.toWord Posix.Signal.hup  <> 0w0)

(* toWord/fromWord round-trip *)
val test_sig4 = check'(fn _ =>
    Posix.Signal.fromWord (Posix.Signal.toWord Posix.Signal.term)
    = Posix.Signal.term)

val test_sig5 = check'(fn _ =>
    Posix.Signal.toWord (Posix.Signal.fromWord 0w9) = 0w9)

(* Different signals have different values *)
val test_sig6 = check'(fn _ =>
    Posix.Signal.toWord Posix.Signal.kill <> Posix.Signal.toWord Posix.Signal.term andalso
    Posix.Signal.toWord Posix.Signal.int  <> Posix.Signal.toWord Posix.Signal.hup)

(* ------------------------------------------------------------------ *)
(* Error                                                               *)
(* ------------------------------------------------------------------ *)

val _ = print "Error tests...\n"

val test_err1 = check'(fn _ => Posix.Error.noent > 0)
val test_err2 = check'(fn _ => Posix.Error.acces > 0)
val test_err3 = check'(fn _ => Posix.Error.perm  > 0)

(* errorMsg returns a non-empty string *)
val test_err4 = check'(fn _ =>
    String.size (Posix.Error.errorMsg Posix.Error.noent) > 0)

(* toWord/fromWord round-trip *)
val test_err5 = check'(fn _ =>
    Posix.Error.fromWord (Posix.Error.toWord Posix.Error.noent)
    = Posix.Error.noent)

(* ------------------------------------------------------------------ *)
(* ProcEnv                                                             *)
(* ------------------------------------------------------------------ *)

val _ = print "ProcEnv tests...\n"

(* getpid returns positive pid *)
val test_penv1 = check'(fn _ =>
    Posix.Process.pidToWord (Posix.ProcEnv.getpid ()) > 0w0)

(* getppid returns positive pid *)
val test_penv2 = check'(fn _ =>
    Posix.Process.pidToWord (Posix.ProcEnv.getppid ()) > 0w0)

(* getpid != getppid in normal context *)
val test_penv3 = check'(fn _ =>
    Posix.ProcEnv.getpid () <> Posix.ProcEnv.getppid ())

(* uid/gid are non-negative *)
val test_penv4 = check'(fn _ =>
    Posix.ProcEnv.uidToWord (Posix.ProcEnv.getuid ()) >= 0w0)
val test_penv5 = check'(fn _ =>
    Posix.ProcEnv.gidToWord (Posix.ProcEnv.getgid ()) >= 0w0)

(* wordToUid/uidToWord round-trip *)
val test_penv6 = check'(fn _ =>
    let val uid = Posix.ProcEnv.getuid ()
    in Posix.ProcEnv.wordToUid (Posix.ProcEnv.uidToWord uid) = uid end)

(* uname returns list with at least sysname and machine *)
val test_penv7 = check'(fn _ =>
    let val info = Posix.ProcEnv.uname ()
        fun has key = List.exists (fn (k,_) => k = key) info
    in has "sysname" andalso has "machine" end)

(* times returns record with non-negative fields *)
val test_penv8 = check'(fn _ =>
    let val t = Posix.ProcEnv.times ()
    in not (Time.< (#elapsed t, Time.zeroTime))
       andalso not (Time.< (#utime t, Time.zeroTime))
       andalso not (Time.< (#cstime t, Time.zeroTime)) end)

(* getenv: PATH should exist *)
val test_penv9 = check'(fn _ =>
    case Posix.ProcEnv.getenv "PATH" of
      SOME p => String.size p > 0
    | NONE => false)

(* getenv: missing key returns NONE *)
val test_penv10 = check'(fn _ =>
    Posix.ProcEnv.getenv "POSIX_TEST_MISSING_XYZ_12345" = NONE)

(* environ returns a list (may be empty in restricted environments) *)
val test_penv11 = check'(fn _ =>
    (Posix.ProcEnv.environ (); true))

(* sysconf CLK_TCK returns positive value *)
val test_penv12 = check'(fn _ =>
    Posix.ProcEnv.sysconf "CLK_TCK" > 0w0)

(* sysconf OPEN_MAX returns positive value *)
val test_penv13 = check'(fn _ =>
    Posix.ProcEnv.sysconf "OPEN_MAX" > 0w0)

(* ctermid: returns a string (usually /dev/tty or empty) *)
val test_penv14 = check'(fn _ =>
    (Posix.ProcEnv.ctermid (); true))

(* ------------------------------------------------------------------ *)
(* Process                                                             *)
(* ------------------------------------------------------------------ *)

val _ = print "Process tests...\n"

(* pidToWord/wordToPid round-trip *)
val test_proc1 = check'(fn _ =>
    let val pid = Posix.ProcEnv.getpid ()
        val w   = Posix.Process.pidToWord pid
    in Posix.Process.wordToPid w = pid end)

(* fork/wait: child exits 0, parent gets W_EXITED *)
val test_proc2 = check'(fn _ =>
    case Posix.Process.fork () of
      NONE =>
        (Posix.Process.exit 0w0; false) (* child *)
    | SOME pid =>
        let val (_, st) = Posix.Process.wait ()
        in case st of
             Posix.Process.W_EXITED => true
           | Posix.Process.W_EXITSTATUS w => (w = 0w0)
           | _ => false
        end)

(* fork/wait: child exits with non-zero status *)
val test_proc3 = check'(fn _ =>
    case Posix.Process.fork () of
      NONE =>
        (Posix.Process.exit 0w42; false)
    | SOME pid =>
        let val (_, st) = Posix.Process.wait ()
        in case st of
             Posix.Process.W_EXITSTATUS w => (w = 0w42)
           | _ => false
        end)

(* fork/waitpid W_CHILD: collect specific child *)
val test_proc4 = check'(fn _ =>
    case Posix.Process.fork () of
      NONE =>
        (Posix.Process.exit 0w7; false)
    | SOME pid =>
        let val (pid2, st) = Posix.Process.waitpid
                                 (Posix.Process.W_CHILD pid, [])
        in pid2 = pid andalso
           (case st of
              Posix.Process.W_EXITSTATUS w => w = 0w7
            | _ => false)
        end)

(* fork/waitpid_nh: poll then collect — handle case where child already reaped *)
val test_proc5 = check'(fn _ =>
    case Posix.Process.fork () of
      NONE =>
        (Posix.Process.sleep (Time.fromReal 0.05);
         Posix.Process.exit 0w0; false)
    | SOME pid =>
        (* Poll with WNOHANG; if child already exited it's reaped here;
           otherwise do a blocking wait *)
        let val collected =
            case Posix.Process.waitpid_nh (Posix.Process.W_CHILD pid, []) of
              SOME (pid2, _) => pid2 = pid   (* reaped by nh *)
            | NONE =>                         (* child still running; block *)
                let val (pid2, _) = Posix.Process.waitpid
                                        (Posix.Process.W_CHILD pid, [])
                in pid2 = pid end
        in collected end)

(* fork/execp: run /bin/true *)
val test_proc6 = check'(fn _ =>
    case Posix.Process.fork () of
      NONE =>
        (Posix.Process.execp ("/bin/true", ["true"]);
         Posix.Process.exit 0w1; false) (* shouldn't reach here *)
    | SOME pid =>
        let val (_, st) = Posix.Process.wait ()
        in case st of
             Posix.Process.W_EXITED => true
           | Posix.Process.W_EXITSTATUS w => (w = 0w0)
           | _ => false
        end)

(* fork/exec: run /bin/false *)
val test_proc7 = check'(fn _ =>
    case Posix.Process.fork () of
      NONE =>
        (Posix.Process.exec ("/bin/false", ["false"]);
         Posix.Process.exit 0w0; false)
    | SOME pid =>
        let val (_, st) = Posix.Process.wait ()
        in case st of
             Posix.Process.W_EXITED => false     (* /bin/false exits non-zero *)
           | Posix.Process.W_EXITSTATUS _ => true (* any non-zero status OK *)
           | _ => false
        end)

(* fork/kill: send SIGTERM to child, child gets W_SIGNALED *)
val test_proc8 = check'(fn _ =>
    case Posix.Process.fork () of
      NONE =>
        (* child sleeps until killed *)
        (Posix.Process.sleep (Time.fromReal 60.0);
         Posix.Process.exit 0w0; false)
    | SOME pid =>
        (Posix.Process.kill (Posix.Process.K_PROC pid, Posix.Signal.term);
         let val (_, st) = Posix.Process.wait ()
         in case st of
              Posix.Process.W_SIGNALED _ => true
            | _ => false
         end))

(* alarm: returns remaining time (0 if no prior alarm) *)
val test_proc9 = check'(fn _ =>
    let val rem = Posix.Process.alarm (Time.fromReal 60.0)
        val _   = Posix.Process.alarm Time.zeroTime (* cancel *)
    in rem = Time.zeroTime end)

(* sleep: sleeps and returns remaining time *)
val test_proc10 = check'(fn _ =>
    let val rem = Posix.Process.sleep (Time.fromReal 0.0)
    in rem = Time.zeroTime end)

(* fromStatus *)
val test_proc11 = check'(fn _ =>
    case Posix.Process.fromStatus (OS.Process.success) of
      Posix.Process.W_EXITED => true
    | _                      => false)

(* ------------------------------------------------------------------ *)
(* FileSys                                                             *)
(* ------------------------------------------------------------------ *)

val _ = print "FileSys tests...\n"

(* stdin/stdout/stderr file descriptors *)
val test_fs1 = check'(fn _ =>
    Posix.FileSys.fdToWord Posix.FileSys.stdin  = 0w0 andalso
    Posix.FileSys.fdToWord Posix.FileSys.stdout = 0w1 andalso
    Posix.FileSys.fdToWord Posix.FileSys.stderr = 0w2)

(* wordToFD/fdToWord round-trip *)
val test_fs2 = check'(fn _ =>
    Posix.FileSys.fdToWord
        (Posix.FileSys.wordToFD 0w3) = 0w3)

(* S mode constants are sensible *)
val test_fs3 = check'(fn _ =>
    (* irwxu subsumes irusr (allSet checks subset) *)
    Posix.FileSys.S.allSet (Posix.FileSys.S.irwxu, Posix.FileSys.S.irusr))

(* S.flags combines modes *)
val test_fs4 = check'(fn _ =>
    (* flags combines; allSet checks subset membership *)
    let val combined = Posix.FileSys.S.flags
                           [Posix.FileSys.S.irusr, Posix.FileSys.S.iwusr]
    in Posix.FileSys.S.allSet (combined, Posix.FileSys.S.irusr) andalso
       Posix.FileSys.S.allSet (combined, Posix.FileSys.S.iwusr) end)

(* O.flags *)
val test_fs5 = check'(fn _ =>
    (* empty flags is a unit of the flags algebra; anySet of empty is false *)
    let val empty = Posix.FileSys.O.flags []
    in not (Posix.FileSys.O.anySet (empty, Posix.FileSys.O.append)) andalso
       Posix.FileSys.O.toWord Posix.FileSys.O.append > 0w0 end)

(* getcwd returns non-empty string *)
val test_fs6 = check'(fn _ =>
    String.size (Posix.FileSys.getcwd ()) > 0)

(* stat on current directory *)
val test_fs7 = check'(fn _ =>
    let val st = Posix.FileSys.stat (Posix.FileSys.getcwd ())
    in Posix.FileSys.ST.isDir st end)

(* stat: /tmp is a directory *)
val test_fs8 = check'(fn _ =>
    Posix.FileSys.ST.isDir (Posix.FileSys.stat "/tmp"))

(* lstat: /tmp is also a directory (or symlink to one) *)
val test_fs9 = check'(fn _ =>
    let val st = Posix.FileSys.lstat "/tmp"
    in Posix.FileSys.ST.isDir st orelse Posix.FileSys.ST.isLink st end)

(* stat: nlink >= 1 *)
val test_fs10 = check'(fn _ =>
    Posix.FileSys.ST.nlink (Posix.FileSys.stat "/tmp") >= 1)

(* stat: size >= 0 *)
val test_fs11 = check'(fn _ =>
    Posix.FileSys.ST.size (Posix.FileSys.stat "/tmp") >= 0)

(* creat/close/unlink round-trip *)
local
  val tmpfile = "/tmp/posix_test_" ^ Int.toString
                    (Word.toInt (Posix.Process.pidToWord (Posix.ProcEnv.getpid ())))
in
  val test_fs12 = check'(fn _ =>
      let val fd = Posix.FileSys.creat (tmpfile, Posix.FileSys.S.irwxu)
          val _  = Posix.IO.close fd
          val st = Posix.FileSys.stat tmpfile
          val _  = Posix.FileSys.unlink tmpfile
      in Posix.FileSys.ST.isReg st end)
end

(* mkdir/rmdir *)
local
  val tmpdir = "/tmp/posix_testdir_" ^ Int.toString
                   (Word.toInt (Posix.Process.pidToWord (Posix.ProcEnv.getpid ())))
in
  val test_fs13 = check'(fn _ =>
      (Posix.FileSys.mkdir (tmpdir, Posix.FileSys.S.irwxu);
       let val st = Posix.FileSys.stat tmpdir
           val ok = Posix.FileSys.ST.isDir st
       in Posix.FileSys.rmdir tmpdir; ok end))
end

(* openf O_RDONLY on /dev/null *)
val test_fs14 = check'(fn _ =>
    let val fd = Posix.FileSys.openf
                     ("/dev/null", Posix.FileSys.O_RDONLY,
                      Posix.FileSys.O.flags [])
        val _  = Posix.IO.close fd
    in Posix.FileSys.fdToWord fd >= 0w0 end)

(* openf O_WRONLY on /dev/null *)
val test_fs15 = check'(fn _ =>
    let val fd = Posix.FileSys.openf
                     ("/dev/null", Posix.FileSys.O_WRONLY,
                      Posix.FileSys.O.flags [])
        val _  = Posix.IO.close fd
    in true end)

(* link/unlink: create a hard link *)
local
  val base = "/tmp/posix_lnk_" ^ Int.toString
                 (Word.toInt (Posix.Process.pidToWord (Posix.ProcEnv.getpid ())))
  val src  = base ^ "_src"
  val dst  = base ^ "_dst"
in
  val test_fs16 = check'(fn _ =>
      let val fd  = Posix.FileSys.creat (src, Posix.FileSys.S.irwxu)
          val _   = Posix.IO.close fd
          val _   = Posix.FileSys.link {old = src, new = dst}
          val st  = Posix.FileSys.stat src
          val nl  = Posix.FileSys.ST.nlink st
          val _   = Posix.FileSys.unlink src
          val _   = Posix.FileSys.unlink dst
      in nl = 2 end)
end

(* symlink/readlink *)
local
  val base = "/tmp/posix_sym_" ^ Int.toString
                 (Word.toInt (Posix.Process.pidToWord (Posix.ProcEnv.getpid ())))
  val target = base ^ "_tgt"
  val lnk    = base ^ "_lnk"
in
  val test_fs17 = check'(fn _ =>
      let val fd  = Posix.FileSys.creat (target, Posix.FileSys.S.irwxu)
          val _   = Posix.IO.close fd
          val _   = Posix.FileSys.symlink {old = target, new = lnk}
          val r   = Posix.FileSys.readlink lnk
          val lst = Posix.FileSys.lstat lnk
          val _   = Posix.FileSys.unlink lnk
          val _   = Posix.FileSys.unlink target
      in r = target andalso Posix.FileSys.ST.isLink lst end)
end

(* access: /tmp exists and is read+exec *)
val test_fs18 = check'(fn _ =>
    Posix.FileSys.access ("/tmp", [Posix.FileSys.A_READ, Posix.FileSys.A_EXEC]))

(* access: /tmp exists (empty mode list = existence test) *)
val test_fs19 = check'(fn _ =>
    Posix.FileSys.access ("/tmp", []))

(* rename *)
local
  val base = "/tmp/posix_ren_" ^ Int.toString
                 (Word.toInt (Posix.Process.pidToWord (Posix.ProcEnv.getpid ())))
  val src = base ^ "_s"
  val dst = base ^ "_d"
in
  val test_fs20 = check'(fn _ =>
      let val fd = Posix.FileSys.creat (src, Posix.FileSys.S.irwxu)
          val _  = Posix.IO.close fd
          val _  = Posix.FileSys.rename {old = src, new = dst}
          val ok = Posix.FileSys.access (dst, [])
              andalso not (Posix.FileSys.access (src, []) handle _ => false)
          val _  = Posix.FileSys.unlink dst
      in ok end)
end

(* fstat matches stat for /dev/null *)
val test_fs21 = check'(fn _ =>
    let val fd  = Posix.FileSys.openf
                      ("/dev/null", Posix.FileSys.O_RDONLY,
                       Posix.FileSys.O.flags [])
        val fst = Posix.FileSys.fstat fd
        val _   = Posix.IO.close fd
    in Posix.FileSys.ST.isChr fst end)

(* umask: returns a word, setting and restoring *)
val test_fs22 = check'(fn _ =>
    let val m18 = Posix.FileSys.S.flags [Posix.FileSys.S.iwgrp, Posix.FileSys.S.iwoth]
        val old = Posix.FileSys.umask m18
        val _   = Posix.FileSys.umask old  (* restore *)
        val w   = Posix.FileSys.S.toWord old
    in w <= 0w0777 end)

(* opendir/readdir/closedir *)
val test_fs23 = check'(fn _ =>
    let val ds  = Posix.FileSys.opendir "/tmp"
        fun drain () =
            case Posix.FileSys.readdir ds of
              NONE   => true
            | SOME _ => drain ()
        val ok  = drain ()
        val _   = Posix.FileSys.closedir ds
    in ok end)

(* pathconf on /tmp *)
val test_fs24 = check'(fn _ =>
    case Posix.FileSys.pathconf ("/tmp", "NAME_MAX") of
      SOME n => n > 0w0
    | NONE   => true)  (* NONE is legal if unbounded *)

(* ftruncate *)
local
  val tmpf = "/tmp/posix_trunc_" ^ Int.toString
                 (Word.toInt (Posix.Process.pidToWord (Posix.ProcEnv.getpid ())))
in
  val test_fs25 = check'(fn _ =>
      let val fd = Posix.FileSys.creat (tmpf, Posix.FileSys.S.irwxu)
          val _  = Posix.IO.writeVec (fd,
                       Word8VectorSlice.full
                           (Byte.stringToBytes "hello world"))
          val _  = Posix.IO.close fd
          val fd2 = Posix.FileSys.openf
                        (tmpf, Posix.FileSys.O_WRONLY, Posix.FileSys.O.flags [])
          val _  = Posix.FileSys.ftruncate (fd2, 5)
          val _  = Posix.IO.close fd2
          val st = Posix.FileSys.stat tmpf
          val _  = Posix.FileSys.unlink tmpf
      in Posix.FileSys.ST.size st = 5 end)
end

(* ------------------------------------------------------------------ *)
(* IO                                                                  *)
(* ------------------------------------------------------------------ *)

val _ = print "IO tests...\n"

(* pipe: creates two valid fds *)
val test_io1 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val _ = Posix.IO.close infd
        val _ = Posix.IO.close outfd
    in true end)

(* write/read round-trip through pipe *)
val test_io2 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val msg = Byte.stringToBytes "hello"
        val n   = Posix.IO.writeVec (outfd, Word8VectorSlice.full msg)
        val _   = Posix.IO.close outfd
        val buf = Posix.IO.readVec (infd, 100)
        val _   = Posix.IO.close infd
    in n = 5 andalso Byte.bytesToString buf = "hello" end)

(* write/read: exact byte count *)
val test_io3 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val msg = Byte.stringToBytes "abcde"
        val _   = Posix.IO.writeVec (outfd, Word8VectorSlice.full msg)
        val _   = Posix.IO.close outfd
        val v   = Posix.IO.readVec (infd, 3)
        val _   = Posix.IO.close infd
    in Word8Vector.length v = 3 andalso
       Byte.bytesToString v = "abc" end)

(* readVec on /dev/null returns empty *)
val test_io4 = check'(fn _ =>
    let val fd = Posix.FileSys.openf
                     ("/dev/null", Posix.FileSys.O_RDONLY,
                      Posix.FileSys.O.flags [])
        val v  = Posix.IO.readVec (fd, 100)
        val _  = Posix.IO.close fd
    in Word8Vector.length v = 0 end)

(* dup: duplicate stdin fd *)
val test_io5 = check'(fn _ =>
    let val fd2 = Posix.IO.dup Posix.FileSys.stdin
        val _   = Posix.IO.close fd2
    in Posix.FileSys.fdToWord fd2 > 0w0 end)

(* dup2: duplicate to specific fd number *)
val test_io6 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val _ = Posix.IO.dup2 {old = outfd, new = outfd} (* dup to itself: no-op *)
        val _ = Posix.IO.close infd
        val _ = Posix.IO.close outfd
    in true end)

(* dupfd: dup to base or higher *)
val test_io7 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val fd2 = Posix.IO.dupfd {old = infd, base = Posix.FileSys.wordToFD 0w10}
        val _   = Posix.IO.close infd
        val _   = Posix.IO.close outfd
        val _   = Posix.IO.close fd2
    in Posix.FileSys.fdToWord fd2 >= 0w10 end)

(* lseek on a regular file *)
local
  val tmpf = "/tmp/posix_seek_" ^ Int.toString
                 (Word.toInt (Posix.Process.pidToWord (Posix.ProcEnv.getpid ())))
in
  val test_io8 = check'(fn _ =>
      let val fd  = Posix.FileSys.creat (tmpf, Posix.FileSys.S.irwxu)
          val _   = Posix.IO.writeVec (fd,
                        Word8VectorSlice.full (Byte.stringToBytes "abcde"))
          val pos = Posix.IO.lseek (fd, 2, Posix.IO.SEEK_SET)
          val _   = Posix.IO.close fd
          val _   = Posix.FileSys.unlink tmpf
      in pos = 2 end)
end

(* getfd/setfd: FD_CLOEXEC flag *)
val test_io9 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val old = Posix.IO.getfd infd
        val _   = Posix.IO.setfd (infd, Posix.IO.FD.cloexec)
        val new = Posix.IO.getfd infd
        val _   = Posix.IO.setfd (infd, old)  (* restore *)
        val _   = Posix.IO.close infd
        val _   = Posix.IO.close outfd
    in Posix.IO.FD.allSet (new, Posix.IO.FD.cloexec) end)

(* getfl/setfl: O.append flag *)
val test_io10 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val (fl, omode) = Posix.IO.getfl outfd
        val _ = Posix.IO.close infd
        val _ = Posix.IO.close outfd
    in true end)  (* just test it doesn't raise *)

(* writeArr/readArr *)
val test_io11 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val arr  = Word8Array.fromList
                       (map Word8.fromInt [104, 101, 108, 108, 111]) (* "hello" *)
        val sl   = Word8ArraySlice.full arr
        val n    = Posix.IO.writeArr (outfd, sl)
        val _    = Posix.IO.close outfd
        val v    = Posix.IO.readVec (infd, 5)
        val _    = Posix.IO.close infd
    in n = 5 andalso Byte.bytesToString v = "hello" end)

(* readArr into array slice *)
val test_io12 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val msg  = Byte.stringToBytes "world"
        val _    = Posix.IO.writeVec (outfd, Word8VectorSlice.full msg)
        val _    = Posix.IO.close outfd
        val arr  = Word8Array.array (10, 0w0)
        val sl   = Word8ArraySlice.slice (arr, 0, SOME 5)
        val n    = Posix.IO.readArr (infd, sl)
        val _    = Posix.IO.close infd
    in n = 5 andalso Byte.bytesToString (Word8Array.vector arr) = "world\000\000\000\000\000"
    end)

(* mkfifo: create a named pipe and communicate through it *)
local
  val fifo = "/tmp/posix_fifo_" ^ Int.toString
                 (Word.toInt (Posix.Process.pidToWord (Posix.ProcEnv.getpid ())))
in
  val test_io13 = check'(fn _ =>
      (Posix.FileSys.mkfifo (fifo, Posix.FileSys.S.irwxu);
       let val ok =
           case Posix.Process.fork () of
             NONE =>
               (* child: write to fifo *)
               let val fd  = Posix.FileSys.openf
                                 (fifo, Posix.FileSys.O_WRONLY,
                                  Posix.FileSys.O.flags [])
                   val _   = Posix.IO.writeVec (fd,
                                 Word8VectorSlice.full (Byte.stringToBytes "ok"))
                   val _   = Posix.IO.close fd
               in Posix.Process.exit 0w0; false end
           | SOME pid =>
               (* parent: read from fifo *)
               let val fd  = Posix.FileSys.openf
                                 (fifo, Posix.FileSys.O_RDONLY,
                                  Posix.FileSys.O.flags [])
                   val v   = Posix.IO.readVec (fd, 10)
                   val _   = Posix.IO.close fd
                   val _   = Posix.Process.wait ()
               in Byte.bytesToString v = "ok" end
       in Posix.FileSys.unlink fifo; ok end))
end

(* ------------------------------------------------------------------ *)
(* SysDB                                                               *)
(* ------------------------------------------------------------------ *)

val _ = print "SysDB tests...\n"

(* getpwuid: look up current user *)
val test_db1 = check'(fn _ =>
    let val uid  = Posix.ProcEnv.getuid ()
        val pw   = Posix.SysDB.getpwuid uid
        val name = Posix.SysDB.Passwd.name pw
    in String.size name > 0 end)

(* getpwuid: uid matches *)
val test_db2 = check'(fn _ =>
    let val uid = Posix.ProcEnv.getuid ()
        val pw  = Posix.SysDB.getpwuid uid
    in Posix.SysDB.Passwd.uid pw = uid end)

(* getpwuid: home and shell are non-empty strings *)
val test_db3 = check'(fn _ =>
    let val pw = Posix.SysDB.getpwuid (Posix.ProcEnv.getuid ())
    in String.size (Posix.SysDB.Passwd.home  pw) > 0 andalso
       String.size (Posix.SysDB.Passwd.shell pw) > 0 end)

(* getpwnam: round-trip through name *)
val test_db4 = check'(fn _ =>
    let val uid  = Posix.ProcEnv.getuid ()
        val pw1  = Posix.SysDB.getpwuid uid
        val name = Posix.SysDB.Passwd.name pw1
        val pw2  = Posix.SysDB.getpwnam name
    in Posix.SysDB.Passwd.uid pw2 = uid end)

(* getgrgid: look up current group *)
val test_db5 = check'(fn _ =>
    let val gid  = Posix.ProcEnv.getgid ()
        val gr   = Posix.SysDB.getgrgid gid
        val name = Posix.SysDB.Group.name gr
    in String.size name > 0 end)

(* getgrgid: gid matches *)
val test_db6 = check'(fn _ =>
    let val gid = Posix.ProcEnv.getgid ()
        val gr  = Posix.SysDB.getgrgid gid
    in Posix.SysDB.Group.gid gr = gid end)

(* getgrnam: round-trip through name *)
val test_db7 = check'(fn _ =>
    let val gid  = Posix.ProcEnv.getgid ()
        val gr1  = Posix.SysDB.getgrgid gid
        val name = Posix.SysDB.Group.name gr1
        val gr2  = Posix.SysDB.getgrnam name
    in Posix.SysDB.Group.gid gr2 = gid end)

(* SysDB.Group.members returns a list *)
val test_db8 = check'(fn _ =>
    let val gid = Posix.ProcEnv.getgid ()
        val gr  = Posix.SysDB.getgrgid gid
        val _   = Posix.SysDB.Group.members gr   (* just check no exception *)
    in true end)

(* ------------------------------------------------------------------ *)
(* Summary                                                             *)
(* ------------------------------------------------------------------ *)

val _ = print "All Posix tests done.\n"
