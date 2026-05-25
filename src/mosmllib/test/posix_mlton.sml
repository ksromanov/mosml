(* posix_mlton.sml -- Posix tests adapted from the MLton regression suite.
 *
 * Sources:
 *   mlton/regression/posix-exit.sml
 *   mlton/regression/posix-procenv.sml
 *   mlton/regression/filesys.sml        (the Posix.FileSys parts)
 *   mlton/regression/basis-sharing.sml  (the Posix sharing assertions)
 *)

use "auxil.sml";
load "Posix";
load "OS";

(* ------------------------------------------------------------------ *)
(* Helpers (adapted from MLton's tst0/tst' pattern)                   *)
(* ------------------------------------------------------------------ *)

fun tst0 s s' = print (s ^ "\t" ^ s' ^ "\n")
fun tst  s b  = tst0 s (if b then "OK" else "WRONG")
fun tst' s f  = tst0 s ((if f () then "OK" else "WRONG") handle _ => "EXN")

val pid_str =
    Word.toString (Posix.Process.pidToWord (Posix.ProcEnv.getpid ()))

fun tmpdir () = "/tmp/posix_mlton_" ^ pid_str

(* ------------------------------------------------------------------ *)
(* From posix-exit.sml                                                 *)
(* Posix.Process.exit terminates after writing to stdout.              *)
(* (Tested via fork: child calls exit, parent checks status.)          *)
(* ------------------------------------------------------------------ *)

val _ = print "posix-exit tests...\n"

(* exit 0 → W_EXITED *)
val test_exit1 = check'(fn _ =>
    case Posix.Process.fork () of
      NONE =>
        (TextIO.output (TextIO.stdOut, "");    (* mirrors posix-exit.sml *)
         Posix.Process.exit 0w0; false)
    | SOME pid =>
        let val (_, st) = Posix.Process.wait ()
        in case st of
             Posix.Process.W_EXITED        => true
           | Posix.Process.W_EXITSTATUS w  => w = 0w0
           | _                             => false
        end)

(* exit non-zero → W_EXITSTATUS *)
val test_exit2 = check'(fn _ =>
    case Posix.Process.fork () of
      NONE      => (Posix.Process.exit 0w7; false)
    | SOME pid  =>
        let val (_, st) = Posix.Process.wait ()
        in case st of
             Posix.Process.W_EXITSTATUS w => w = 0w7
           | _                            => false
        end)

(* exit 0w0 is the same type as Word8 *)
val test_exit3 = check'(fn _ =>
    (Posix.Process.exit: Word8.word -> 'a; true))

(* ------------------------------------------------------------------ *)
(* From posix-procenv.sml                                              *)
(* Exercises every ProcEnv accessor and the mutators that are safe     *)
(* to call (setuid/setgid to same values, setpgid to self).            *)
(* ------------------------------------------------------------------ *)

val _ = print "posix-procenv tests...\n"

(* Every accessor returns without raising *)
val test_pe1 = check'(fn _ =>
    let open Posix.ProcEnv
        val _  = getegid ()
        val _  = geteuid ()
        val _  = getgid  ()
        val _  = getgroups ()
        val _  = getlogin () handle _ => "<login>"
        val _  = getpgrp ()
        val _  = getpid  ()
        val _  = getppid ()
        val _  = getuid  ()
    in true end)

(* getenv: known variable has a string, unknown returns NONE *)
val test_pe2 = check'(fn _ =>
    let open Posix.ProcEnv
        val v = case getenv "PATH" of SOME s => String.size s >= 0 | NONE => true
    in v andalso getenv "POSIX_MLTON_NO_SUCH_VAR_XYZ" = NONE end)

(* getpid > 0 and getppid > 0 *)
val test_pe3 = check'(fn _ =>
    let open Posix.ProcEnv
    in Posix.Process.pidToWord (getpid  ()) > 0w0 andalso
       Posix.Process.pidToWord (getppid ()) > 0w0 end)

(* getpid != getppid in normal context *)
val test_pe4 = check'(fn _ =>
    let open Posix.ProcEnv
    in getpid () <> getppid () end)

(* uid/gid word conversions round-trip *)
val test_pe5 = check'(fn _ =>
    let open Posix.ProcEnv
        val uid = getuid ()
        val gid = getgid ()
    in wordToUid (uidToWord uid) = uid andalso
       wordToGid (gidToWord gid) = gid end)

(* setuid/setgid to current values: succeeds or raises (EPERM if no priv) *)
val test_pe6 = check'(fn _ =>
    let open Posix.ProcEnv
        val uid = getuid ()
        val gid = getgid ()
    in (setuid uid; setgid gid; true)
       handle _ => true   (* EPERM is acceptable — we may not be root *)
    end)

(* setpgid {pid=SOME self, pgid=SOME self} — set own process group *)
val test_pe7 = check'(fn _ =>
    let open Posix.ProcEnv
        val pid = getpid ()
    in (setpgid {pid = SOME pid, pgid = SOME pid}; true)
       handle _ => true   (* may fail if already a session leader *)
    end)

(* setsid: may fail if already session leader, that's OK *)
val test_pe8 = check'(fn _ =>
    (Posix.ProcEnv.setsid (); true) handle _ => true)

(* uname: returns a list with at least "sysname" and "machine" *)
val test_pe9 = check'(fn _ =>
    let val info = Posix.ProcEnv.uname ()
        fun has k = List.exists (fn (n,_) => n = k) info
    in has "sysname" andalso has "machine" end)

(* times: all time fields are non-negative *)
val test_pe10 = check'(fn _ =>
    let val t = Posix.ProcEnv.times ()
    in not (Time.< (#elapsed t, Time.zeroTime)) andalso
       not (Time.< (#utime   t, Time.zeroTime)) andalso
       not (Time.< (#cstime  t, Time.zeroTime)) end)

(* sysconf: standard properties are available *)
val test_pe11 = check'(fn _ =>
    let fun sc s = Posix.ProcEnv.sysconf s > 0w0
    in sc "CLK_TCK" andalso sc "OPEN_MAX" end)

(* ctermid, isatty: don't crash *)
val test_pe12 = check'(fn _ =>
    (Posix.ProcEnv.ctermid (); true))

(* environ: returns a list (may be empty in restricted environments) *)
val test_pe13 = check'(fn _ =>
    (Posix.ProcEnv.environ (); true))

(* ------------------------------------------------------------------ *)
(* From filesys.sml — the Posix.FileSys portions                      *)
(* Tests symlink, link, stat, stat.nlink, chmod, access, fstat.        *)
(* We run in a temp directory so we don't pollute the test directory.  *)
(* ------------------------------------------------------------------ *)

val _ = print "filesys (Posix.FileSys) tests...\n"

local
  val base      = tmpdir ()
  val readme    = base ^ "/README"
  val testlink  = base ^ "/testlink"
  val testcycl  = base ^ "/testcycl"
  val testbadl  = base ^ "/testbadl"
  val hla       = base ^ "/hardlinkA"
  val hlb       = base ^ "/hardlinkB"

  fun writeFile path =
      TextIO.closeOut (TextIO.openOut path)

  val _ = Posix.FileSys.mkdir (base, Posix.FileSys.S.irwxu)
  val _ = writeFile readme
  val _ = writeFile hla

  (* Create symlinks: testlink→README, testcycl→testcycl, testbadl→exists.not *)
  val _ = Posix.FileSys.symlink {old = readme,       new = testlink}
  val _ = Posix.FileSys.symlink {old = "testcycl",   new = testcycl}
  val _ = Posix.FileSys.symlink {old = "exists.not", new = testbadl}
  (* Hard link hlb → hla (both exist as regular files) *)
  val _ = Posix.FileSys.link   {old = hla,           new = hlb}

in

  (* Stat a regular file: isReg=true, nlink=1 *)
  val test_fs1 = tst' "fs_stat_isReg"
      (fn _ => Posix.FileSys.ST.isReg (Posix.FileSys.stat readme))

  val test_fs2 = tst' "fs_stat_nlink"
      (fn _ => Posix.FileSys.ST.nlink (Posix.FileSys.stat readme) >= 1)

  (* Stat a directory: isDir=true *)
  val test_fs3 = tst' "fs_stat_isDir"
      (fn _ => Posix.FileSys.ST.isDir (Posix.FileSys.stat base))

  (* lstat a symlink: isLink=true; stat through it: isReg=true *)
  val test_fs4 = tst' "fs_lstat_isLink"
      (fn _ => Posix.FileSys.ST.isLink (Posix.FileSys.lstat testlink))

  val test_fs5 = tst' "fs_stat_through_link"
      (fn _ => Posix.FileSys.ST.isReg (Posix.FileSys.stat testlink))

  (* lstat cyclic symlink: isLink=true (we don't follow it) *)
  val test_fs6 = tst' "fs_lstat_cyclic"
      (fn _ => Posix.FileSys.ST.isLink (Posix.FileSys.lstat testcycl))

  (* lstat dangling symlink: isLink=true *)
  val test_fs7 = tst' "fs_lstat_dangling"
      (fn _ => Posix.FileSys.ST.isLink (Posix.FileSys.lstat testbadl))

  (* stat cyclic/dangling symlinks: fails *)
  val test_fs8 = tst0 "fs_stat_cyclic_fails"
      ((Posix.FileSys.stat testcycl; "WRONG")
       handle OS.SysErr _ => "OK" | Fail _ => "OK" | _ => "OK")

  val test_fs9 = tst0 "fs_stat_dangling_fails"
      ((Posix.FileSys.stat testbadl; "WRONG")
       handle OS.SysErr _ => "OK" | Fail _ => "OK" | _ => "OK")

  (* link: hard link increases nlink to 2 — both hla and hlb exist *)
  val test_fs10 = tst' "fs_link_nlink"
      (fn _ => Posix.FileSys.ST.nlink (Posix.FileSys.stat hla) = 2)

  (* readlink: symlinks return correct targets *)
  val test_fs11 = tst' "fs_readlink_testlink"
      (fn _ => Posix.FileSys.readlink testlink = readme)

  val test_fs12 = tst' "fs_readlink_cyclic"
      (fn _ => Posix.FileSys.readlink testcycl = "testcycl")

  val test_fs13 = tst' "fs_readlink_dangling"
      (fn _ => Posix.FileSys.readlink testbadl = "exists.not")

  (* readlink on non-symlink: fails *)
  val test_fs14 = tst0 "fs_readlink_nonsymlink_fails"
      ((Posix.FileSys.readlink readme; "WRONG")
       handle OS.SysErr _ => "OK" | Fail _ => "OK" | _ => "OK")

  (* access: README exists and is readable *)
  val test_fs15 = tst' "fs_access_exists"
      (fn _ => Posix.FileSys.access (readme, []))

  val test_fs16 = tst' "fs_access_read"
      (fn _ => Posix.FileSys.access (readme, [Posix.FileSys.A_READ]))

  (* access: non-existing file → false *)
  val test_fs17 = tst' "fs_access_nonexist"
      (fn _ => not (Posix.FileSys.access (base ^ "/no_such_file", [])))

  (* access: testlink accessible via symlink *)
  val test_fs18 = tst' "fs_access_through_link"
      (fn _ => Posix.FileSys.access (testlink, []))

  (* access: dangling symlink not accessible *)
  val test_fs19 = tst' "fs_access_dangling"
      (fn _ => not (Posix.FileSys.access (testbadl, [])))

  (* chmod: change mode and verify via fstat *)
  val test_fs20 = tst' "fs_chmod"
      (fn _ =>
          let val newmode = Posix.FileSys.S.flags [Posix.FileSys.S.irusr]
              val _ = Posix.FileSys.chmod (readme, newmode)
              val m = Posix.FileSys.ST.mode (Posix.FileSys.stat readme)
              (* Restore original mode *)
              val _ = Posix.FileSys.chmod (readme, Posix.FileSys.S.irwxu)
          in Posix.FileSys.S.allSet (m, Posix.FileSys.S.irusr) end)

  (* fstat: same inode as stat *)
  val test_fs21 = tst' "fs_fstat_ino"
      (fn _ =>
          let val fd  = Posix.FileSys.openf
                            (readme, Posix.FileSys.O_RDONLY,
                             Posix.FileSys.O.flags [])
              val fst = Posix.FileSys.fstat fd
              val st  = Posix.FileSys.stat readme
              val _   = Posix.IO.close fd
          in Posix.FileSys.ST.ino fst = Posix.FileSys.ST.ino st end)

  (* umask: returns a value in 0..0777 *)
  val test_fs22 = tst' "fs_umask"
      (fn _ =>
          let val prev = Posix.FileSys.umask
                             (Posix.FileSys.S.flags [Posix.FileSys.S.iwgrp])
              val _    = Posix.FileSys.umask prev   (* restore *)
          in Posix.FileSys.S.toWord prev <= 0w0777 end)

  (* openf modes: O_RDONLY and O_WRONLY open cleanly *)
  val test_fs23 = tst' "fs_openf_rdonly"
      (fn _ =>
          let val fd = Posix.FileSys.openf
                           (readme, Posix.FileSys.O_RDONLY,
                            Posix.FileSys.O.flags [])
          in Posix.IO.close fd; true end)

  val test_fs24 = tst' "fs_openf_wronly"
      (fn _ =>
          let val fd = Posix.FileSys.openf
                           (readme, Posix.FileSys.O_WRONLY,
                            Posix.FileSys.O.flags [])
          in Posix.IO.close fd; true end)

  (* creat: creates a file and returns a writable fd *)
  val test_fs25 = tst' "fs_creat"
      (fn _ =>
          let val newf = base ^ "/creat_test"
              val fd   = Posix.FileSys.creat (newf, Posix.FileSys.S.irwxu)
              val _    = Posix.IO.writeVec (fd,
                             Word8VectorSlice.full
                                 (Byte.stringToBytes "test"))
              val _    = Posix.IO.close fd
              val sz   = Posix.FileSys.ST.size (Posix.FileSys.stat newf)
              val _    = Posix.FileSys.unlink newf
          in sz = 4 end)

  (* mkdir + rmdir round-trip inside temp dir *)
  val test_fs26 = tst' "fs_mkdir_rmdir"
      (fn _ =>
          let val sub = base ^ "/subdir"
          in Posix.FileSys.mkdir (sub, Posix.FileSys.S.irwxu);
             Posix.FileSys.rmdir sub;
             not (Posix.FileSys.access (sub, [])) end)

  (* mkfifo: creates a FIFO special file *)
  val test_fs27 = tst' "fs_mkfifo"
      (fn _ =>
          let val fifo = base ^ "/test_fifo"
          in Posix.FileSys.mkfifo (fifo, Posix.FileSys.S.irwxu);
             let val st = Posix.FileSys.stat fifo
             in Posix.FileSys.unlink fifo;
                Posix.FileSys.ST.isFIFO st end
          end)

  (* rename inside temp dir *)
  val test_fs28 = tst' "fs_rename"
      (fn _ =>
          let val f1 = base ^ "/rename_src"
              val f2 = base ^ "/rename_dst"
              val _  = TextIO.closeOut (TextIO.openOut f1)
              val _  = Posix.FileSys.rename {old = f1, new = f2}
          in not (Posix.FileSys.access (f1, [])) andalso
             Posix.FileSys.access (f2, []) andalso
             (Posix.FileSys.unlink f2; true)
          end)

  (* ftruncate: resize a file *)
  val test_fs29 = tst' "fs_ftruncate"
      (fn _ =>
          let val f   = base ^ "/trunc_test"
              val fd  = Posix.FileSys.creat (f, Posix.FileSys.S.irwxu)
              val _   = Posix.IO.writeVec (fd,
                            Word8VectorSlice.full
                                (Byte.stringToBytes "hello world"))
              val _   = Posix.FileSys.ftruncate (fd, 5)
              val sz  = Posix.FileSys.ST.size (Posix.FileSys.fstat fd)
              val _   = Posix.IO.close fd
              val _   = Posix.FileSys.unlink f
          in sz = 5 end)

  (* Cleanup *)
  val _ = (Posix.FileSys.unlink testlink) handle _ => ()
  val _ = (Posix.FileSys.unlink testcycl) handle _ => ()
  val _ = (Posix.FileSys.unlink testbadl) handle _ => ()
  val _ = (Posix.FileSys.unlink hla)      handle _ => ()
  val _ = (Posix.FileSys.unlink hlb)      handle _ => ()
  val _ = (Posix.FileSys.unlink readme)   handle _ => ()
  val _ = (Posix.FileSys.rmdir base)      handle _ => ()

end

(* ------------------------------------------------------------------ *)
(* From basis-sharing.sml — Posix type sharing assertions             *)
(* Each line tests that two Posix types are IDENTICAL by applying the  *)
(* polymorphic identity function across the two types.                 *)
(* If the types differ, this is a compile-time type error.             *)
(* ------------------------------------------------------------------ *)

val _ = print "basis-sharing (Posix type sharing) tests...\n"

local
  val id = fn (x : 'a) => x
in

  (* Posix.Process.signal = Posix.Signal.signal *)
  val test_share1 = tst' "share_signal"
      (fn _ =>
          let val s : Posix.Signal.signal = Posix.Signal.term
              val _ : Posix.Process.signal = id s
          in true end)

  (* Posix.ProcEnv.pid = Posix.Process.pid *)
  val test_share2 = tst' "share_pid"
      (fn _ =>
          let val p : Posix.ProcEnv.pid = Posix.ProcEnv.getpid ()
              val _ : Posix.Process.pid = id p
          in true end)

  (* Posix.IO.pid = Posix.Process.pid *)
  val test_share3 = tst' "share_IO_pid"
      (fn _ =>
          let val p : Posix.Process.pid = Posix.ProcEnv.getpid ()
              val _ : Posix.IO.pid = id p
          in true end)

  (* Posix.IO.file_desc = Posix.FileSys.file_desc *)
  val test_share4 = tst' "share_IO_file_desc"
      (fn _ =>
          let val fd : Posix.FileSys.file_desc = Posix.FileSys.stdin
              val _ : Posix.IO.file_desc = id fd
          in true end)

  (* Posix.FileSys.file_desc = Posix.ProcEnv.file_desc *)
  val test_share5 = tst' "share_FileSys_file_desc"
      (fn _ =>
          let val fd : Posix.ProcEnv.file_desc = Posix.FileSys.stdin
              val _ : Posix.FileSys.file_desc = id fd
          in true end)

  (* Posix.FileSys.uid = Posix.ProcEnv.uid *)
  val test_share6 = tst' "share_uid"
      (fn _ =>
          let val uid : Posix.ProcEnv.uid = Posix.ProcEnv.getuid ()
              val _ : Posix.FileSys.uid = id uid
          in true end)

  (* Posix.FileSys.gid = Posix.ProcEnv.gid *)
  val test_share7 = tst' "share_gid"
      (fn _ =>
          let val gid : Posix.ProcEnv.gid = Posix.ProcEnv.getgid ()
              val _ : Posix.FileSys.gid = id gid
          in true end)

  (* Posix.SysDB.uid = Posix.ProcEnv.uid *)
  val test_share8 = tst' "share_SysDB_uid"
      (fn _ =>
          let val uid : Posix.ProcEnv.uid = Posix.ProcEnv.getuid ()
              val pw  = Posix.SysDB.getpwuid uid
              val _   : Posix.SysDB.uid = Posix.SysDB.Passwd.uid pw
              (* SysDB.uid should equal ProcEnv.uid *)
              val _   : Posix.ProcEnv.uid = id (Posix.SysDB.Passwd.uid pw)
          in true end)

  (* Posix.SysDB.gid = Posix.ProcEnv.gid *)
  val test_share9 = tst' "share_SysDB_gid"
      (fn _ =>
          let val gid : Posix.ProcEnv.gid = Posix.ProcEnv.getgid ()
              val gr  = Posix.SysDB.getgrgid gid
              val _   : Posix.SysDB.gid = Posix.SysDB.Group.gid gr
              val _   : Posix.ProcEnv.gid = id (Posix.SysDB.Group.gid gr)
          in true end)

  (* Posix.IO.open_mode = Posix.FileSys.open_mode *)
  val test_share10 = tst' "share_open_mode"
      (fn _ =>
          let val m : Posix.FileSys.open_mode = Posix.FileSys.O_RDONLY
              val _ : Posix.IO.open_mode = id m
          in true end)

  (* Confirmed: after sharing fix, FileSys.uid = ProcEnv.uid allows
     cross-structure uid comparison — the stat UID matches getuid *)
  val test_share11 = tst' "share_stat_uid_matches_getuid"
      (fn _ =>
          let val tmp = "/tmp/posix_mlton_share_" ^ pid_str
              val _   = TextIO.closeOut (TextIO.openOut tmp)
              val st  = Posix.FileSys.stat tmp
              val _   = OS.FileSys.remove tmp
              (* These are now the same type: FileSys.uid = ProcEnv.uid *)
          in Posix.FileSys.ST.uid st = Posix.ProcEnv.getuid () end)

  val test_share12 = tst' "share_stat_gid_matches_getgid"
      (fn _ =>
          let val tmp = "/tmp/posix_mlton_share2_" ^ pid_str
              val _   = TextIO.closeOut (TextIO.openOut tmp)
              val st  = Posix.FileSys.stat tmp
              val _   = OS.FileSys.remove tmp
          in Posix.FileSys.ST.gid st = Posix.ProcEnv.getgid () end)

end

(* ------------------------------------------------------------------ *)
(* IO substructure tests from the spirit of basis-sharing.sml          *)
(* Validates that IO flags and operations work with the shared types.   *)
(* ------------------------------------------------------------------ *)

val _ = print "IO type-consistency tests...\n"

(* IO.FD.cloexec is compatible with IO flags operations *)
val test_io1 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val fl  = Posix.IO.getfd infd
        val _   = Posix.IO.setfd (infd, Posix.IO.FD.cloexec)
        val fl2 = Posix.IO.getfd infd
        val _   = Posix.IO.setfd (infd, fl)   (* restore *)
        val _   = Posix.IO.close infd
        val _   = Posix.IO.close outfd
    in Posix.IO.FD.allSet (fl2, Posix.IO.FD.cloexec) end)

(* IO.getfl returns open_mode identical to FileSys.open_mode value *)
val test_io2 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val (_, m)  = Posix.IO.getfl infd    (* m : IO.open_mode = FileSys.open_mode *)
        val _       = Posix.IO.close infd
        val _       = Posix.IO.close outfd
        (* m is of type IO.open_mode which equals FileSys.open_mode *)
        val _ : Posix.FileSys.open_mode = m
    in true end)

(* setfl / getfl round-trip with O.nonblock *)
val test_io3 = check'(fn _ =>
    let val {infd, outfd} = Posix.IO.pipe ()
        val (fl0, _) = Posix.IO.getfl infd
        val _ = Posix.IO.setfl (infd, Posix.IO.O.nonblock)
        val (fl1, _) = Posix.IO.getfl infd
        val _ = Posix.IO.setfl (infd, fl0)   (* restore *)
        val _ = Posix.IO.close infd
        val _ = Posix.IO.close outfd
    in Posix.IO.O.allSet (fl1, Posix.IO.O.nonblock) end)

(* ------------------------------------------------------------------ *)
(* stat atime/mtime/ctime tests                                        *)
(* Adapted from MLton filesys.sml (test9a/9b modTime) and OCaml        *)
(* utimes.ml. Tests that stat returns sensible timestamps.             *)
(* ------------------------------------------------------------------ *)

val _ = print "stat atime/mtime/ctime tests...\n"

(* mtime of a newly created file is close to current time *)
val test_time1 = tst' "stat_mtime_new_file"
    (fn _ =>
        let val tmp = "/tmp/posix_mtime_" ^ pid_str
            val t0  = Posix.ProcEnv.time ()
            val _   = TextIO.closeOut (TextIO.openOut tmp)
            val st  = Posix.FileSys.stat tmp
            val mt  = Posix.FileSys.ST.mtime st
            val _   = Posix.FileSys.unlink tmp
            val diff = Time.toReal (Time.- (mt, t0))
        in Real.abs diff < 5.0 end)

(* ctime of a newly created file is close to current time *)
val test_time2 = tst' "stat_ctime_new_file"
    (fn _ =>
        let val tmp = "/tmp/posix_ctime_" ^ pid_str
            val t0  = Posix.ProcEnv.time ()
            val _   = TextIO.closeOut (TextIO.openOut tmp)
            val st  = Posix.FileSys.stat tmp
            val ct  = Posix.FileSys.ST.ctime st
            val _   = Posix.FileSys.unlink tmp
            val diff = Time.toReal (Time.- (ct, t0))
        in Real.abs diff < 5.0 end)

(* atime is also a valid time (>= epoch) *)
val test_time3 = tst' "stat_atime_positive"
    (fn _ =>
        let val tmp = "/tmp/posix_atime_" ^ pid_str
            val _   = TextIO.closeOut (TextIO.openOut tmp)
            val st  = Posix.FileSys.stat tmp
            val at  = Posix.FileSys.ST.atime st
            val _   = Posix.FileSys.unlink tmp
        in not (Time.< (at, Time.zeroTime)) end)

(* mtime changes after writing to a file *)
val test_time4 = tst' "stat_mtime_changes_on_write"
    (fn _ =>
        let val tmp = "/tmp/posix_mtime2_" ^ pid_str
            val _   = TextIO.closeOut (TextIO.openOut tmp)
            val mt1 = Posix.FileSys.ST.mtime (Posix.FileSys.stat tmp)
            (* Wait briefly so mtime can change — 1 second minimum on
               filesystems with 1-second resolution *)
            val _   = Posix.Process.sleep (Time.fromReal 1.1)
            val fd  = Posix.FileSys.openf
                          (tmp, Posix.FileSys.O_WRONLY, Posix.FileSys.O.flags [])
            val _   = Posix.IO.writeVec (fd,
                          Word8VectorSlice.full (Byte.stringToBytes "update"))
            val _   = Posix.IO.close fd
            val mt2 = Posix.FileSys.ST.mtime (Posix.FileSys.stat tmp)
            val _   = Posix.FileSys.unlink tmp
        in Time.< (mt1, mt2) end)

(* ctime changes after chmod (metadata change) *)
val test_time5 = tst' "stat_ctime_changes_on_chmod"
    (fn _ =>
        let val tmp = "/tmp/posix_ctime2_" ^ pid_str
            val _   = TextIO.closeOut (TextIO.openOut tmp)
            val ct1 = Posix.FileSys.ST.ctime (Posix.FileSys.stat tmp)
            val _   = Posix.Process.sleep (Time.fromReal 1.1)
            val _   = Posix.FileSys.chmod (tmp, Posix.FileSys.S.irwxu)
            val ct2 = Posix.FileSys.ST.ctime (Posix.FileSys.stat tmp)
            val _   = Posix.FileSys.unlink tmp
        in Time.< (ct1, ct2) end)

(* fstat returns same timestamps as stat for same file *)
val test_time6 = tst' "stat_fstat_times_match"
    (fn _ =>
        let val tmp = "/tmp/posix_ftime_" ^ pid_str
            val _   = TextIO.closeOut (TextIO.openOut tmp)
            val st  = Posix.FileSys.stat tmp
            val fd  = Posix.FileSys.openf
                          (tmp, Posix.FileSys.O_RDONLY, Posix.FileSys.O.flags [])
            val fst = Posix.FileSys.fstat fd
            val _   = Posix.IO.close fd
            val _   = Posix.FileSys.unlink tmp
        in Posix.FileSys.ST.mtime st = Posix.FileSys.ST.mtime fst andalso
           Posix.FileSys.ST.ctime st = Posix.FileSys.ST.ctime fst end)

(* Directories also have valid timestamps *)
val test_time7 = tst' "stat_dir_mtime"
    (fn _ =>
        let val st = Posix.FileSys.stat "/tmp"
        in not (Time.< (Posix.FileSys.ST.mtime st, Time.zeroTime)) end)

(* lstat on symlink: returns the symlink's own timestamps *)
val test_time8 = tst' "lstat_symlink_times"
    (fn _ =>
        let val tgt  = "/tmp/posix_ltgt_" ^ pid_str
            val lnk  = "/tmp/posix_llnk_" ^ pid_str
            val _    = TextIO.closeOut (TextIO.openOut tgt)
            val _    = Posix.FileSys.symlink {old = tgt, new = lnk}
            val lst  = Posix.FileSys.lstat lnk
            val mt   = Posix.FileSys.ST.mtime lst
            val _    = Posix.FileSys.unlink lnk
            val _    = Posix.FileSys.unlink tgt
        in not (Time.< (mt, Time.zeroTime)) end)

val _ = print "All MLton-adapted Posix tests done.\n"
