(* Posix -- SML Basis Library, POSIX 1003.1 interface for Moscow ML *)

structure Posix :> Posix = struct
local
  open Dynlib
  val hdl  = dlopen { lib = "libmunix.so", flag = RTLD_LAZY, global = false }
  val symb = Dynlib.dlsym hdl

  fun app1 f = Dynlib.app1 (symb f)
  fun app2 f = Dynlib.app2 (symb f)
  fun app3 f = Dynlib.app3 (symb f)

  (* Process *)
  val posix_execv_      : string -> string Vector.vector -> unit        = app2 "posix_execv"
  val posix_execvp_     : string -> string Vector.vector -> unit        = app2 "posix_execvp"
  val posix_execve_     : string -> string Vector.vector -> string Vector.vector -> unit = app3 "posix_execve"
  val posix_alarm_      : int -> int                                    = app1 "posix_alarm"
  val posix_pause_      : unit -> unit                                  = app1 "posix_pause"
  val posix_nanosleep_  : int -> int -> int * int                       = app2 "posix_nanosleep"
  val posix_waitpid_status_ : int -> int * int                          = app1 "posix_waitpid_status"
  val posix_waitpid_nh_ : int -> int -> int * int * int              = app2 "posix_waitpid_nh"
  val posix_waitpid_any_: unit -> int * int * int                       = app1 "posix_waitpid_any"
  val unix_fork_        : unit -> int                                    = app1 "unix_fork"
  val unix_kill_        : int -> int -> unit                            = app2 "unix_kill"
  val unix_exit_        : int -> unit                                   = app1 "unix_exit"
  val unix_getpid_      : unit -> int                                   = app1 "unix_getpid"
  (* ProcEnv *)
  val posix_getppid_    : unit -> int                                   = app1 "posix_getppid"
  val posix_getuid_     : unit -> int                                   = app1 "posix_getuid"
  val posix_geteuid_    : unit -> int                                   = app1 "posix_geteuid"
  val posix_getgid_     : unit -> int                                   = app1 "posix_getgid"
  val posix_getegid_    : unit -> int                                   = app1 "posix_getegid"
  val posix_setuid_     : int -> unit                                   = app1 "posix_setuid"
  val posix_setgid_     : int -> unit                                   = app1 "posix_setgid"
  val posix_getgroups_  : unit -> int list                              = app1 "posix_getgroups"
  val posix_getlogin_   : unit -> string                                = app1 "posix_getlogin"
  val posix_getpgrp_    : unit -> int                                   = app1 "posix_getpgrp"
  val posix_setsid_     : unit -> int                                   = app1 "posix_setsid"
  val posix_setpgid_    : int -> int -> unit                            = app2 "posix_setpgid"
  val posix_time_       : unit -> int                                   = app1 "posix_time"
  val posix_times_      : unit -> int * int * int * int * int           = app1 "posix_times"
  val posix_clktck_     : unit -> int                                   = app1 "posix_clktck"
  val posix_environ_    : unit -> string list                           = app1 "posix_environ"
  val posix_ctermid_    : unit -> string                                = app1 "posix_ctermid"
  val posix_ttyname_    : int -> string                                 = app1 "posix_ttyname"
  val posix_isatty_     : int -> bool                                   = app1 "posix_isatty"
  val posix_sysconf_    : string -> int                                 = app1 "posix_sysconf"
  (* FileSys *)
  val posix_stat_       : string -> int * int * int * int * int * int * int * int * int * int = app1 "posix_stat"
  val posix_lstat_      : string -> int * int * int * int * int * int * int * int * int * int = app1 "posix_lstat"
  val posix_fstat_      : int -> int * int * int * int * int * int * int * int * int * int    = app1 "posix_fstat"
  val posix_openf_      : string -> int -> int                          = app2 "posix_openf"
  val posix_createf_    : string -> int -> int -> int                   = app3 "posix_createf"
  val posix_creat_      : string -> int -> int                          = app2 "posix_creat"
  val posix_umask_      : int -> int                                    = app1 "posix_umask"
  val posix_link_       : string -> string -> unit                      = app2 "posix_link"
  val posix_mkdir_      : string -> int -> unit                         = app2 "posix_mkdir"
  val posix_mkfifo_     : string -> int -> unit                         = app2 "posix_mkfifo"
  val posix_unlink_     : string -> unit                                = app1 "posix_unlink"
  val posix_rmdir_      : string -> unit                                = app1 "posix_rmdir"
  val posix_rename_     : string -> string -> unit                      = app2 "posix_rename"
  val posix_symlink_    : string -> string -> unit                      = app2 "posix_symlink"
  val posix_chmod_      : string -> int -> unit                         = app2 "posix_chmod"
  val posix_fchmod_     : int -> int -> unit                            = app2 "posix_fchmod"
  val posix_chown_      : string -> int -> int -> unit                  = app3 "posix_chown"
  val posix_fchown_     : int -> int -> int -> unit                     = app3 "posix_fchown"
  val posix_ftruncate_  : int -> int -> unit                            = app2 "posix_ftruncate"
  val posix_access_     : string -> int -> bool                         = app2 "posix_access"
  val posix_pathconf_   : string -> string -> int option                = app2 "posix_pathconf"
  val posix_fpathconf_  : int -> string -> int option                   = app2 "posix_fpathconf"
  (* IO *)
  val posix_close_      : int -> unit                                   = app1 "posix_close"
  val posix_dup_        : int -> int                                    = app1 "posix_dup"
  val posix_dup2_       : int -> int -> unit                            = app2 "posix_dup2"
  val posix_dupfd_      : int -> int -> int                             = app2 "posix_dupfd"
  val posix_pipe_       : unit -> int * int                             = app1 "posix_pipe"
  val posix_getfd_      : int -> int                                    = app1 "posix_getfd"
  val posix_setfd_      : int -> int -> unit                            = app2 "posix_setfd"
  val posix_getfl_      : int -> int                                    = app1 "posix_getfl"
  val posix_setfl_      : int -> int -> unit                            = app2 "posix_setfl"
  val posix_lseek_      : int -> int -> int -> int                      = app3 "posix_lseek"
  val posix_read_       : int -> int -> Word8Vector.vector              = app2 "posix_read"
  val posix_readarr_    : int -> Word8Vector.vector -> int -> int -> int = Dynlib.app4 (symb "posix_readarr")
  val posix_write_      : int -> Word8VectorSlice.vector -> int -> int -> int = Dynlib.app4 (symb "posix_write")
  (* SysDB *)
  val posix_getpwuid_   : int -> string * int * int * string * string   = app1 "posix_getpwuid"
  val posix_getpwnam_   : string -> string * int * int * string * string = app1 "posix_getpwnam"
  val posix_getgrgid_   : int -> string * int * string list             = app1 "posix_getgrgid"
  val posix_getgrnam_   : string -> string * int * string list          = app1 "posix_getgrnam"
  (* Error *)
  val posix_strerror_   : int -> string                                 = app1 "posix_strerror"
  (* uname from runtime *)
  prim_val uname_ : unit -> string * string * string = 1 "sml_uname"
  (* readlink from runtime *)
  prim_val readlink_ : string -> string = 1 "sml_readlink"

  (* Convert C posix_failure() Fail exceptions to proper OS.SysErr.
     SysErr is a built-in top-level exception (= OS.SysErr), available without qualification. *)
  fun sysErr (thunk : unit -> 'a) : 'a =
      thunk () handle Fail s => raise SysErr (s, NONE)

  (* Shared helpers *)
  fun mkBitFlags () =
      { flags     = fn (l : word list) => List.foldl Word.orb 0w0 l,
        toWord    = fn (f : word) => f,
        fromWord  = fn (w : word) => w,
        intersect = fn (l : word list) => List.foldl Word.andb (Word.notb 0w0) l,
        clear     = fn (a : word, b : word) => Word.andb(a, Word.notb b),
        allSet    = fn (a : word, b : word) => Word.andb(a, b) = b,
        anySet    = fn (a : word, b : word) => Word.andb(a, b) <> 0w0 }

in


  (* ------------------------------------------------------------------ *)
  structure Error = struct
    type syserror = int

    fun toWord   (e : syserror) : word = Word.fromInt e
    fun fromWord (w : word) : syserror = Word.toInt w

    fun errorMsg e  = posix_strerror_ e
    fun errorName e = posix_strerror_ e   (* fallback: same as msg *)
    fun syserror  s =
        (* search by name: try to find errno matching strerror *)
        let fun tryN n = if n > 200 then NONE
                         else if posix_strerror_ n = s then SOME n
                         else tryN (n + 1)
        in tryN 1 end

    val acces       = 13   (* EACCES   *)
    val again       = 11   (* EAGAIN   *)
    val badf        =  9   (* EBADF    *)
    val badmsg      = 74   (* EBADMSG  *)
    val busy        = 16   (* EBUSY    *)
    val canceled    = 125  (* ECANCELED *)
    val child       = 10   (* ECHILD   *)
    val deadlk      = 35   (* EDEADLK  *)
    val dom         = 33   (* EDOM     *)
    val exist       = 17   (* EEXIST   *)
    val fault       = 14   (* EFAULT   *)
    val fbig        = 27   (* EFBIG    *)
    val inprogress  = 115  (* EINPROGRESS *)
    val intr        =  4   (* EINTR    *)
    val inval       = 22   (* EINVAL   *)
    val io          =  5   (* EIO      *)
    val isdir       = 21   (* EISDIR   *)
    val loop        = 40   (* ELOOP    *)
    val mfile       = 24   (* EMFILE   *)
    val mlink       = 31   (* EMLINK   *)
    val msgsize     = 90   (* EMSGSIZE *)
    val nametoolong = 36   (* ENAMETOOLONG *)
    val nfile       = 23   (* ENFILE   *)
    val nodev       = 19   (* ENODEV   *)
    val noent       =  2   (* ENOENT   *)
    val noexec      =  8   (* ENOEXEC  *)
    val nolck       = 37   (* ENOLCK   *)
    val nomem       = 12   (* ENOMEM   *)
    val nospc       = 28   (* ENOSPC   *)
    val nosys       = 38   (* ENOSYS   *)
    val notdir      = 20   (* ENOTDIR  *)
    val notempty    = 39   (* ENOTEMPTY *)
    val notsup      = 95   (* ENOTSUP / EOPNOTSUPP *)
    val notty       = 25   (* ENOTTY   *)
    val nxio        =  6   (* ENXIO    *)
    val perm        =  1   (* EPERM    *)
    val pipe        = 32   (* EPIPE    *)
    val range       = 34   (* ERANGE   *)
    val rofs        = 30   (* EROFS    *)
    val spipe       = 29   (* ESPIPE   *)
    val srch        =  3   (* ESRCH    *)
    val toobig      =  7   (* E2BIG    *)
    val xdev        = 18   (* EXDEV    *)
  end

  (* ------------------------------------------------------------------ *)
  structure Signal = struct
    type signal = word

    fun toWord   (s : signal) : word = s
    fun fromWord (w : word) : signal  = w

    val hup  : signal = 0w1
    val int  : signal = 0w2
    val quit : signal = 0w3
    val ill  : signal = 0w4
    val abrt : signal = 0w6
    val bus  : signal = 0w7
    val fpe  : signal = 0w8
    val kill : signal = 0w9
    val usr1 : signal = 0w10
    val segv : signal = 0w11
    val usr2 : signal = 0w12
    val pipe : signal = 0w13
    val alrm : signal = 0w14
    val term : signal = 0w15
    val chld : signal = 0w17
    val cont : signal = 0w18
    val stop : signal = 0w19
    val tstp : signal = 0w20
    val ttin : signal = 0w21
    val ttou : signal = 0w22
  end

  (* ------------------------------------------------------------------ *)
  structure Process = struct
    type signal = Signal.signal
    type pid    = int

    fun wordToPid (w : word) : pid = Word.toInt w
    fun pidToWord (p : pid) : word = Word.fromInt p

    fun fork () =
        sysErr (fn () =>
          let val pid = unix_fork_ ()
          in if pid = 0 then NONE else SOME pid end)

    fun exec (path, args) =
        ( sysErr (fn () => posix_execv_ path (Vector.fromList (path :: args)));
          raise Fail "exec: unreachable" )

    fun exece (path, args, env) =
        ( sysErr (fn () =>
            posix_execve_ path (Vector.fromList (path :: args))
                               (Vector.fromList env));
          raise Fail "exece: unreachable" )

    fun execp (file, args) =
        ( sysErr (fn () => posix_execvp_ file (Vector.fromList (file :: args)));
          raise Fail "execp: unreachable" )

    datatype waitpid_arg
      = W_ANY_CHILD
      | W_CHILD of pid
      | W_SAME_GROUP
      | W_GROUP of pid

    datatype exit_status
      = W_EXITED
      | W_EXITSTATUS of Word8.word
      | W_SIGNALED of signal
      | W_STOPPED of signal

    fun fromStatus (st : OS.Process.status) : exit_status =
        if OS.Process.isSuccess st then W_EXITED
        else W_EXITSTATUS 0w1

    structure W = struct
      type flags = word
      val flags     = fn (l : flags list) => List.foldl Word.orb 0w0 l
      val toWord    = fn (f : flags) => f
      val fromWord  = fn (w : word) => w
      val intersect = fn (l : flags list) => List.foldl Word.andb (Word.notb 0w0) l
      val clear     = fn (a : flags, b : flags) => Word.andb(a, Word.notb b)
      val allSet    = fn (a : flags, b : flags) => Word.andb(a,b) = b
      val anySet    = fn (a : flags, b : flags) => Word.andb(a,b) <> 0w0
      val untraced  : flags = 0w2  (* WUNTRACED *)
    end

    fun decodeStatus (kind, code) =
        case kind of
          0 => if code = 0 then W_EXITED
               else W_EXITSTATUS (Word8.fromInt code)
        | 1 => W_SIGNALED (Word.fromInt code)
        | _ => W_STOPPED  (Word.fromInt code)

    fun pidOf arg =
        case arg of
          W_CHILD p    => p
        | W_ANY_CHILD  => ~1
        | W_SAME_GROUP => 0
        | W_GROUP p    => ~p

    fun wait () =
        sysErr (fn () =>
          let val (pid, kind, code) = posix_waitpid_any_ ()
          in (pid, decodeStatus (kind, code)) end)

    fun waitpid (arg, _) =
        sysErr (fn () =>
          let val pid = pidOf arg
              val (kind, code) = posix_waitpid_status_ pid
          in (pid, decodeStatus (kind, code)) end)

    fun waitpid_nh (arg, _) =
        sysErr (fn () =>
          let val pid = pidOf arg
              val (r, kind, code) = posix_waitpid_nh_ pid 0
          in if r = 0 then NONE
             else SOME (r, decodeStatus (kind, code))
          end)

    datatype killpid_arg
      = K_PROC of pid
      | K_SAME_GROUP
      | K_GROUP of pid

    fun kill (arg, signo) =
        sysErr (fn () =>
          let val pid = case arg of
                          K_PROC p     => p
                        | K_SAME_GROUP => 0
                        | K_GROUP p    => ~p
          in unix_kill_ pid (Word.toInt signo) end)

    fun alarm t =
        let val secs = Real.round (Time.toReal t)
            val rem  = posix_alarm_ secs
        in Time.fromReal (real rem) end

    fun pause () = posix_pause_ ()

    fun sleep t =
        let val secs  = Real.trunc (Time.toReal t)
            val nsecs = Real.round ((Time.toReal t - real secs) * 1.0e9)
            val (rs, rn) = posix_nanosleep_ secs nsecs
        in Time.fromReal (real rs + real rn / 1.0e9) end

    fun exit w = ( unix_exit_ (Word8.toInt w); raise Fail "exit: unreachable" )
  end

  (* ------------------------------------------------------------------ *)
  structure ProcEnv = struct
    type pid       = Process.pid
    type uid       = int
    type gid       = int
    type file_desc = int

    fun uidToWord (u : uid) : word = Word.fromInt u
    fun wordToUid (w : word) : uid = Word.toInt w
    fun gidToWord (g : gid) : word = Word.fromInt g
    fun wordToGid (w : word) : gid = Word.toInt w

    fun getpid  () = unix_getpid_ ()
    fun getppid () = posix_getppid_ ()
    fun getuid  () = posix_getuid_  ()
    fun geteuid () = posix_geteuid_ ()
    fun getgid  () = posix_getgid_  ()
    fun getegid () = posix_getegid_ ()
    fun setuid u   = posix_setuid_ u
    fun setgid g   = posix_setgid_ g
    fun getgroups () = posix_getgroups_ ()
    fun getlogin () = posix_getlogin_ ()
    fun getpgrp  () = posix_getpgrp_ ()
    fun setsid   () = posix_setsid_ ()

    fun setpgid {pid = pidopt, pgid = pgidopt} =
        posix_setpgid_ (case pidopt  of NONE => 0 | SOME p => p)
                       (case pgidopt of NONE => 0 | SOME p => p)

    fun uname () =
        let val (machine, sysname, release) = uname_ ()
        in [("sysname", sysname), ("nodename", ""),
            ("release", release), ("version", ""), ("machine", machine)] end

    fun time () = Time.fromReal (real (posix_time_ ()))

    fun times () =
        let val (ut, st, cut, cst, el) = posix_times_ ()
            val hz = real (posix_clktck_ ())
        in { utime   = Time.fromReal (real ut  / hz),
             stime   = Time.fromReal (real st  / hz),
             cutime  = Time.fromReal (real cut / hz),
             cstime  = Time.fromReal (real cst / hz),
             elapsed = Time.fromReal (real el  / hz) } end

    fun getenv s = OS.Process.getEnv s
    fun environ () = posix_environ_ ()
    fun ctermid () = posix_ctermid_ ()
    fun ttyname fd = posix_ttyname_ fd
    fun isatty  fd = posix_isatty_  fd
    fun sysconf s  = Word.fromInt (posix_sysconf_ s)
  end

  (* ------------------------------------------------------------------ *)
  structure FileSys = struct
    type uid       = ProcEnv.uid
    type gid       = ProcEnv.gid
    type file_desc = ProcEnv.file_desc

    fun fdToWord (fd : file_desc) : word = Word.fromInt fd
    fun wordToFD (w : word) : file_desc  = Word.toInt w

    (* OS.IO.iodesc stubs — mosml doesn't expose real iodesc *)
    fun fdToIOD fd = raise Fail "fdToIOD: not supported"
    fun iodToFD iod = NONE

    type dirstream = OS.FileSys.dirstream

    val opendir   = OS.FileSys.openDir
    fun readdir d  = OS.FileSys.readDir d
    val rewinddir = OS.FileSys.rewindDir
    val closedir  = OS.FileSys.closeDir
    val chdir     = OS.FileSys.chDir
    val getcwd    = OS.FileSys.getDir

    val stdin  : file_desc = 0
    val stdout : file_desc = 1
    val stderr : file_desc = 2

    (* S — file permission mode bits (octal values) *)
    structure S = struct
      type mode  = word
      type flags = mode
      fun flags (l : flags list) = List.foldl Word.orb 0w0 l
      fun toWord   (f : flags) : word = f
      fun fromWord (w : word) : flags = w
      fun intersect l = List.foldl Word.andb (Word.notb 0w0) l
      fun clear (a, b) = Word.andb(a, Word.notb b)
      fun allSet (a, b) = Word.andb(a, b) = b
      fun anySet (a, b) = Word.andb(a, b) <> 0w0
      val irwxu : mode = 0w0448  (* 0700 octal *)
      val irusr : mode = 0w0256  (* 0400 *)
      val iwusr : mode = 0w0128  (* 0200 *)
      val ixusr : mode = 0w0064  (* 0100 *)
      val irwxg : mode = 0w0056  (* 0070 *)
      val irgrp : mode = 0w0032  (* 0040 *)
      val iwgrp : mode = 0w0016  (* 0020 *)
      val ixgrp : mode = 0w0008  (* 0010 *)
      val irwxo : mode = 0w0007  (* 0007 *)
      val iroth : mode = 0w0004  (* 0004 *)
      val iwoth : mode = 0w0002  (* 0002 *)
      val ixoth : mode = 0w0001  (* 0001 *)
      val isuid : mode = 0w2048  (* 04000 *)
      val isgid : mode = 0w1024  (* 02000 *)
    end

    (* FD — file descriptor flags *)
    structure FD = struct
      type flags = word
      fun flags (l : flags list) = List.foldl Word.orb 0w0 l
      fun toWord   (f : flags) : word = f
      fun fromWord (w : word) : flags = w
      fun intersect l = List.foldl Word.andb (Word.notb 0w0) l
      fun clear (a, b) = Word.andb(a, Word.notb b)
      fun allSet (a, b) = Word.andb(a, b) = b
      fun anySet (a, b) = Word.andb(a, b) <> 0w0
      val cloexec : flags = 0w1  (* FD_CLOEXEC *)
    end

    (* O — file status flags for open *)
    structure O = struct
      type flags = word
      fun flags (l : flags list) = List.foldl Word.orb 0w0 l
      fun toWord   (f : flags) : word = f
      fun fromWord (w : word) : flags = w
      fun intersect l = List.foldl Word.andb (Word.notb 0w0) l
      fun clear (a, b) = Word.andb(a, Word.notb b)
      fun allSet (a, b) = Word.andb(a, b) = b
      fun anySet (a, b) = Word.andb(a, b) <> 0w0
      val append   : flags = 0w1024   (* O_APPEND   = 0x400 *)
      val excl     : flags = 0w128    (* O_EXCL     = 0x80  *)
      val noctty   : flags = 0w256    (* O_NOCTTY   = 0x100 *)
      val nonblock : flags = 0w2048   (* O_NONBLOCK = 0x800 *)
      val sync     : flags = 0w1052672 (* O_SYNC    = 0x101000 *)
      val trunc    : flags = 0w512    (* O_TRUNC    = 0x200 *)
    end

    datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR

    fun openModeInt m = case m of O_RDONLY => 0 | O_WRONLY => 1 | O_RDWR => 2

    fun openf (path, mode, oflags) =
        sysErr (fn () =>
          posix_openf_ path (Word.toInt (Word.orb (Word.fromInt (openModeInt mode), oflags))))

    fun createf (path, mode, oflags, perm) =
        sysErr (fn () =>
          posix_createf_ path
            (Word.toInt (Word.orb (Word.orb (Word.fromInt (openModeInt mode), oflags), 0w64)))
            (Word.toInt perm))

    fun creat (path, perm) = sysErr (fn () => posix_creat_ path (Word.toInt perm))

    fun umask m = Word.fromInt (posix_umask_ (Word.toInt m))

    fun link {old, new}   = sysErr (fn () => posix_link_ old new)
    fun mkdir (path, m)   = sysErr (fn () => posix_mkdir_ path (Word.toInt m))
    fun mkfifo (path, m)  = sysErr (fn () => posix_mkfifo_ path (Word.toInt m))
    fun unlink path       = sysErr (fn () => posix_unlink_ path)
    fun rmdir path        = sysErr (fn () => posix_rmdir_ path)
    fun rename {old, new} = sysErr (fn () => posix_rename_ old new)
    fun symlink {old, new}= sysErr (fn () => posix_symlink_ old new)
    fun readlink path     = readlink_ path

    type dev = word
    fun wordToDev w = w
    fun devToWord d = d

    type ino = word
    fun wordToIno w = w
    fun inoToWord i = i

    (* stat record: (mode, ino, dev, nlink, uid, gid, size) *)
    structure ST = struct
      type stat = { mode : word, ino : word, dev : word, nlink : int,
                    uid : int,   gid : int,  size : int,
                    atime : int, mtime : int, ctime : int }
      fun isDir  (st : stat) = Word.andb(#mode st, 0wxF000) = 0wx4000
      fun isChr  (st : stat) = Word.andb(#mode st, 0wxF000) = 0wx2000
      fun isBlk  (st : stat) = Word.andb(#mode st, 0wxF000) = 0wx6000
      fun isReg  (st : stat) = Word.andb(#mode st, 0wxF000) = 0wx8000
      fun isFIFO (st : stat) = Word.andb(#mode st, 0wxF000) = 0wx1000
      fun isLink (st : stat) = Word.andb(#mode st, 0wxF000) = 0wxA000
      fun isSock (st : stat) = Word.andb(#mode st, 0wxF000) = 0wxC000
      fun mode   (st : stat) : S.mode = Word.andb(#mode st, 0w4095)
      fun ino    (st : stat) = #ino   st
      fun dev    (st : stat) = #dev   st
      fun nlink  (st : stat) = #nlink st
      fun uid    (st : stat) = #uid   st
      fun gid    (st : stat) = #gid   st
      fun size   (st : stat) = #size  st
      fun atime  (st : stat) = Time.fromReal (real (#atime st))
      fun mtime  (st : stat) = Time.fromReal (real (#mtime st))
      fun ctime  (st : stat) = Time.fromReal (real (#ctime st))
    end

    fun mkStat (m, i, d, nl, u, g, sz, at, mt, ct) : ST.stat =
        { mode  = Word.fromInt m,
          ino   = Word.fromInt i,
          dev   = Word.fromInt d,
          nlink = nl,
          uid   = u,
          gid   = g,
          size  = sz,
          atime = at,
          mtime = mt,
          ctime = ct }

    fun stat  path = sysErr (fn () => mkStat (posix_stat_  path))
    fun lstat path = sysErr (fn () => mkStat (posix_lstat_ path))
    fun fstat fd   = sysErr (fn () => mkStat (posix_fstat_ fd))

    datatype access_mode = A_READ | A_WRITE | A_EXEC

    fun access (path, modes) =
        let fun toBits []              acc = acc
              | toBits (A_READ  :: r) acc = toBits r (acc + 1)
              | toBits (A_WRITE :: r) acc = toBits r (acc + 2)
              | toBits (A_EXEC  :: r) acc = toBits r (acc + 4)
            val bits = toBits modes 0
        in posix_access_ path bits end

    fun chmod  (path, m)     = sysErr (fn () => posix_chmod_  path (Word.toInt m))
    fun fchmod (fd, m)       = sysErr (fn () => posix_fchmod_ fd   (Word.toInt m))
    fun chown  (path, u, g)  = sysErr (fn () => posix_chown_  path u g)
    fun fchown (fd, u, g)    = sysErr (fn () => posix_fchown_ fd   u g)
    fun ftruncate (fd, n)    = sysErr (fn () => posix_ftruncate_ fd n)

    fun pathconf  (path, prop) =
        case posix_pathconf_ path prop of
          NONE   => NONE
        | SOME v => SOME (Word.fromInt v)

    fun fpathconf (fd, prop) =
        case posix_fpathconf_ fd prop of
          NONE   => NONE
        | SOME v => SOME (Word.fromInt v)
  end

  (* ------------------------------------------------------------------ *)
  structure IO = struct
    type file_desc = FileSys.file_desc
    type pid       = Process.pid
    type open_mode = FileSys.open_mode

    datatype whence = SEEK_SET | SEEK_CUR | SEEK_END

    structure FD = struct
      type flags = word
      fun flags (l : flags list) = List.foldl Word.orb 0w0 l
      fun toWord   (f : flags) : word = f
      fun fromWord (w : word) : flags = w
      fun intersect l = List.foldl Word.andb (Word.notb 0w0) l
      fun clear (a, b) = Word.andb(a, Word.notb b)
      fun allSet (a, b) = Word.andb(a, b) = b
      fun anySet (a, b) = Word.andb(a, b) <> 0w0
      val cloexec : flags = 0w1
    end

    structure O = struct
      type flags = word
      fun flags (l : flags list) = List.foldl Word.orb 0w0 l
      fun toWord   (f : flags) : word = f
      fun fromWord (w : word) : flags = w
      fun intersect l = List.foldl Word.andb (Word.notb 0w0) l
      fun clear (a, b) = Word.andb(a, Word.notb b)
      fun allSet (a, b) = Word.andb(a, b) = b
      fun anySet (a, b) = Word.andb(a, b) <> 0w0
      val append   : flags = 0w1024
      val nonblock : flags = 0w2048
      val sync     : flags = 0w1052672
    end

    fun close fd = sysErr (fn () => posix_close_ fd)
    fun dup   fd = sysErr (fn () => posix_dup_   fd)
    fun dup2  {old, new} = sysErr (fn () => posix_dup2_ old new)
    fun dupfd {old, base} = sysErr (fn () => posix_dupfd_ old base)
    fun pipe  () =
        sysErr (fn () =>
          let val (i, o_) = posix_pipe_ ()
          in {infd = i, outfd = o_} end)

    fun getfd fd      = sysErr (fn () => Word.fromInt (posix_getfd_ fd))
    fun setfd (fd, f) = sysErr (fn () => posix_setfd_ fd (Word.toInt f))

    fun setfl (fd, f) = sysErr (fn () => posix_setfl_ fd (Word.toInt f))

    fun getfl fd =
        let val r = posix_getfl_ fd
            val mode = case Word.andb(Word.fromInt r, 0w3) of
                         0w0 => FileSys.O_RDONLY
                       | 0w1 => FileSys.O_WRONLY
                       | _   => FileSys.O_RDWR
            val oflags = Word.andb(Word.fromInt r, Word.notb 0w3)
        in (oflags, mode) end

    fun whenceInt w = case w of SEEK_SET => 0 | SEEK_CUR => 1 | SEEK_END => 2

    fun lseek (fd, off, wh) = sysErr (fn () => posix_lseek_ fd off (whenceInt wh))

    fun readVec (fd, n) = sysErr (fn () => posix_read_ fd n)

    fun readArr (fd, slice) =
        sysErr (fn () =>
          let val (arr, ofs, len) = Word8ArraySlice.base slice
              val v = posix_read_ fd len
              val n = Word8Vector.length v
              val _ = Word8Array.copyVec {src = v, dst = arr, di = ofs}
          in n end)

    fun writeVec (fd, slice) =
        sysErr (fn () =>
          let val (vec, ofs, len) = Word8VectorSlice.base slice
          in posix_write_ fd vec ofs len end)

    fun writeArr (fd, slice) =
        sysErr (fn () =>
          let val vec = Word8ArraySlice.vector slice
          in posix_write_ fd vec 0 (Word8Vector.length vec) end)
  end

  (* ------------------------------------------------------------------ *)
  structure SysDB = struct
    type uid = ProcEnv.uid
    type gid = ProcEnv.gid

    structure Passwd = struct
      type passwd = { name : string, uid : uid, gid : gid,
                      home : string, shell : string }
      fun name  (p : passwd) = #name  p
      fun uid   (p : passwd) = #uid   p
      fun gid   (p : passwd) = #gid   p
      fun home  (p : passwd) = #home  p
      fun shell (p : passwd) = #shell p
    end

    structure Group = struct
      type group = { name : string, gid : gid, members : string list }
      fun name    (g : group) = #name    g
      fun gid     (g : group) = #gid     g
      fun members (g : group) = #members g
    end

    fun mkPasswd (name, uid, gid, home, shell) : Passwd.passwd =
        { name = name, uid = uid, gid = gid, home = home, shell = shell }

    fun mkGroup (name, gid, members) : Group.group =
        { name = name, gid = gid, members = members }

    fun getpwuid u = mkPasswd (posix_getpwuid_ u)
    fun getpwnam s = mkPasswd (posix_getpwnam_ s)
    fun getgrgid g = mkGroup  (posix_getgrgid_ g)
    fun getgrnam s = mkGroup  (posix_getgrnam_ s)
  end

end (* local *)

end (* structure Posix *)
