(* Posix -- SML Basis Library, POSIX 1003.1 interface *)

signature Posix = sig

  (* ------------------------------------------------------------------ *)
  (* Error                                                               *)
  (* ------------------------------------------------------------------ *)
  structure Error : sig
    type syserror = int   (* errno value *)

    val toWord   : syserror -> word
    val fromWord : word -> syserror

    val errorMsg  : syserror -> string
    val errorName : syserror -> string
    val syserror  : string -> syserror option

    val acces       : syserror
    val again       : syserror
    val badf        : syserror
    val badmsg      : syserror
    val busy        : syserror
    val canceled    : syserror
    val child       : syserror
    val deadlk      : syserror
    val dom         : syserror
    val exist       : syserror
    val fault       : syserror
    val fbig        : syserror
    val inprogress  : syserror
    val intr        : syserror
    val inval       : syserror
    val io          : syserror
    val isdir       : syserror
    val loop        : syserror
    val mfile       : syserror
    val mlink       : syserror
    val msgsize     : syserror
    val nametoolong : syserror
    val nfile       : syserror
    val nodev       : syserror
    val noent       : syserror
    val noexec      : syserror
    val nolck       : syserror
    val nomem       : syserror
    val nospc       : syserror
    val nosys       : syserror
    val notdir      : syserror
    val notempty    : syserror
    val notsup      : syserror
    val notty       : syserror
    val nxio        : syserror
    val perm        : syserror
    val pipe        : syserror
    val range       : syserror
    val rofs        : syserror
    val spipe       : syserror
    val srch        : syserror
    val toobig      : syserror
    val xdev        : syserror
  end

  (* ------------------------------------------------------------------ *)
  (* Signal                                                              *)
  (* ------------------------------------------------------------------ *)
  structure Signal : sig
    eqtype signal

    val toWord   : signal -> word
    val fromWord : word -> signal

    val abrt : signal
    val alrm : signal
    val bus  : signal
    val fpe  : signal
    val hup  : signal
    val ill  : signal
    val int  : signal
    val kill : signal
    val pipe : signal
    val quit : signal
    val segv : signal
    val term : signal
    val usr1 : signal
    val usr2 : signal
    val chld : signal
    val cont : signal
    val stop : signal
    val tstp : signal
    val ttin : signal
    val ttou : signal
  end

  (* ------------------------------------------------------------------ *)
  (* Process                                                             *)
  (* ------------------------------------------------------------------ *)
  structure Process : sig
    type signal = Signal.signal
    eqtype pid

    val wordToPid : word -> pid
    val pidToWord : pid -> word

    val fork  : unit -> pid option
    val exec  : string * string list -> 'a
    val exece : string * string list * string list -> 'a
    val execp : string * string list -> 'a

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

    val fromStatus : OS.Process.status -> exit_status

    structure W : sig
      type flags
      val flags     : flags list -> flags
      val toWord    : flags -> word
      val fromWord  : word -> flags
      val intersect : flags list -> flags
      val clear     : flags * flags -> flags
      val allSet    : flags * flags -> bool
      val anySet    : flags * flags -> bool
      val untraced  : flags
    end

    val wait       : unit -> pid * exit_status
    val waitpid    : waitpid_arg * W.flags list -> pid * exit_status
    val waitpid_nh : waitpid_arg * W.flags list -> (pid * exit_status) option
    val exit       : Word8.word -> 'a

    datatype killpid_arg
      = K_PROC of pid
      | K_SAME_GROUP
      | K_GROUP of pid

    val kill  : killpid_arg * signal -> unit
    val alarm : Time.time -> Time.time
    val pause : unit -> unit
    val sleep : Time.time -> Time.time
  end

  (* ------------------------------------------------------------------ *)
  (* ProcEnv                                                             *)
  (* ------------------------------------------------------------------ *)
  structure ProcEnv : sig
    type pid = Process.pid
    eqtype uid
    eqtype gid
    eqtype file_desc

    val uidToWord : uid -> word
    val wordToUid : word -> uid
    val gidToWord : gid -> word
    val wordToGid : word -> gid

    val getpid   : unit -> pid
    val getppid  : unit -> pid
    val getuid   : unit -> uid
    val geteuid  : unit -> uid
    val getgid   : unit -> gid
    val getegid  : unit -> gid
    val setuid   : uid -> unit
    val setgid   : gid -> unit
    val getgroups: unit -> gid list
    val getlogin : unit -> string
    val getpgrp  : unit -> pid
    val setsid   : unit -> pid
    val setpgid  : {pid : pid option, pgid : pid option} -> unit

    val uname    : unit -> (string * string) list
    val time     : unit -> Time.time
    val times    : unit -> { elapsed : Time.time,
                             utime   : Time.time,
                             stime   : Time.time,
                             cutime  : Time.time,
                             cstime  : Time.time }
    val getenv   : string -> string option
    val environ  : unit -> string list
    val ctermid  : unit -> string
    val ttyname  : file_desc -> string
    val isatty   : file_desc -> bool
    val sysconf  : string -> word
  end

  (* ------------------------------------------------------------------ *)
  (* FileSys                                                             *)
  (* ------------------------------------------------------------------ *)
  structure FileSys : sig
    type uid       = ProcEnv.uid        (* shared with ProcEnv *)
    type gid       = ProcEnv.gid
    type file_desc = ProcEnv.file_desc

    val fdToWord : file_desc -> word
    val wordToFD : word -> file_desc

    type dirstream

    val opendir   : string -> dirstream
    val readdir   : dirstream -> string option
    val rewinddir : dirstream -> unit
    val closedir  : dirstream -> unit
    val chdir     : string -> unit
    val getcwd    : unit -> string

    val stdin  : file_desc
    val stdout : file_desc
    val stderr : file_desc

    structure S : sig
      eqtype mode
      type flags = mode
      val flags     : flags list -> flags
      val toWord    : flags -> word
      val fromWord  : word -> flags
      val intersect : flags list -> flags
      val clear     : flags * flags -> flags
      val allSet    : flags * flags -> bool
      val anySet    : flags * flags -> bool
      val irwxu : mode
      val irusr : mode
      val iwusr : mode
      val ixusr : mode
      val irwxg : mode
      val irgrp : mode
      val iwgrp : mode
      val ixgrp : mode
      val irwxo : mode
      val iroth : mode
      val iwoth : mode
      val ixoth : mode
      val isuid : mode
      val isgid : mode
    end

    structure FD : sig
      type flags
      val flags     : flags list -> flags
      val toWord    : flags -> word
      val fromWord  : word -> flags
      val intersect : flags list -> flags
      val clear     : flags * flags -> flags
      val allSet    : flags * flags -> bool
      val anySet    : flags * flags -> bool
      val cloexec   : flags
    end

    structure O : sig
      type flags
      val flags     : flags list -> flags
      val toWord    : flags -> word
      val fromWord  : word -> flags
      val intersect : flags list -> flags
      val clear     : flags * flags -> flags
      val allSet    : flags * flags -> bool
      val anySet    : flags * flags -> bool
      val append    : flags
      val excl      : flags
      val noctty    : flags
      val nonblock  : flags
      val sync      : flags
      val trunc     : flags
    end

    datatype open_mode = O_RDONLY | O_WRONLY | O_RDWR

    val openf    : string * open_mode * O.flags -> file_desc
    val createf  : string * open_mode * O.flags * S.mode -> file_desc
    val creat    : string * S.mode -> file_desc
    val umask    : S.mode -> S.mode
    val link     : {old : string, new : string} -> unit
    val mkdir    : string * S.mode -> unit
    val mkfifo   : string * S.mode -> unit
    val unlink   : string -> unit
    val rmdir    : string -> unit
    val rename   : {old : string, new : string} -> unit
    val symlink  : {old : string, new : string} -> unit
    val readlink : string -> string

    eqtype dev
    val wordToDev : word -> dev
    val devToWord : dev -> word

    eqtype ino
    val wordToIno : word -> ino
    val inoToWord : ino -> word

    structure ST : sig
      type stat
      val isDir  : stat -> bool
      val isChr  : stat -> bool
      val isBlk  : stat -> bool
      val isReg  : stat -> bool
      val isFIFO : stat -> bool
      val isLink : stat -> bool
      val isSock : stat -> bool
      val mode   : stat -> S.mode
      val ino    : stat -> ino
      val dev    : stat -> dev
      val nlink  : stat -> int
      val uid    : stat -> uid
      val gid    : stat -> gid
      val size   : stat -> int
      val atime  : stat -> Time.time
      val mtime  : stat -> Time.time
      val ctime  : stat -> Time.time
    end

    val stat  : string -> ST.stat
    val lstat : string -> ST.stat
    val fstat : file_desc -> ST.stat

    datatype access_mode = A_READ | A_WRITE | A_EXEC

    val access    : string * access_mode list -> bool
    val chmod     : string * S.mode -> unit
    val fchmod    : file_desc * S.mode -> unit
    val chown     : string * uid * gid -> unit
    val fchown    : file_desc * uid * gid -> unit
    val ftruncate : file_desc * int -> unit
    val pathconf  : string * string -> word option
    val fpathconf : file_desc * string -> word option
  end

  (* ------------------------------------------------------------------ *)
  (* IO                                                                  *)
  (* ------------------------------------------------------------------ *)
  structure IO : sig
    type file_desc = FileSys.file_desc
    type pid = Process.pid

    datatype whence = SEEK_SET | SEEK_CUR | SEEK_END

    type open_mode = FileSys.open_mode  (* shared with FileSys *)

    structure FD : sig
      type flags
      val flags     : flags list -> flags
      val toWord    : flags -> word
      val fromWord  : word -> flags
      val intersect : flags list -> flags
      val clear     : flags * flags -> flags
      val allSet    : flags * flags -> bool
      val anySet    : flags * flags -> bool
      val cloexec   : flags
    end

    structure O : sig
      type flags
      val flags     : flags list -> flags
      val toWord    : flags -> word
      val fromWord  : word -> flags
      val intersect : flags list -> flags
      val clear     : flags * flags -> flags
      val allSet    : flags * flags -> bool
      val anySet    : flags * flags -> bool
      val append    : flags
      val nonblock  : flags
      val sync      : flags
    end

    val close  : file_desc -> unit
    val dup    : file_desc -> file_desc
    val dup2   : {old : file_desc, new : file_desc} -> unit
    val dupfd  : {old : file_desc, base : file_desc} -> file_desc
    val pipe   : unit -> {infd : file_desc, outfd : file_desc}
    val getfd  : file_desc -> FD.flags
    val setfd  : file_desc * FD.flags -> unit
    val setfl  : file_desc * O.flags -> unit
    val getfl  : file_desc -> O.flags * open_mode
    val lseek  : file_desc * int * whence -> int

    val readVec  : file_desc * int -> Word8Vector.vector
    val readArr  : file_desc * Word8ArraySlice.slice -> int
    val writeVec : file_desc * Word8VectorSlice.slice -> int
    val writeArr : file_desc * Word8ArraySlice.slice -> int
  end

  (* ------------------------------------------------------------------ *)
  (* SysDB                                                               *)
  (* ------------------------------------------------------------------ *)
  structure SysDB : sig
    type uid = ProcEnv.uid
    type gid = ProcEnv.gid

    structure Passwd : sig
      type passwd
      val name  : passwd -> string
      val uid   : passwd -> uid
      val gid   : passwd -> gid
      val home  : passwd -> string
      val shell : passwd -> string
    end

    structure Group : sig
      type group
      val name    : group -> string
      val gid     : group -> gid
      val members : group -> string list
    end

    val getgrgid : gid -> Group.group
    val getgrnam : string -> Group.group
    val getpwuid : uid -> Passwd.passwd
    val getpwnam : string -> Passwd.passwd
  end

end

(*

[structure Error] Symbolic names for POSIX errors.
[structure Signal] Symbolic names of POSIX signals.
[structure Process] POSIX process operations.
[structure ProcEnv] Access to POSIX process environment.
[structure FileSys] POSIX file system operations.
[structure IO] POSIX I/O operations.
[structure SysDB] POSIX user/group database.

*)
