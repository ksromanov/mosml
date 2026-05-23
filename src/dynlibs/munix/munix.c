/* File mosml/src/dynlibs/munix/munix.c
   sestoft@dina.kvl.dk 1999-11-07 version 0.1
 */

/* General includes */

#include <errno.h>
#include <unistd.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <sys/times.h>
#include <sys/utsname.h>
#include <fcntl.h>
#include <time.h>
#include <pwd.h>
#include <grp.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Moscow ML includes */

#include <mlvalues.h>
#include <alloc.h>
#include <memory.h>
#include <fail.h>
#include <str.h>
#include <signals.h>

#ifdef WIN32
#define EXTERNML __declspec(dllexport)
#else
#define EXTERNML
#endif

void failure() {
  switch (errno) {
  case EFAULT : 
    failwith("EFAULT"); break;
  case EMFILE :
    failwith("EMFILE"); break;
  case ENFILE : 
    failwith("ENFILE"); break;
  case EAGAIN : 
    failwith("EAGAIN"); break;
  case ENOMEM : 
    failwith("ENOMEM"); break;
  case EACCES:
    failwith("EACCES"); break;
  case ENOEXEC:
    failwith("ENOEXEC"); break;
  case EPERM:
    failwith("EPERM"); break;
  case E2BIG:
    failwith("E2BIG"); break;
  case ENAMETOOLONG:
    failwith("ENAMETOOLONG"); break;
  case ENOENT:
    failwith("ENOENT"); break;
  case ENOTDIR:
    failwith("ENOTDIR"); break;
  case ELOOP:
    failwith("ELOOP"); break;
  case EIO:
    failwith("EIO"); break;
  case EINVAL:
    failwith("EINVAL"); break;
  case EISDIR:
    failwith("EISDIR"); break;
#ifdef ELIBBAD
  case ELIBBAD:
    failwith("ELIBBAD"); break;
#endif
  case ECHILD:
    failwith("ECHILD"); break;
  case EINTR:
    failwith("EINTR"); break;
  case ESRCH:
    failwith("ESRCH"); break;
  default:
    failwith("EUNSPECIFIED"); break;
  }
}

char** mkcharptrvec(value strvec) {
  int i;
  int argc = Wosize_val(strvec);
  char **argv = (char**) malloc((argc + 1) * sizeof(char*));
  if (argv == (char**)NULL)
    failwith("mkcharptrvec: malloc failed");
  for (i=0; i<argc; i++)
    argv[i] = String_val(Field(strvec, i));
  argv[argc] = (char*)NULL;
  return argv;
}

/* ML type: string -> string vector -> string vector option 
            -> int * int * int */
EXTERNML value unix_execute(value cmd, value args, value envopt) {
  int p2c[2];			      /* Pipe from parent to child */
  int c2p[2];			      /* Pipe from child to parent */
  int pid;
  char **argv = mkcharptrvec(args);

  if (pipe(p2c) < 0 || pipe(c2p) < 0)
    failure();
  pid = fork();
  if (pid < 0) 
    // In the parent process; fork failed
    failure();
  else if (pid > 0) {
    // In the parent process; fork succeeded
    value res = alloc_tuple(3);
    // printf("<%d>\n", pid); fflush();
    free(argv);
    close(c2p[1]);		      /* Close child's ends of pipes    */
    close(p2c[0]);
    Field(res, 0) = Val_long(pid); 
    Field(res, 1) = Val_long(c2p[0]); /* Parent reads from the c2p pipe */
    Field(res, 2) = Val_long(p2c[1]); /* Parent writes to the p2c pipe  */
    return res;
  } else { 
    // In the child process
    close(p2c[1]);		      /* Close parent's ends of pipes   */
    close(c2p[0]);
    dup2(p2c[0], 0 /* STD_IN  */);    /* Child stdin from the p2c pipe  */
    dup2(c2p[1], 1 /* STD_OUT */);    /* Child stdout to the c2p pipe   */
    if (envopt == NONE) 
      execv(String_val(cmd), argv);
    else {
      char **envv = mkcharptrvec(Field(envopt, 0));
      execve(String_val(cmd), argv, envv);
    }      
    printf("Could not exec %s\n", String_val(cmd));
    exit(1);
    // Never gets here
  }
    return Val_unit;
  }

/* ML type: int -> int */
EXTERNML value unix_waitpid(value pid) {
  int status;
  if (waitpid(Long_val(pid), &status, /* options = */ 0) < 0)
    failure();
  if (WIFEXITED(status)) 
    return Val_long(WEXITSTATUS(status));
  else
    return Val_long(-1);
}

/* ML type: int -> unit */
EXTERNML value unix_kill(value pid, value sig) {
  if (kill(Long_val(pid), Long_val(sig)) < 0)
    failure();
  return Val_unit;
}

/* ML type: unit -> int */
EXTERNML value unix_fork(value unit) {
  int pid;
  fflush(stdout);
  fflush(stderr);
  pid = fork();
  if (pid < 0)
    failure();
  return Val_long(pid);
}

/* ML type: int -> int * int */
EXTERNML value unix_waitpid_status(value vpid) {
  int status;
  value res;
  if (waitpid(Long_val(vpid), &status, 0) < 0)
    failure();
  res = alloc_tuple(2);
  if (WIFEXITED(status)) {
    Field(res, 0) = Val_long(0); /* exited */
    Field(res, 1) = Val_long(WEXITSTATUS(status));
  } else if (WIFSIGNALED(status)) {
    Field(res, 0) = Val_long(1); /* signaled */
    Field(res, 1) = Val_long(WTERMSIG(status));
  } else {
    Field(res, 0) = Val_long(2); /* stopped */
    Field(res, 1) = Val_long(WSTOPSIG(status));
  }
  return res;
}

/* ML type: unit -> int * int  (pid, exit_code) */
EXTERNML value unix_waitpid_any(value unit) {
  int status;
  value res;
  int pid = waitpid(-1, &status, 0);
  if (pid < 0)
    failure();
  res = alloc_tuple(2);
  Field(res, 0) = Val_long(pid);
  if (WIFEXITED(status))
    Field(res, 1) = Val_long(WEXITSTATUS(status));
  else if (WIFSIGNALED(status))
    Field(res, 1) = Val_long(128 + WTERMSIG(status));
  else
    Field(res, 1) = Val_long(-1);
  return res;
}

/* ML type: unit -> int */
EXTERNML value unix_getpid(value unit) {
  return Val_long(getpid());
}

/* ML type: int -> unit */
EXTERNML value unix_exit(value code) {
  _exit(Long_val(code));
  return Val_unit; /* not reached */
}


/* ================================================================
   Posix C functions
   ================================================================ */

static void posix_failure(void) {
  failwith(strerror(errno));
}

/* ---- Process ---- */

/* ML: string -> string vector -> unit  (never returns normally) */
EXTERNML value posix_execv(value vpath, value vargs) {
  char **argv = mkcharptrvec(vargs);
  execv(String_val(vpath), argv);
  free(argv);
  posix_failure();
  return Val_unit;
}

/* ML: string -> string vector -> unit */
EXTERNML value posix_execvp(value vfile, value vargs) {
  char **argv = mkcharptrvec(vargs);
  execvp(String_val(vfile), argv);
  free(argv);
  posix_failure();
  return Val_unit;
}

/* ML: string -> string vector -> string vector -> unit */
EXTERNML value posix_execve(value vpath, value vargs, value venv) {
  char **argv = mkcharptrvec(vargs);
  char **envv = mkcharptrvec(venv);
  execve(String_val(vpath), argv, envv);
  free(argv);
  free(envv);
  posix_failure();
  return Val_unit;
}

/* ML: unit -> int  (remaining seconds, 0 if no prior alarm) */
EXTERNML value posix_alarm(value vsec) {
  return Val_long((long)alarm((unsigned)Long_val(vsec)));
}

/* ML: unit -> unit  (suspends until signal) */
EXTERNML value posix_pause(value unit) {
  pause();
  return Val_unit;
}

/* ML: int -> int -> int * int  (sec, nsec) -> (rem_sec, rem_nsec) */
EXTERNML value posix_nanosleep(value vsec, value vnsec) {
  struct timespec req, rem;
  value res;
  req.tv_sec  = (time_t)Long_val(vsec);
  req.tv_nsec = (long)Long_val(vnsec);
  rem.tv_sec  = 0; rem.tv_nsec = 0;
  nanosleep(&req, &rem);
  res = alloc_tuple(2);
  Field(res, 0) = Val_long((long)rem.tv_sec);
  Field(res, 1) = Val_long((long)rem.tv_nsec);
  return res;
}

/* ML: int -> int * int  (pid) -> (kind, code)
   kind: 0=exited, 1=signaled, 2=stopped */
EXTERNML value posix_waitpid_status(value vpid) {
  int status;
  value res;
  if (waitpid((pid_t)Long_val(vpid), &status, 0) < 0)
    posix_failure();
  res = alloc_tuple(2);
  if (WIFEXITED(status)) {
    Field(res, 0) = Val_long(0);
    Field(res, 1) = Val_long((long)WEXITSTATUS(status));
  } else if (WIFSIGNALED(status)) {
    Field(res, 0) = Val_long(1);
    Field(res, 1) = Val_long((long)WTERMSIG(status));
  } else {
    Field(res, 0) = Val_long(2);
    Field(res, 1) = Val_long((long)WSTOPSIG(status));
  }
  return res;
}

/* ML: int -> int -> int * int * int  (pid, flags) -> (r, kind, code)
   r=0 means no child ready (NONE), r>0 means child reaped (SOME)
   kind: 0=exited, 1=signaled, 2=stopped; code is exit/signal code */
EXTERNML value posix_waitpid_nh(value vpid, value vflags) {
  int status;
  pid_t r;
  value res;
  r = waitpid((pid_t)Long_val(vpid), &status, WNOHANG | (int)Long_val(vflags));
  if (r < 0) posix_failure();
  res = alloc_tuple(3);
  Field(res, 0) = Val_long((long)r);
  if (r == 0) {
    Field(res, 1) = Val_long(0);
    Field(res, 2) = Val_long(0);
  } else if (WIFEXITED(status)) {
    Field(res, 1) = Val_long(0);
    Field(res, 2) = Val_long((long)WEXITSTATUS(status));
  } else if (WIFSIGNALED(status)) {
    Field(res, 1) = Val_long(1);
    Field(res, 2) = Val_long((long)WTERMSIG(status));
  } else {
    Field(res, 1) = Val_long(2);
    Field(res, 2) = Val_long((long)WSTOPSIG(status));
  }
  return res;
}

/* ML: unit -> int * int * int  (pid, kind, code) */
EXTERNML value posix_waitpid_any(value unit) {
  int status;
  pid_t pid;
  value res;
  pid = waitpid(-1, &status, 0);
  if (pid < 0) posix_failure();
  res = alloc_tuple(3);
  Field(res, 0) = Val_long((long)pid);
  if (WIFEXITED(status)) {
    Field(res, 1) = Val_long(0);
    Field(res, 2) = Val_long((long)WEXITSTATUS(status));
  } else if (WIFSIGNALED(status)) {
    Field(res, 1) = Val_long(1);
    Field(res, 2) = Val_long((long)WTERMSIG(status));
  } else {
    Field(res, 1) = Val_long(2);
    Field(res, 2) = Val_long((long)WSTOPSIG(status));
  }
  return res;
}

/* ---- ProcEnv ---- */

EXTERNML value posix_getppid(value unit) {
  return Val_long((long)getppid());
}

EXTERNML value posix_getuid(value unit) {
  return Val_long((long)getuid());
}

EXTERNML value posix_geteuid(value unit) {
  return Val_long((long)geteuid());
}

EXTERNML value posix_getgid(value unit) {
  return Val_long((long)getgid());
}

EXTERNML value posix_getegid(value unit) {
  return Val_long((long)getegid());
}

EXTERNML value posix_setuid(value vuid) {
  if (setuid((uid_t)Long_val(vuid)) < 0) posix_failure();
  return Val_unit;
}

EXTERNML value posix_setgid(value vgid) {
  if (setgid((gid_t)Long_val(vgid)) < 0) posix_failure();
  return Val_unit;
}

/* ML: unit -> int list */
EXTERNML value posix_getgroups(value unit) {
  int n, i;
  gid_t *buf;
  value lst;
  Push_roots(r, 1);
  n = getgroups(0, NULL);
  if (n < 0) { Pop_roots(); posix_failure(); }
  buf = (gid_t*)malloc((n+1) * sizeof(gid_t));
  if (!buf) { Pop_roots(); failwith("ENOMEM"); }
  n = getgroups(n, buf);
  if (n < 0) { free(buf); Pop_roots(); posix_failure(); }
  r[0] = Val_int(0);
  for (i = n-1; i >= 0; i--) {
    value cell = alloc_tuple(2);
    Field(cell, 0) = Val_long((long)buf[i]);
    Field(cell, 1) = r[0];
    r[0] = cell;
  }
  free(buf);
  lst = r[0];
  Pop_roots();
  return lst;
}

/* ML: unit -> string */
EXTERNML value posix_getlogin(value unit) {
  char *login = getlogin();
  if (!login) posix_failure();
  return copy_string(login);
}

EXTERNML value posix_getpgrp(value unit) {
  return Val_long((long)getpgrp());
}

EXTERNML value posix_setsid(value unit) {
  pid_t r = setsid();
  if (r < 0) posix_failure();
  return Val_long((long)r);
}

/* ML: int -> int -> unit  (pid, pgid; 0 = self) */
EXTERNML value posix_setpgid(value vpid, value vpgid) {
  if (setpgid((pid_t)Long_val(vpid), (pid_t)Long_val(vpgid)) < 0)
    posix_failure();
  return Val_unit;
}

/* ML: unit -> int  (seconds since epoch) */
EXTERNML value posix_time(value unit) {
  time_t t = time(NULL);
  if (t == (time_t)-1) posix_failure();
  return Val_long((long)t);
}

/* ML: unit -> int * int * int * int * int  (utime,stime,cutime,cstime,elapsed) clock ticks */
EXTERNML value posix_times(value unit) {
  struct tms buf;
  clock_t elapsed;
  value res;
  elapsed = times(&buf);
  res = alloc_tuple(5);
  Field(res, 0) = Val_long((long)buf.tms_utime);
  Field(res, 1) = Val_long((long)buf.tms_stime);
  Field(res, 2) = Val_long((long)buf.tms_cutime);
  Field(res, 3) = Val_long((long)buf.tms_cstime);
  Field(res, 4) = Val_long((long)elapsed);
  return res;
}

/* ML: unit -> int */
EXTERNML value posix_clktck(value unit) {
  return Val_long(sysconf(_SC_CLK_TCK));
}

/* ML: unit -> string list */
EXTERNML value posix_environ(value unit) {
  extern char **environ;
  int n, i;
  value lst;
  Push_roots(r, 2);
  for (n = 0; environ[n] != NULL; n++);
  r[0] = Val_int(0);
  for (i = n-1; i >= 0; i--) {
    r[1] = copy_string(environ[i]);
    { value cell = alloc_tuple(2);
      Field(cell, 0) = r[1];
      Field(cell, 1) = r[0];
      r[0] = cell; }
  }
  lst = r[0];
  Pop_roots();
  return lst;
}

/* ML: unit -> string */
EXTERNML value posix_ctermid(value unit) {
  char buf[L_ctermid + 1];
  ctermid(buf);
  return copy_string(buf);
}

/* ML: int -> string */
EXTERNML value posix_ttyname(value vfd) {
  char *name = ttyname((int)Long_val(vfd));
  if (!name) posix_failure();
  return copy_string(name);
}

/* ML: int -> bool */
EXTERNML value posix_isatty(value vfd) {
  return Val_bool(isatty((int)Long_val(vfd)));
}

/* ML: string -> int  (sysconf property name -> value) */
EXTERNML value posix_sysconf(value vname) {
  const char *name = String_val(vname);
  int sc;
  long r;
  if      (strcmp(name, "ARG_MAX")     == 0) sc = _SC_ARG_MAX;
  else if (strcmp(name, "CHILD_MAX")   == 0) sc = _SC_CHILD_MAX;
  else if (strcmp(name, "CLK_TCK")     == 0) sc = _SC_CLK_TCK;
  else if (strcmp(name, "NGROUPS_MAX") == 0) sc = _SC_NGROUPS_MAX;
  else if (strcmp(name, "OPEN_MAX")    == 0) sc = _SC_OPEN_MAX;
  else if (strcmp(name, "STREAM_MAX")  == 0) sc = _SC_STREAM_MAX;
  else if (strcmp(name, "TZNAME_MAX")  == 0) sc = _SC_TZNAME_MAX;
  else if (strcmp(name, "JOB_CONTROL") == 0) sc = _SC_JOB_CONTROL;
  else if (strcmp(name, "SAVED_IDS")   == 0) sc = _SC_SAVED_IDS;
  else if (strcmp(name, "VERSION")     == 0) sc = _SC_VERSION;
  else failwith("sysconf: unknown property");
  errno = 0;
  r = sysconf(sc);
  if (r == -1 && errno != 0) posix_failure();
  return Val_long(r);
}

/* ---- FileSys ---- */

/* ML: string -> int * int * int * int * int * int * int
   returns (mode, ino, dev, nlink, uid, gid, size) */
EXTERNML value posix_stat(value vpath) {
  struct stat buf;
  value res;
  if (stat(String_val(vpath), &buf) < 0) posix_failure();
  res = alloc_tuple(10);
  Field(res, 0) = Val_long((long)buf.st_mode);
  Field(res, 1) = Val_long((long)buf.st_ino);
  Field(res, 2) = Val_long((long)buf.st_dev);
  Field(res, 3) = Val_long((long)buf.st_nlink);
  Field(res, 4) = Val_long((long)buf.st_uid);
  Field(res, 5) = Val_long((long)buf.st_gid);
  Field(res, 6) = Val_long((long)buf.st_size);
  Field(res, 7) = Val_long((long)buf.st_atime);
  Field(res, 8) = Val_long((long)buf.st_mtime);
  Field(res, 9) = Val_long((long)buf.st_ctime);
  return res;
}

EXTERNML value posix_lstat(value vpath) {
  struct stat buf;
  value res;
  if (lstat(String_val(vpath), &buf) < 0) posix_failure();
  res = alloc_tuple(10);
  Field(res, 0) = Val_long((long)buf.st_mode);
  Field(res, 1) = Val_long((long)buf.st_ino);
  Field(res, 2) = Val_long((long)buf.st_dev);
  Field(res, 3) = Val_long((long)buf.st_nlink);
  Field(res, 4) = Val_long((long)buf.st_uid);
  Field(res, 5) = Val_long((long)buf.st_gid);
  Field(res, 6) = Val_long((long)buf.st_size);
  Field(res, 7) = Val_long((long)buf.st_atime);
  Field(res, 8) = Val_long((long)buf.st_mtime);
  Field(res, 9) = Val_long((long)buf.st_ctime);
  return res;
}

EXTERNML value posix_fstat(value vfd) {
  struct stat buf;
  value res;
  if (fstat((int)Long_val(vfd), &buf) < 0) posix_failure();
  res = alloc_tuple(10);
  Field(res, 0) = Val_long((long)buf.st_mode);
  Field(res, 1) = Val_long((long)buf.st_ino);
  Field(res, 2) = Val_long((long)buf.st_dev);
  Field(res, 3) = Val_long((long)buf.st_nlink);
  Field(res, 4) = Val_long((long)buf.st_uid);
  Field(res, 5) = Val_long((long)buf.st_gid);
  Field(res, 6) = Val_long((long)buf.st_size);
  Field(res, 7) = Val_long((long)buf.st_atime);
  Field(res, 8) = Val_long((long)buf.st_mtime);
  Field(res, 9) = Val_long((long)buf.st_ctime);
  return res;
}

/* ML: string -> int -> int  (path, flags) -> fd
   flags = openModeInt | oflags (caller does the OR) */
EXTERNML value posix_openf(value vpath, value vflags) {
  int fd = open(String_val(vpath), (int)Long_val(vflags));
  if (fd < 0) posix_failure();
  return Val_long((long)fd);
}

/* ML: string -> int -> int -> int  (path, flags, mode) -> fd  [with O_CREAT] */
EXTERNML value posix_createf(value vpath, value vflags, value vmode) {
  int fd = open(String_val(vpath), (int)Long_val(vflags), (mode_t)Long_val(vmode));
  if (fd < 0) posix_failure();
  return Val_long((long)fd);
}

/* ML: string -> int -> int  (path, mode) -> fd */
EXTERNML value posix_creat(value vpath, value vmode) {
  int fd = open(String_val(vpath), O_WRONLY|O_CREAT|O_TRUNC, (mode_t)Long_val(vmode));
  if (fd < 0) posix_failure();
  return Val_long((long)fd);
}

/* ML: int -> int  (mode) -> old_mode */
EXTERNML value posix_umask(value vmode) {
  return Val_long((long)umask((mode_t)Long_val(vmode)));
}

/* ML: string -> string -> unit */
EXTERNML value posix_link(value vold, value vnew) {
  if (link(String_val(vold), String_val(vnew)) < 0) posix_failure();
  return Val_unit;
}

/* ML: string -> int -> unit */
EXTERNML value posix_mkdir(value vpath, value vmode) {
  if (mkdir(String_val(vpath), (mode_t)Long_val(vmode)) < 0) posix_failure();
  return Val_unit;
}

/* ML: string -> int -> unit */
EXTERNML value posix_mkfifo(value vpath, value vmode) {
  if (mkfifo(String_val(vpath), (mode_t)Long_val(vmode)) < 0) posix_failure();
  return Val_unit;
}

/* ML: string -> unit */
EXTERNML value posix_unlink(value vpath) {
  if (unlink(String_val(vpath)) < 0) posix_failure();
  return Val_unit;
}

/* ML: string -> unit */
EXTERNML value posix_rmdir(value vpath) {
  if (rmdir(String_val(vpath)) < 0) posix_failure();
  return Val_unit;
}

/* ML: string -> string -> unit */
EXTERNML value posix_rename(value vold, value vnew) {
  if (rename(String_val(vold), String_val(vnew)) < 0) posix_failure();
  return Val_unit;
}

/* ML: string -> string -> unit */
EXTERNML value posix_symlink(value vold, value vnew) {
  if (symlink(String_val(vold), String_val(vnew)) < 0) posix_failure();
  return Val_unit;
}

/* ML: string -> int -> unit */
EXTERNML value posix_chmod(value vpath, value vmode) {
  if (chmod(String_val(vpath), (mode_t)Long_val(vmode)) < 0) posix_failure();
  return Val_unit;
}

/* ML: int -> int -> unit */
EXTERNML value posix_fchmod(value vfd, value vmode) {
  if (fchmod((int)Long_val(vfd), (mode_t)Long_val(vmode)) < 0) posix_failure();
  return Val_unit;
}

/* ML: string -> int -> int -> unit  (path, uid, gid) */
EXTERNML value posix_chown(value vpath, value vuid, value vgid) {
  if (chown(String_val(vpath), (uid_t)Long_val(vuid), (gid_t)Long_val(vgid)) < 0)
    posix_failure();
  return Val_unit;
}

/* ML: int -> int -> int -> unit  (fd, uid, gid) */
EXTERNML value posix_fchown(value vfd, value vuid, value vgid) {
  if (fchown((int)Long_val(vfd), (uid_t)Long_val(vuid), (gid_t)Long_val(vgid)) < 0)
    posix_failure();
  return Val_unit;
}

/* ML: int -> int -> unit  (fd, size) */
EXTERNML value posix_ftruncate(value vfd, value vsize) {
  if (ftruncate((int)Long_val(vfd), (off_t)Long_val(vsize)) < 0) posix_failure();
  return Val_unit;
}

/* ML: string -> int -> bool  (path, amode_bits) -> bool
   amode_bits: 0=F_OK, 1=R_OK, 2=W_OK, 4=X_OK */
EXTERNML value posix_access(value vpath, value vmode) {
  int mode = (int)Long_val(vmode);
  int amode = 0;
  if (mode == 0) amode = F_OK;
  else {
    if (mode & 1) amode |= R_OK;
    if (mode & 2) amode |= W_OK;
    if (mode & 4) amode |= X_OK;
  }
  return Val_bool(access(String_val(vpath), amode) == 0);
}

static int pathconf_name(const char *name) {
  if      (strcmp(name, "CHOWN_RESTRICTED") == 0) return _PC_CHOWN_RESTRICTED;
  else if (strcmp(name, "LINK_MAX")         == 0) return _PC_LINK_MAX;
  else if (strcmp(name, "MAX_CANON")        == 0) return _PC_MAX_CANON;
  else if (strcmp(name, "MAX_INPUT")        == 0) return _PC_MAX_INPUT;
  else if (strcmp(name, "NAME_MAX")         == 0) return _PC_NAME_MAX;
  else if (strcmp(name, "NO_TRUNC")         == 0) return _PC_NO_TRUNC;
  else if (strcmp(name, "PATH_MAX")         == 0) return _PC_PATH_MAX;
  else if (strcmp(name, "PIPE_BUF")         == 0) return _PC_PIPE_BUF;
  else if (strcmp(name, "VDISABLE")         == 0) return _PC_VDISABLE;
#ifdef _PC_ASYNC_IO
  else if (strcmp(name, "ASYNC_IO")         == 0) return _PC_ASYNC_IO;
#endif
#ifdef _PC_SYNC_IO
  else if (strcmp(name, "SYNC_IO")          == 0) return _PC_SYNC_IO;
#endif
#ifdef _PC_PRIO_IO
  else if (strcmp(name, "PRIO_IO")          == 0) return _PC_PRIO_IO;
#endif
  else return -1;
}

/* ML: string -> string -> int option  (path, prop) -> SOME val | NONE */
EXTERNML value posix_pathconf(value vpath, value vprop) {
  int pc = pathconf_name(String_val(vprop));
  long r;
  value res;
  if (pc < 0) failwith("pathconf: unknown property");
  errno = 0;
  r = pathconf(String_val(vpath), pc);
  if (r == -1) {
    if (errno != 0) posix_failure();
    return Val_int(0); /* NONE */
  }
  res = alloc_tuple(1);
  Field(res, 0) = Val_long(r);
  return res; /* SOME r */
}

/* ML: int -> string -> int option  (fd, prop) -> SOME val | NONE */
EXTERNML value posix_fpathconf(value vfd, value vprop) {
  int pc = pathconf_name(String_val(vprop));
  long r;
  value res;
  if (pc < 0) failwith("fpathconf: unknown property");
  errno = 0;
  r = fpathconf((int)Long_val(vfd), pc);
  if (r == -1) {
    if (errno != 0) posix_failure();
    return Val_int(0); /* NONE */
  }
  res = alloc_tuple(1);
  Field(res, 0) = Val_long(r);
  return res;
}

/* ---- IO ---- */

/* ML: int -> unit */
EXTERNML value posix_close(value vfd) {
  if (close((int)Long_val(vfd)) < 0) posix_failure();
  return Val_unit;
}

/* ML: int -> int */
EXTERNML value posix_dup(value vfd) {
  int r = dup((int)Long_val(vfd));
  if (r < 0) posix_failure();
  return Val_long((long)r);
}

/* ML: int -> int -> unit */
EXTERNML value posix_dup2(value vold, value vnew) {
  if (dup2((int)Long_val(vold), (int)Long_val(vnew)) < 0) posix_failure();
  return Val_unit;
}

/* ML: int -> int -> int  (old, base) -> new_fd  [fcntl F_DUPFD] */
EXTERNML value posix_dupfd(value vold, value vbase) {
  int r = fcntl((int)Long_val(vold), F_DUPFD, (int)Long_val(vbase));
  if (r < 0) posix_failure();
  return Val_long((long)r);
}

/* ML: unit -> int * int  (infd, outfd) */
EXTERNML value posix_pipe(value unit) {
  int fds[2];
  value res;
  if (pipe(fds) < 0) posix_failure();
  res = alloc_tuple(2);
  Field(res, 0) = Val_long((long)fds[0]);
  Field(res, 1) = Val_long((long)fds[1]);
  return res;
}

/* ML: int -> int  (fd) -> FD flags */
EXTERNML value posix_getfd(value vfd) {
  int r = fcntl((int)Long_val(vfd), F_GETFD);
  if (r < 0) posix_failure();
  return Val_long((long)r);
}

/* ML: int -> int -> unit  (fd, flags) */
EXTERNML value posix_setfd(value vfd, value vflags) {
  if (fcntl((int)Long_val(vfd), F_SETFD, (int)Long_val(vflags)) < 0)
    posix_failure();
  return Val_unit;
}

/* ML: int -> int  (fd) -> file status flags (includes open mode bits) */
EXTERNML value posix_getfl(value vfd) {
  int r = fcntl((int)Long_val(vfd), F_GETFL);
  if (r < 0) posix_failure();
  return Val_long((long)r);
}

/* ML: int -> int -> unit */
EXTERNML value posix_setfl(value vfd, value vflags) {
  if (fcntl((int)Long_val(vfd), F_SETFL, (int)Long_val(vflags)) < 0)
    posix_failure();
  return Val_unit;
}

/* ML: int -> int -> int -> int  (fd, offset, whence) -> new_pos */
EXTERNML value posix_lseek(value vfd, value voff, value vwhence) {
  static int whence_table[] = { SEEK_SET, SEEK_CUR, SEEK_END };
  int whence_idx = (int)Long_val(vwhence);
  off_t r;
  if (whence_idx < 0 || whence_idx > 2) failwith("lseek: bad whence");
  r = lseek((int)Long_val(vfd), (off_t)Long_val(voff), whence_table[whence_idx]);
  if (r == (off_t)-1) posix_failure();
  return Val_long((long)r);
}

/* ML: int -> int -> string  (fd, n) -> bytes  (Word8Vector) */
EXTERNML value posix_read(value vfd, value vn) {
  int fd = (int)Long_val(vfd);
  int n  = (int)Long_val(vn);
  char *buf;
  ssize_t nread;
  value res;
  if (n < 0) failwith("Size");
  if (n == 0) return copy_string("");
  buf = (char*)malloc(n);
  if (!buf) failwith("ENOMEM");
  nread = read(fd, buf, (size_t)n);
  if (nread < 0) { int e = errno; free(buf); failwith(strerror(e)); }
  res = alloc_string((mlsize_t)nread);
  memmove(String_val(res), buf, (size_t)nread);
  free(buf);
  return res;
}

/* ML: int -> string -> int -> int -> int  (fd, buf, ofs, len) -> bytes_written */
EXTERNML value posix_write(value vfd, value vbuf, value vofs, value vlen) {
  ssize_t r = write((int)Long_val(vfd),
                    String_val(vbuf) + Long_val(vofs),
                    (size_t)Long_val(vlen));
  if (r < 0) posix_failure();
  return Val_long((long)r);
}

/* ML: int -> string -> int -> int -> int  (fd, arr, ofs, len) -> bytes_read */
EXTERNML value posix_readarr(value vfd, value varr, value vofs, value vlen) {
  ssize_t r = read((int)Long_val(vfd),
                   String_val(varr) + Long_val(vofs),
                   (size_t)Long_val(vlen));
  if (r < 0) posix_failure();
  return Val_long((long)r);
}

/* ---- SysDB ---- */

/* ML: int -> string * int * int * string * string  (uid) -> (name,uid,gid,home,shell) */
EXTERNML value posix_getpwuid(value vuid) {
  struct passwd *pw = getpwuid((uid_t)Long_val(vuid));
  value res;
  Push_roots(r, 5);
  if (!pw) posix_failure();
  r[0] = copy_string(pw->pw_name);
  r[1] = Val_long((long)pw->pw_uid);
  r[2] = Val_long((long)pw->pw_gid);
  r[3] = copy_string(pw->pw_dir);
  r[4] = copy_string(pw->pw_shell);
  res = alloc_tuple(5);
  Field(res, 0) = r[0]; Field(res, 1) = r[1]; Field(res, 2) = r[2];
  Field(res, 3) = r[3]; Field(res, 4) = r[4];
  Pop_roots();
  return res;
}

/* ML: string -> string * int * int * string * string */
EXTERNML value posix_getpwnam(value vname) {
  struct passwd *pw = getpwnam(String_val(vname));
  value res;
  Push_roots(r, 5);
  if (!pw) posix_failure();
  r[0] = copy_string(pw->pw_name);
  r[1] = Val_long((long)pw->pw_uid);
  r[2] = Val_long((long)pw->pw_gid);
  r[3] = copy_string(pw->pw_dir);
  r[4] = copy_string(pw->pw_shell);
  res = alloc_tuple(5);
  Field(res, 0) = r[0]; Field(res, 1) = r[1]; Field(res, 2) = r[2];
  Field(res, 3) = r[3]; Field(res, 4) = r[4];
  Pop_roots();
  return res;
}

/* ML: int -> string * int * string list  (gid) -> (name, gid, members) */
EXTERNML value posix_getgrgid(value vgid) {
  struct group *gr = getgrgid((gid_t)Long_val(vgid));
  int n, i;
  value res;
  Push_roots(r, 3);
  if (!gr) posix_failure();
  r[0] = copy_string(gr->gr_name);
  r[1] = Val_long((long)gr->gr_gid);
  /* Build member list */
  for (n = 0; gr->gr_mem[n] != NULL; n++);
  r[2] = Val_int(0);
  for (i = n-1; i >= 0; i--) {
    value cell = alloc_tuple(2);
    value str  = copy_string(gr->gr_mem[i]);
    Field(cell, 0) = str;
    Field(cell, 1) = r[2];
    r[2] = cell;
  }
  res = alloc_tuple(3);
  Field(res, 0) = r[0]; Field(res, 1) = r[1]; Field(res, 2) = r[2];
  Pop_roots();
  return res;
}

/* ML: string -> string * int * string list */
EXTERNML value posix_getgrnam(value vname) {
  struct group *gr = getgrnam(String_val(vname));
  int n, i;
  value res;
  Push_roots(r, 3);
  if (!gr) posix_failure();
  r[0] = copy_string(gr->gr_name);
  r[1] = Val_long((long)gr->gr_gid);
  for (n = 0; gr->gr_mem[n] != NULL; n++);
  r[2] = Val_int(0);
  for (i = n-1; i >= 0; i--) {
    value cell = alloc_tuple(2);
    value str  = copy_string(gr->gr_mem[i]);
    Field(cell, 0) = str;
    Field(cell, 1) = r[2];
    r[2] = cell;
  }
  res = alloc_tuple(3);
  Field(res, 0) = r[0]; Field(res, 1) = r[1]; Field(res, 2) = r[2];
  Pop_roots();
  return res;
}

/* ---- Error ---- */

/* ML: int -> string  (errno value) -> strerror string */
EXTERNML value posix_strerror(value verrno) {
  return copy_string(strerror((int)Long_val(verrno)));
}
