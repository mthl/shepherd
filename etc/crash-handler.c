/* crash-handler.c -- PID 1 helper to dump core upon a crash on GNU/Linux.
   Copyright © 2019 Ludovic Courtès <ludo@gnu.org>

   This file is part of the GNU Shepherd.

   The GNU Shepherd is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or (at
   your option) any later version.

   The GNU Shepherd is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with the GNU Shepherd.  If not, see <http://www.gnu.org/licenses/>.  */

#define _GNU_SOURCE

#include <stdlib.h>
#include <unistd.h>
#include <sched.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/syscall.h>   /* For SYS_xxx definitions */
#include <signal.h>

/* Arrange to dump core.  */
static void
handle_crash (int sig)
{
  static const char msg[] = "Shepherd crashed!\n";
  write (2, msg, sizeof msg);

#ifdef __sparc__
  /* See 'raw_clone' in systemd.  */
# error "SPARC uses a different 'clone' syscall convention"
#endif

  /* Start a child process that will be able to dump core.  */
  pid_t pid = syscall (SYS_clone, SIGCHLD, NULL);
  if (pid < 0)
    abort ();

  if (pid == 0)
    {
      /* Restore the default signal handler to get a core dump.  */
      signal (sig, SIG_DFL);

      const struct rlimit infinity = { RLIM_INFINITY, RLIM_INFINITY };
      setrlimit (RLIMIT_CORE, &infinity);
      chdir ("/");

      int pid = syscall (SYS_getpid);
      kill (pid, sig);

      /* As it turns out, 'kill' simply returns without doing anything, which
	 is consistent with the "Notes" section of kill(2).  Thus, force a
	 crash.  */
      * (int *) 0 = 42;

      _exit (254);
    }
  else
    {
      /* Wait for the child process and terminate.  */
      signal (sig, SIG_IGN);

      int status;
      waitpid (pid, &status, 0);

      sync ();

      _exit (255);
    }

  _exit (253);
}

static void initialize_crash_handler (void)
  __attribute__ ((constructor));

static void
initialize_crash_handler (void)
{
  /* Register raw signal handlers.  This cannot be done in Scheme because the
     handler can only call async-signal-safe functions.  */
  signal (SIGSEGV, handle_crash);
  signal (SIGABRT, handle_crash);
}
