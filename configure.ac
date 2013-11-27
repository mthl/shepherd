dnl Process this file with autoconf to procude a configure script.

AC_PREREQ(2.69)

dnl Initialization
AC_INIT([GNU dmd], [0.1], [bug-guix@gnu.org])
AC_CONFIG_SRCDIR([modules/dmd.scm])
AC_CONFIG_AUX_DIR([build-aux])

dnl We're fine with GNU make constructs, hence '-Wno-portability'.
AM_INIT_AUTOMAKE([1.11 gnu silent-rules -Wall -Wno-portability])

AC_CANONICAL_HOST
AC_PROG_MKDIR_P
AC_PROG_SED

dnl Checks for programs.
GUILE_PROGS

AC_MSG_CHECKING([whether it is Guile 2.0])
GUILE_CHECK(version_ok,
(or (and (>= (string->number (major-version)) 2)
         (>= (string->number (major-version)) 0))
    (quit 1))
)
if test $version_ok = 0; then
  recent_version=yes
else
  recent_version=no
fi
AC_MSG_RESULT($recent_version)
if test $recent_version = no; then
  AC_MSG_WARN(your Guile is probably too old to run dmd)
fi

AC_PATH_PROG([GUILD], [guild])
if test "x$GUILD" = "x"; then
   AC_MSG_ERROR(['guild' binary not found; please check your Guile 2.x installation.])
fi
AC_SUBST([GUILD])

dnl Low-level operating system interface.
AC_PROG_CC
AC_MSG_CHECKING([<sys/reboot.h> constants])
AC_COMPUTE_INT([RB_AUTOBOOT], [RB_AUTOBOOT], [#include <sys/reboot.h>])
case "$host_os" in
  linux-gnu*)
    AC_COMPUTE_INT([RB_HALT_SYSTEM], [RB_HALT_SYSTEM], [#include <sys/reboot.h>])
    AC_COMPUTE_INT([RB_ENABLE_CAD], [RB_ENABLE_CAD], [#include <sys/reboot.h>])
    AC_COMPUTE_INT([RB_DISABLE_CAD], [RB_DISABLE_CAD], [#include <sys/reboot.h>])
    AC_COMPUTE_INT([RB_POWER_OFF], [RB_POWER_OFF], [#include <sys/reboot.h>])
    AC_COMPUTE_INT([RB_SW_SUSPEND], [RB_SW_SUSPEND], [#include <sys/reboot.h>])
    ;;
  gnu*)
    # On GNU/Hurd, the Mach-derived reboot.h uses different names, and
    # has a different set of features and flags.
    AC_COMPUTE_INT([RB_HALT_SYSTEM], [RB_HALT], [#include <sys/reboot.h>])
    AC_COMPUTE_INT([RB_POWER_OFF], [RB_HALT], [#include <sys/reboot.h>])
    ;;
  *)
    # What is this?  GNU/kFreeBSD?
    AC_MSG_WARN([not sure how to halt the system on '$host_os'])
    AC_COMPUTE_INT([RB_HALT_SYSTEM], [RB_HALT_SYSTEM], [#include <sys/reboot.h>])
    AC_COMPUTE_INT([RB_POWER_OFF], [RB_HALT_SYSTEM], [#include <sys/reboot.h>])
    ;;
esac

AC_SUBST([RB_AUTOBOOT])
AC_SUBST([RB_HALT_SYSTEM])
AC_SUBST([RB_POWER_OFF])

AC_MSG_RESULT([done])


dnl Finish.
AC_CONFIG_FILES([Makefile
                 examples/Makefile
		 utils/Makefile
		 modules/dmd/system.scm])

AC_OUTPUT
