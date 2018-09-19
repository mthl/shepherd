dnl Process this file with autoconf to procude a configure script.

AC_PREREQ(2.69)

dnl Initialization
AC_INIT([GNU Shepherd], [0.4.0], [bug-guix@gnu.org])
AC_CONFIG_SRCDIR([modules/shepherd.scm])
AC_CONFIG_AUX_DIR([build-aux])

dnl We're fine with GNU make constructs, hence '-Wno-portability'.
AM_INIT_AUTOMAKE([1.11 gnu silent-rules -Wall -Wno-portability
  std-options color-tests])

dnl Enable silent rules by default.
AM_SILENT_RULES([yes])

AM_GNU_GETTEXT([external])

AC_CANONICAL_HOST
AC_PROG_MKDIR_P
AC_PROG_SED

dnl Check for Guile 2.x.
m4_pattern_forbid([^GUILE_PKG$])
GUILE_PKG([2.2 2.0])

dnl Checks for programs.
GUILE_PROGS

if test "x$GUILE_EFFECTIVE_VERSION" = "x2.0"; then
  PKG_CHECK_MODULES([GUILE], [guile-2.0 >= 2.0.13])
fi

guilemoduledir="${datarootdir}/guile/site/$GUILE_EFFECTIVE_VERSION"
guileobjectdir="${libdir}/guile/$GUILE_EFFECTIVE_VERSION/site-ccache"
AC_SUBST([guilemoduledir])
AC_SUBST([guileobjectdir])

dnl
dnl Low-level operating system interface.
dnl

AC_PROG_CC

dnl In practice that value is fixed for all platforms in glibc, but better
dnl check it.
AC_MSG_CHECKING([the '_SC_OPEN_MAX' value])
AC_COMPUTE_INT([_SC_OPEN_MAX], [_SC_OPEN_MAX], [#include <unistd.h>])
AC_MSG_RESULT([$_SC_OPEN_MAX])
AC_SUBST([_SC_OPEN_MAX])

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
    RB_DISABLE_CAD="#f"
    ;;
  *)
    # What is this?  GNU/kFreeBSD?
    AC_MSG_WARN([not sure how to halt the system on '$host_os'])
    AC_COMPUTE_INT([RB_HALT_SYSTEM], [RB_HALT_SYSTEM], [#include <sys/reboot.h>])
    AC_COMPUTE_INT([RB_POWER_OFF], [RB_HALT_SYSTEM], [#include <sys/reboot.h>])
    RB_DISABLE_CAD="#f"
    ;;
esac

AC_SUBST([RB_DISABLE_CAD])
AC_SUBST([RB_AUTOBOOT])
AC_SUBST([RB_HALT_SYSTEM])
AC_SUBST([RB_POWER_OFF])
AC_MSG_RESULT([done])

AC_MSG_CHECKING([<sys/prctl.h> constants])
AC_COMPUTE_INT([PR_SET_CHILD_SUBREAPER], [PR_SET_CHILD_SUBREAPER], [#include <sys/prctl.h>])
AC_SUBST([PR_SET_CHILD_SUBREAPER])
AC_MSG_RESULT([done])

dnl Manual pages.
AM_MISSING_PROG([HELP2MAN], [help2man])

dnl Finish.
AC_CONFIG_FILES([Makefile po/Makefile.in
                 modules/shepherd/system.scm])

AC_OUTPUT
