dnl Process this file with autoconf to procude a configure script.

AC_PREREQ(2.68)

dnl Initialization
AC_INIT([GNU dmd], [-0.4], [bug-guix@gnu.org])
AC_CONFIG_SRCDIR([modules/dmd.scm])
AC_CONFIG_AUX_DIR([build-aux])

AM_INIT_AUTOMAKE([1.11 gnu silent-rules])

AC_PROG_MKDIR_P

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
  AC_MSG_WARN(your guile is probably too old to run dmd)
fi

dnl Finish.
AC_OUTPUT([Makefile examples/Makefile utils/Makefile])
