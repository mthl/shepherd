# Makefile.am -- How to build and install dmd.
# Copyright (C) 2002, 2003 Wolfgang Jährling <wolfgang@pro-linux.de>
#
# This is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2, or (at your option)
# any later version.
#
# This file is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; see the file COPYING. If not, write to
# the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
# Boston, MA  02111-1307, USA.

# The main programs.
bin_SCRIPTS = dmd deco
sbin_SCRIPTS = halt reboot
templates =					\
  dmd.in deco.in halt.in reboot.in		\
  modules/dmd/config.scm.in			\
  modules/dmd/system.scm.in

# Build this module first.
BUILT_SOURCES = modules/dmd/config.scm

# The source files.
dmddir = ${datadir}/dmd
dist_dmd_DATA =					\
  modules/deco.scm				\
  modules/dmd.scm				\
  modules/halt.scm				\
  modules/reboot.scm
nodist_dmd_DATA = $(dist_dmd_DATA:%.scm=%.go)

dmdsubdir = $(dmddir)/dmd
dist_dmdsub_DATA =				\
  modules/dmd/args.scm				\
  modules/dmd/service.scm			\
  modules/dmd/support.scm			\
  modules/dmd/runlevel.scm			\
  modules/dmd/comm.scm
nodist_dmdsub_DATA =				\
  modules/dmd/config.scm			\
  modules/dmd/config.go				\
  modules/dmd/system.scm			\
  modules/dmd/system.go				\
  $(dist_dmdsub_DATA:%.scm=%.go)

CLEANFILES =					\
  $(nodist_dmd_DATA) $(nodist_dmdsub_DATA)	\
  $(bin_SCRIPTS) $(sbin_SCRIPTS)

# Documentation.
info_TEXINFOS = dmd.texi
SUBDIRS = examples utils

# Things not automatically included in the distribution.
EXTRA_DIST = $(templates) QUESTIONS fdl-1.3.texi ChangeLog-2003

# Create the socket directory (aka. 'default-socket-dir'.)  Make it
# accessible only by its owner since otherwise 'dmd' suggests using
# '--insecure'.  This is just a convenience, so allow it to fail.
install-data-local:
	-$(MKDIR_P) $(DESTDIR)$(localstatedir)/run/dmd
	-chmod 700 $(DESTDIR)$(localstatedir)/run/dmd

# Relocate the script---i.e., have them refer to the installed module
# directory.
install-exec-hook: install-executable-symlinks
	for script in						\
	   $(bin_SCRIPTS:%=$(DESTDIR)$(bindir)/%)		\
	   $(sbin_SCRIPTS:%=$(DESTDIR)$(sbindir)/%) ;		\
	do							\
	  $(SED) -e's|$(abs_top_srcdir)/modules|$(dmddir)|g'	\
	         -e's|$(abs_top_builddir)/modules|$(dmddir)|g'	\
	         -i $$script ;					\
	done

# Install 'shutdown' as a symlink to 'halt; XXX: someday we'll do
# better.
install-executable-symlinks:
	cd $(DESTDIR)$(sbindir); ln -s halt shutdown

# Remove the 'shutdown' symlink.
uninstall-hook:
	cd $(DESTDIR)$(sbindir); rm -f shutdown

# 'sed' expression to instantiate templates.
instantiate =						\
  -e 's,%PREFIX%,${prefix},g'				\
  -e 's,%modsrcdir%,${abs_top_srcdir}/modules,g'	\
  -e 's,%modbuilddir%,${abs_top_builddir}/modules,g'	\
  -e 's,%localstatedir%,${localstatedir},g'		\
  -e 's,%sysconfdir%,${sysconfdir},g'			\
  -e 's,%VERSION%,@VERSION@,g'				\
  -e 's,%PACKAGE_BUGREPORT%,@PACKAGE_BUGREPORT@,g'	\
  -e 's,%PACKAGE_NAME%,@PACKAGE_NAME@,g'		\
  -e 's,%PACKAGE_URL%,@PACKAGE_URL@,g'			\
  -e 's,%GUILE%,$(GUILE),g'

# Instantiate templates.

# Emit a 'chmod +x' command only if $@ matches one of the scripts.
maybe_executable =					\
   $(subst $@,chmod +x $@,				\
      $(findstring $@,$(bin_SCRIPTS) $(sbin_SCRIPTS)))

%: %.in Makefile
	$(MKDIR_P) "`dirname $@`"
	$(SED) $(instantiate) < $< >$@
	$(maybe_executable)

modules/dmd/config.scm: modules/dmd/config.scm.in Makefile
	$(MKDIR_P) "`dirname $@`"
	$(SED) $(instantiate) < $< >$@

# Make sure 'modules/dmd/{config,system.scm}' are built first.
# XXX: Use the C locale for when Guile lacks
# <http://git.sv.gnu.org/cgit/guile.git/commit/?h=stable-2.0&id=e2c6bf3866d1186c60bacfbd4fe5037087ee5e3f>.
%.go: %.scm $(templates:%.in=%)
	$(MKDIR_P) "`dirname "$@"`"
	LC_ALL=C					\
	$(GUILD) compile --target="$(host)"		\
	  -L "$(top_builddir)/modules"			\
	  -L "$(top_srcdir)/modules"			\
	  -Wformat -Wunbound-variable -Warity-mismatch	\
	  -o "$@" "$<"

SUFFIXES = .go

TESTS =						\
  tests/basic.sh				\
  tests/respawn.sh				\
  tests/no-home.sh				\
  tests/status-sexp.sh				\
  tests/sigint.sh

TEST_EXTENSIONS = .sh
EXTRA_DIST += $(TESTS)

AM_TESTS_ENVIRONMENT =				\
  PATH="$(abs_top_builddir):$$PATH" LC_ALL=C	\
  SHELL="$(SHELL)" GUILE="$(GUILE)"
  GUILE_LOAD_PATH="$(abs_top_srcdir)/modules:$(abs_top_builddir)/modules:$$GUILE_LOAD_PATH" \
  GUILE_LOAD_COMPILED_PATH="$(abs_top_srcdir)/modules:$(abs_top_builddir)/modules:$$GUILE_LOAD_COMPILED_PATH"

SH_LOG_COMPILER = $(SHELL)
AM_SH_LOG_FLAGS = -x -e

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
dmd_install_go_files = install-nobase_nodist_guilemoduleDATA
$(dmd_install_go_files): install-nobase_dist_guilemoduleDATA

gen-ChangeLog:
	if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    8da9cf2..HEAD > $(distdir)/cl-t;		\
	  rm -f $(distdir)/ChangeLog;			\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;	\
	fi

dist-hook: gen-ChangeLog

.PHONY: gen-ChangeLog
