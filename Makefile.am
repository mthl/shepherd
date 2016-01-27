# Makefile.am -- How to build and install the Shepherd.
# Copyright © 2002, 2003 Wolfgang Jährling <wolfgang@pro-linux.de>
# Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
#
# This file is part of the GNU Shepherd.
#
# The GNU Shepherd is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or (at
# your option) any later version.
#
# The GNU Shepherd is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with the GNU Shepherd.  If not, see <http://www.gnu.org/licenses/>.

# The main programs.
bin_SCRIPTS = shepherd herd
sbin_SCRIPTS = halt reboot
templates =					\
  shepherd.in herd.in halt.in reboot.in		\
  modules/shepherd/config.scm.in		\
  modules/shepherd/system.scm.in

# Build this module first.
BUILT_SOURCES = modules/shepherd/config.scm

# The source files.
# XXX: we can't use 'nobase_' because of the modules/ subdirectory.
shepherddir = ${datadir}/shepherd
dist_shepherd_DATA =				\
  modules/shepherd.scm
nodist_shepherd_DATA = $(dist_shepherd_DATA:%.scm=%.go)

shepherdsubdir = $(shepherddir)/shepherd
dist_shepherdsub_DATA =				\
  modules/shepherd/args.scm			\
  modules/shepherd/service.scm			\
  modules/shepherd/support.scm			\
  modules/shepherd/runlevel.scm			\
  modules/shepherd/comm.scm
nodist_shepherdsub_DATA =			\
  modules/shepherd/config.scm			\
  modules/shepherd/config.go			\
  modules/shepherd/system.scm			\
  modules/shepherd/system.go			\
  $(dist_shepherdsub_DATA:%.scm=%.go)

scriptsdir = $(shepherdsubdir)/scripts
dist_scripts_DATA =				\
  modules/shepherd/scripts/halt.scm		\
  modules/shepherd/scripts/herd.scm		\
  modules/shepherd/scripts/reboot.scm
nodist_scripts_DATA =				\
  $(dist_scripts_DATA:%.scm=%.go)

CLEANFILES =						\
  $(nodist_shepherd_DATA) $(nodist_shepherdsub_DATA)	\
  $(nodist_scripts_DATA)				\
  $(bin_SCRIPTS) $(sbin_SCRIPTS)

# Documentation.
info_TEXINFOS = shepherd.texi
SUBDIRS = examples

# Things not automatically included in the distribution.
EXTRA_DIST = $(templates) QUESTIONS fdl-1.3.texi ChangeLog-2003

# Create the socket directory (aka. 'default-socket-dir'.)  Make it
# accessible only by its owner since otherwise 'shepherd' suggests using
# '--insecure'.  This is just a convenience, so allow it to fail.
install-data-local:
	-$(MKDIR_P) $(DESTDIR)$(localstatedir)/run/shepherd
	-chmod 700 $(DESTDIR)$(localstatedir)/run/shepherd

# Relocate the script---i.e., have them refer to the installed module
# directory.
install-exec-hook: install-executable-symlinks
	for script in							\
	   $(bin_SCRIPTS:%=$(DESTDIR)$(bindir)/%)			\
	   $(sbin_SCRIPTS:%=$(DESTDIR)$(sbindir)/%) ;			\
	do								\
	  $(SED) -e's|$(abs_top_srcdir)/modules|$(shepherddir)|g'	\
	         -e's|$(abs_top_builddir)/modules|$(shepherddir)|g'	\
	         -i $$script ;						\
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
	$(AM_V_GEN)$(MKDIR_P) "`dirname $@`" ;		\
	$(SED) $(instantiate) < $< >$@ ;		\
	$(maybe_executable)

modules/shepherd/config.scm: modules/shepherd/config.scm.in Makefile
	$(MKDIR_P) "`dirname $@`"
	$(SED) $(instantiate) < $< >$@

AM_V_GUILEC = $(AM_V_GUILEC_$(V))
AM_V_GUILEC_ = $(AM_V_GUILEC_$(AM_DEFAULT_VERBOSITY))
AM_V_GUILEC_0 = @echo "  GUILEC  " $@;

# Make sure 'modules/shepherd/{config,system.scm}' are built first.
# XXX: Use the C locale for when Guile lacks
# <http://git.sv.gnu.org/cgit/guile.git/commit/?h=stable-2.0&id=e2c6bf3866d1186c60bacfbd4fe5037087ee5e3f>.
%.go: %.scm $(templates:%.in=%)
	$(AM_V_GUILEC)$(MKDIR_P) "`dirname "$@"`" ;	\
	$(AM_V_P) && out=1 || out=- ;			\
	LC_ALL=C					\
	$(GUILD) compile --target="$(host)"		\
	  -L "$(top_builddir)/modules"			\
	  -L "$(top_srcdir)/modules"			\
	  -Wformat -Wunbound-variable -Warity-mismatch	\
	  -o "$@" "$<" >&$$out

SUFFIXES = .go

TESTS =						\
  tests/basic.sh				\
  tests/respawn.sh				\
  tests/misbehaved-client.sh			\
  tests/no-home.sh				\
  tests/status-sexp.sh				\
  tests/sigint.sh

TEST_EXTENSIONS = .sh
EXTRA_DIST += $(TESTS)

AM_TESTS_ENVIRONMENT =				\
  unset XDG_CONFIG_HOME; LC_ALL=C		\
  PATH="$(abs_top_builddir):$$PATH"		\
  SHELL="$(SHELL)" GUILE="$(GUILE)"		\
  GUILE_LOAD_PATH="$(abs_top_srcdir)/modules:$(abs_top_builddir)/modules:$$GUILE_LOAD_PATH" \
  GUILE_LOAD_COMPILED_PATH="$(abs_top_srcdir)/modules:$(abs_top_builddir)/modules:$$GUILE_LOAD_COMPILED_PATH"

SH_LOG_COMPILER = $(SHELL)
AM_SH_LOG_FLAGS = -x -e

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
shepherd_install_go_files = install-nobase_nodist_guilemoduleDATA
$(shepherd_install_go_files): install-nobase_dist_guilemoduleDATA

gen-ChangeLog:
	if test -d .git; then				\
	  $(top_srcdir)/build-aux/gitlog-to-changelog	\
	    8da9cf2..HEAD > $(distdir)/cl-t;		\
	  rm -f $(distdir)/ChangeLog;			\
	  mv $(distdir)/cl-t $(distdir)/ChangeLog;	\
	fi

dist-hook: gen-ChangeLog

.PHONY: gen-ChangeLog
