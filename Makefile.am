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
templates =					\
  dmd.in deco.in				\
  modules/dmd/config.scm.in			\
  modules/dmd/system.scm.in

# Build this module first.
BUILT_SOURCES = modules/dmd/config.scm

# The source files.
dmddir = ${datadir}/dmd
dist_dmd_DATA =					\
  modules/deco.scm				\
  modules/dmd.scm
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

CLEANFILES = $(nodist_dmd_DATA) $(nodist_dmdsub_DATA) $(bin_SCRIPTS)

# Documentation.
info_TEXINFOS = dmd.texi
SUBDIRS = examples utils

# Things not automatically included in the distribution.
EXTRA_DIST = $(templates) QUESTIONS

# Create the directory for the sockets.
install-data-local:
	mkdir -p $(DESTDIR)/${prefix}/var/run/dmd
	chmod 777 $(DESTDIR)/${prefix}/var/run/dmd

# 'sed' expression to instantiate templates.
instantiate =						\
  -e 's,%DMDDIR%,${dmddir},g'				\
  -e 's,%PREFIX%,${prefix},g'				\
  -e 's,%localstatedir%,${localstatedir},g'		\
  -e 's,%VERSION%,@VERSION@,g'				\
  -e 's,%PACKAGE_BUGREPORT%,@PACKAGE_BUGREPORT@,g'	\
  -e 's,%PACKAGE_NAME%,@PACKAGE_NAME@,g'		\
  -e 's,%PACKAGE_URL%,@PACKAGE_URL@,g'			\
  -e 's,%GUILE%,$(GUILE),g'

# Instantiate templates.

%: %.in Makefile
	$(MKDIR_P) "`dirname $@`"
	$(SED) $(instantiate) < $< >$@

modules/dmd/config.scm: modules/dmd/config.scm.in Makefile
	$(MKDIR_P) "`dirname $@`"
	$(SED) $(instantiate) < $< >$@

# Instantiate commands.

deco: deco.in Makefile
	$(MKDIR_P) "`dirname $@`"
	$(SED) $(instantiate) < $< >$@
	chmod +x "$@"

dmd: dmd.in Makefile
	$(MKDIR_P) "`dirname $@`"
	$(SED) $(instantiate) < $< >$@
	chmod +x "$@"

# XXX: Use the C locale for when Guile lacks
# <http://git.sv.gnu.org/cgit/guile.git/commit/?h=stable-2.0&id=e2c6bf3866d1186c60bacfbd4fe5037087ee5e3f>.
.scm.go:
	$(MKDIR_P) "`dirname "$@"`"
	LC_ALL=C					\
	$(GUILD) compile -L "$(top_builddir)/modules"	\
	  -L "$(top_srcdir)/modules"			\
	  -Wformat -Wunbound-variable -Warity-mismatch	\
	  -o "$@" "$<"

SUFFIXES = .go

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
dmd_install_go_files = install-nobase_nodist_guilemoduleDATA
$(dmd_install_go_files): install-nobase_dist_guilemoduleDATA
