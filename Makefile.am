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
templates = dmd.in deco.in config.scm.in

# The source files.
dmddir = ${datadir}/dmd
dmd_data = args.scm deco.scm dmd.scm self.scm service.scm \
           support.scm runlevel.scm deco.scm comm.scm
dmd_DATA = $(dmd_data) config.scm

# Documentation.
info_TEXINFOS = dmd.texi
SUBDIRS = examples utils

# Things not automatically included in the distribution.
EXTRA_DIST = $(dmd_data) $(templates) QUESTIONS

# Create the directory for the sockets.
install-data-local:
	mkdir -p $(DESTDIR)/${prefix}/var/run/dmd
	chmod 777 $(DESTDIR)/${prefix}/var/run/dmd

# Generate main programs and insert dir names into config.scm.
%: %.in Makefile
	sed $< >$@ \
	  -e 's,%DMDDIR%,${dmddir},g' \
	  -e 's,%PREFIX%,${prefix},g' \
	  -e 's,%VERSION%,@VERSION@,g'
