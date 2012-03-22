# -*- mode:makefile-gmake;coding:utf-8 -*-
#*****************************************************************************
#FILE:               Makefile
#LANGUAGE:           makefile-gmake
#SYSTEM:             POSIX
#USER-INTERFACE:     NONE
#DESCRIPTION
#    
#    Makefile to build the products.
#    
#AUTHORS
#    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
#MODIFICATIONS
#    2012-03-01 <PJB> Created.
#BUGS
#LEGAL
#    AGPL3
#    
#    Copyright Pascal J. Bourguignon 2012 - 2012
#    
#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU Affero General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.
#    
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU Affero General Public License for more details.
#    
#    You should have received a copy of the GNU Affero General Public License
#    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#*****************************************************************************

UPLOAD         = 1
BUILD_HOSTS    = kuiper galatea lassell # triton
RMERGE         = rsync -HSWacvxz --progress -e ssh
RSYNCH         = rsync -HSWacvxz --progress -e ssh --force --delete --delete-after

LOGFILE        = /tmp/lse-build.log
SYMLINK_CLI    = awk '/Generating/{print $$2}' < $(LOGFILE) | ( read executable ; rm -f lse ; ln -sf $$executable lse )
SYMLINK_SERVER = awk '/Generating/{print $$2}' < $(LOGFILE) | ( read executable ; rm -f lse-server ; ln -sf $$executable lse-server )

CCL            = ccl --load      
CLISP          = clisp -ansi -q  
LISP           = $(CCL)


help::
	@printf 'make all          # builds all executables on all hosts.\n'
all:ChangeLog
	@for host in $(BUILD_HOSTS) ; do \
		echo "// Building on $$host" ;\
		ssh $$host $(MAKE) $(MFLAGS) -C $$(pwd) cli server ;\
	 done
	$(MAKE) $(MFLAGS) html-doc





help::
	@printf 'make cli          # builds the unix cli executable on the local hosts.\n'

cli:cli/ccl

cli/ccl:
	$(CCL)   generate-cli.lisp | tee $(LOGFILE)
	$(SYMLINK_CLI)

cli/clisp:
	$(CLISP) generate-cli.lisp | tee $(LOGFILE)
	$(SYMLINK_CLI)

unix-cli:cli



help::
	@printf 'make ChangeLog    # Extracts the logs from git into the ChangeLog file.\n'
ChangeLog:../_FOSSIL_
	fossil timeline -n 2000 > ChangeLog
#	git log > ChangeLog


help::
	@printf 'make server       # builds the server executable on the local hosts.\n'
server:server/ccl

server/ccl:
	$(CCL)   generate-server.lisp | tee $(LOGFILE)
	$(SYMLINK_SERVER)

server/clisp:
	$(CLISP) generate-server.lisp | tee $(LOGFILE)
	$(SYMLINK_SERVER)




help::
	@printf 'make html-doc     # builds the html doc directory on the local hosts.\n'
html-doc:
	$(CCL)  generate-documentation.lisp
	if [ $(UPLOAD) -ne 0 ] ; then $(MAKE) $(MFLAGS) -C /home/pjb/public_html/sites/com.ogamita.www upload ; fi

# doc-unix-cli:*.lisp
# 	$(MAKE) $(MFLAGS) html-doc
# 
# install-doc:doc-unix-cli
# 	$(RSYNCH) doc-unix-cli/ ~/public_html/sites/com.ogamita.www/nasium-lse/doc-unix-cli



help::
	@printf 'make variables    # shows the variables.\n'
variables:
	@printf '%-20s = %s\n' pwd            "$$(pwd)"
	@printf '%-20s = %s\n' BUILD_HOSTS    "$(BUILD_HOSTS)"
	@printf '%-20s = %s\n' LISP           "$(LISP)"
	@printf '%-20s = %s\n' SYMLINK_SERVER "$(SYMLINK_SERVER)"
	@printf '%-20s = %s\n' SYMLINK_CLI    "$(SYMLINK_CLI)"



.PHONY: unix-cli html-doc

#### THE END ####
