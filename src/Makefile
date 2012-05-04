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
#    along with this program.  If not, see http://www.gnu.org/licenses/
#*****************************************************************************

# If UPLOAD is 1, then documentation is generated in $(UPLOAD_DOC_DIR)
# and uploaded by make upload in $(UPLOAD_DIR).
# otherwise, the documentation is generated in $(DOC_DIR).
UPLOAD         = 1
UPLOAD_DIR     = /home/pjb/public_html/sites/com.ogamita.www
UPLOAD_DOC_DIR = $(UPLOAD_DIR)/nasium-lse/
DOC_DIR        = ../doc/

BUILD_HOSTS    = kuiper galatea lassell # triton

RMERGE         = rsync -HSWacvxz --progress -e ssh
RSYNCH         = rsync -HSWacvxz --progress -e ssh --force --delete --delete-after

LOGFILE        = /tmp/lse-build.log
SYMLINK_CLI    = awk '/Generating/{print $$2}' < $(LOGFILE) | ( read executable ; rm -f lse ; ln -sf $$executable lse )
SYMLINK_SERVER = awk '/Generating/{print $$2}' < $(LOGFILE) | ( read executable ; rm -f lse-server ; ln -sf $$executable lse-server )


CCL            = ccl --no-init
CCL_EVAL       =  --eval
CCL_LOAD       =  --load 

CLISP          = clisp -ansi -q -norc
CLISP_LOAD     = 
CLISP_EVAL     = -repl -x

LISP           = $(CCL)
LOAD           = $(CCL_LOAD)
EVAL           = $(CCL_EVAL)


HELPFORMAT="$(MAKE) %-20s \# %s\n"

help::
	@printf $(HELPFORMAT) all 'builds all executables on all hosts.'
all:ChangeLog
	@for host in $(BUILD_HOSTS) ; do \
		echo "// Building on $$host" ;\
		ssh $$host $(MAKE) $(MFLAGS) -C $$(pwd) cli server ;\
	 done
	$(MAKE) $(MFLAGS) html-doc



help::
	@printf $(HELPFORMAT) ChangeLog 'Extracts the logs from git into the ChangeLog file.'
ChangeLog:../_FOSSIL_
	fossil timeline -n 2000 > ChangeLog
#	git log > ChangeLog



help::
	@printf $(HELPFORMAT) cli 'builds the unix cli executable on the local hosts.'
cli:
	$(LISP) $(LOAD) generate-cli.lisp | tee $(LOGFILE)
	$(SYMLINK_CLI)


help::
	@printf $(HELPFORMAT) server 'builds the server executable on the local hosts.'
server:
	$(LISP) $(LOAD) generate-server.lisp | tee $(LOGFILE)
	$(SYMLINK_SERVER)



help::
	@printf $(HELPFORMAT) html-doc 'builds the html doc directory on the local hosts.'
html-doc:
	if [ $(UPLOAD) -ne 0 ] ; then \
		 $(LISP) $(EVAL) "(defparameter *doc-dir* \"$(UPLOAD_DOC_DIR)\")" \
				 $(LOAD) generate-documentation.lisp ;\
	else \
		 $(LISP) $(EVAL) "(defparameter *doc-dir* \"$(DOC_DIR)\")" \
				 $(LOAD) generate-documentation.lisp ;\
	fi
	if [ $(UPLOAD) -ne 0 ] ; then $(MAKE) $(MFLAGS) -C $(UPLOAD_DIR) upload ; fi



help::
	@printf $(HELPFORMAT) variables 'shows the variables.'
variables:
	@printf '%-20s = %s\n' pwd            "$$(pwd)"
	@printf '%-20s = %s\n' BUILD_HOSTS    "$(BUILD_HOSTS)"
	@printf '%-20s = %s\n' LISP           "$(LISP)"
	@printf '%-20s = %s\n' SYMLINK_SERVER "$(SYMLINK_SERVER)"
	@printf '%-20s = %s\n' SYMLINK_CLI    "$(SYMLINK_CLI)"



#cp lse /usr/local/bin/nasium-lse-inetd
help::
	@printf $(HELPFORMAT) install 'installs the inetd executable.'
install:
	cp lse ~/bin/lse

.PHONY: unix-cli html-doc

#### THE END ####
