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

BUILD_HOSTS = kuiper galatea lassell # triton

all:ChangeLog
	@for host in $(BUILD_HOSTS) ; do \
		echo "// Building on $$host" ;\
		ssh $$host make -C $$(pwd) unix-cli ;\
	 done

help:
	@printf 'make all          # builds all executables on all hosts.\n'
	@printf 'make unix-cli     # builds the unix cli executable on the local hosts.\n'
	@printf 'make ChangeLog    # Extracts the logs from git into the ChangeLog file.\n'
	@printf 'make variables    # shows the variables.\n'


ChangeLog:
	git log > ChangeLog

unix-cli: # --batch
	ccl --load generate-unix-cli.lisp | tee /tmp/unix-cli.log 
	awk '/Generating/{print $$2}' < /tmp/unix-cli.log | ( read executable ; rm -f lse ; ln -sf $$executable lse )
symlink:
	awk '/Generating/{print $$2}' < /tmp/unix-cli.log | ( read executable ; rm -f lse ; ln -sf $$executable lse )

variables:
	@echo pwd=$$(pwd)
	@echo BUILD_HOSTS=$(BUILD_HOSTS)

.PHONY: unix-cli

#### THE END ####
