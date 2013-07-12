# -*- mode:makefile-bsdmake;coding:utf-8 -*-
#*****************************************************************************
#FILE:               Makefile
#LANGUAGE:           makefile-bsdmake
#SYSTEM:             POSIX
#USER-INTERFACE:     NONE
#DESCRIPTION
#    
#    Generates a MacOSX application bundle.
#    
#AUTHORS
#    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
#MODIFICATIONS
#    2012-05-06 <PJB> Created.
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

all:app

NAME=NASIUM-LSE

app:
	-@rm -rf  ${NAME}.app
	mkdir -p ${NAME}.app/Contents/MacOS  ${NAME}.app/Contents/Resources
	sed -e "s/{VERSION}/$(shell cat VERSION)/g" \
		-e "s/{SHORT_VERSION}/$(shell cat VERSION_SHORT)/g" \
		-e "s/{LONG_VERSION}/$(shell cat VERSION_LONG)/g" \
		< Info.plist \
		> ${NAME}.app/Contents/Info.plist
	@cp -v ../lse-*-darwin-*   ${NAME}.app/Contents/MacOS/
	@cp -v nasium-lse.sh       ${NAME}.app/Contents/MacOS/
	@cp -v \
		nasium-lse-application.icns  \
		nasium-lse-programme.icns  \
		nasium-lse-ruban.icns  \
	    ${NAME}.app/Contents/Resources/
	@cp -v nasium-lse.terminal    ${NAME}.app/Contents/Resources/

#### THE END ####
