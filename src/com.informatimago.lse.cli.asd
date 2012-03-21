;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.lse.cli.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the com.informatimago.lse program.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Created this .asd file.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************


(asdf:defsystem :com.informatimago.lse.cli
    :description  "This system defines the Command Line Interface L.S.E. interpreter."
    :author "<PJB> Pascal J. Bourguignon <pjb@informatimago.com>"
    :version "1.2.2"
    :licence "AGPL3"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Winter 2012")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.lse.cli/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))

    :depends-on (
                 :terminfo
                 
                 :cffi
                 :uffi

                 :trivial-gray-streams
                 
                 #+(and unix (not clisp)) :iolib.base
                 #+(and unix (not clisp)) :iolib.os
                 #+(and unix (not clisp)) :iolib.syscalls
                 #+(and unix (not clisp)) :iolib.termios
                 
                 :com.informatimago.common-lisp.unix
                 :com.informatimago.lse
                 )
    :components (

                 (:file "patch-cffi-uffi")
                 
                 (:file "cli-package")

                 #+swank
                 (:file "swank-terminal"      :depends-on ("cli-package"))

                 #+(and unix (not clisp))
                 (:file "unix-terminal"       :depends-on ("cli-package"))
                 #-(and unix (not clisp))
                 (:file "unix-terminal-stub"  :depends-on ("cli-package"))
                 
                 (:file "terminfo-terminal"   :depends-on ("cli-package"))

                 (:file "cli-arguments"       :depends-on ("cli-package"))
                 (:file "cli"
                        :depends-on ("cli-package"
                                     "cli-arguments"
                                     "terminfo-terminal"
                                     #+swank "swank-terminal"
                                     #-(and unix (not clisp)) "unix-terminal-stub"
                                     #+(and unix (not clisp)) "unix-terminal"))
                 ))


;;;; THE END ;;;;
