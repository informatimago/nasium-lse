;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.common-lisp.asd
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


(asdf:defsystem :com.informatimago.lse
    :description  "LSE interpreter."
    :author "<PJB> Pascal J. Bourguignon <pjb@informatimago.com>"
    :version "1.1.0"
    :licence "AGPLv3"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Winter 2012")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.lse.server/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))

    :depends-on (:split-sequence
                 :alexandria
                 :babel
                 
                 :iolib.base
                 :iolib.os
                 :iolib.syscalls
                 :iolib

                 #+clisp :com.informatimago.clisp
                 #+clisp :com.informatimago.susv3

                 :com.informatimago.common-lisp
                 :com.informatimago.lse
                 )
    :components (

                 (:file "iolib-message"       :depends-on ())
                 (:file "iolib-end-point")
                 (:file "iolib-utils"         :depends-on ("iolib-message" "iolib-end-point"))        
                 (:file "iolib-server"        :depends-on ("iolib-utils" "iolib-message"))   
                 (:file "iolib-client"        :depends-on ("iolib-utils" "iolib-message"))   
                 ;;---------------------                                                              
                                                                                                      
                 (:file "server-package"      :depends-on ("iolib-message"
                                                           "iolib-end-point"
                                                           "iolib-utils"
                                                           "iolib-server"))
                                                                                                      
                 (:file "server-commands"     :depends-on ("server-package"))
                 (:file "server"              :depends-on ("server-package"
                                                           "iolib-end-point"
                                                           "iolib-utils"
                                                           "iolib-server"
                                                           "server-commands"))
                 ))


;;;; THE END ;;;;