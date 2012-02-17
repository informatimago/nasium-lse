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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************


(asdf:defsystem :com.informatimago.lse
    :description  "LSE interpreter."
    :author "<PJB> Pascal J. Bourguignon <pjb@informatimago.com>"
    :version "1.0.7"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Winter 2012")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.lse/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))

    :depends-on (:split-sequence
                 :alexandria
                 :babel
                 
                 :iolib.base
                 :iolib

                 #+clisp :com.informatimago.clisp
                 #+clisp :com.informatimago.susv3

                 :com.informatimago.common-lisp
                 :com.informatimago.rdp
                 ;; :com.hp.zebu
                 )
    :components (
                 ;; Some generic utility
                 (:file "logger")
                 (:file "signal")
                 (:file "environment")
                 (:file "iolib-message")
                 (:file "iolib-end-point")
                 (:file "iolib-utils"         :depends-on ("logger" "signal" "iolib-message"))        
                 (:file "iolib-server"        :depends-on ("logger" "iolib-utils" "iolib-message"))   
                 (:file "iolib-client"        :depends-on ("logger" "iolib-utils" "iolib-message"))   
                 ;;---------------------                                                              
                                                                                                      
                 ;; LSE language                                                                      
                 (:file "packages")                                                                   
                                                                                                      
                 ;; (:file "patch-zebu")                                                              
                 ;; (:file "grammar")                                                                 
                 ;; (:file "lse-domain")                                                              
                                                                                                      
                                                                                                      
                 (:file "version"             :depends-on ("packages"))
                 (:file "configuration"       :depends-on ("packages"))
                 (:file "error"               :depends-on ("packages"))                               

                 (:file "file"                :depends-on ("packages"
                                                           "configuration" "error"))
                 
                 (:file "catalog"             :depends-on ("packages"))                               
                 (:file "functions"           :depends-on ("packages" "error"))
                                                                                                      
                 (:file "lse-scanner"         :depends-on ("packages" "error"))
                 (:file "lse-parser"          :depends-on ("packages" "lse-scanner"))                 
                 (:file "byte-code"           :depends-on ("packages"))                               
                 (:file "compiler"            :depends-on ("packages" "lse-parser" "byte-code"))      
                 (:file "vm"                  :depends-on ("packages"
                                                           "error" "byte-code" "functions" "file"))

                 (:file "commands"            :depends-on ("packages" "error")) 
                 (:file "task"                :depends-on ("packages" "vm"))
                 (:file "io"                  :depends-on ("packages" "file" "task"))

                 ;; LSE cli
                 (:file "swank-terminal"      :depends-on ("packages" "io"))
                 (:file "unix-cli"            :depends-on ("packages"
                                                           "version" "configuration"
                                                           "commands" "vm" "compiler"
                                                           "io" "swank-terminal"))
                 ;; LSE server
                 (:file "server-commands"     :depends-on ("packages"))
                 (:file "server"              :depends-on ("packages"
                                                           "iolib-end-point" "iolib-utils" "iolib-server"
                                                           "server-commands"
                                                           "version" "configuration"
                                                           "task" "commands" "vm" "compiler"))

                 ;; (:file "simple-server")
                 ;; (:file "main")
                 ))


;;;; THE END ;;;;
