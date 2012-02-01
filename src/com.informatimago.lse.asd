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
    :version "1.0.1"
    :licence "GPL"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Winter 2012")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.lse/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))

    :depends-on (:split-sequence

                 #+clisp :com.informatimago.clisp
                 #+clisp :com.informatimago.susv3
                 
                 :com.informatimago.common-lisp

                 :com.hp.zebu
                 )
    :serial t
    :components ((:file "patch-zebu")
                 (:file "packages")
                 (:file "grammar")
                 (:file "lse-domain")

                 
                 (:file "configuration")
                 (:file "io")
                 (:file "error")

                 (:file "task")

                 (:file "catalog")
                 (:file "functions")
                 (:file "scanner-lse")
                 (:file "compiler")

                 (:file "commands")

                 (:file "pmatch")
                 (:file "server")
                 (:file "simple-server")
                 ;; (:file "vm")

                 (:file "main")
                 ))


;;;; THE END ;;;;
