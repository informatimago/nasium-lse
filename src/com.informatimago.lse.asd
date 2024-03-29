;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.lse.asd
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
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************


(asdf:defsystem :com.informatimago.lse
  :description  "This system implements a L.S.E. interpreter."
  :author "<PJB> Pascal J. Bourguignon <pjb@informatimago.com>"
  :version "1.2.0"
  :licence "AGPL3"
  :properties ((#:author-email                   . "pjb@informatimago.com")
               (#:date                           . "Winter 2012")
               ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.lse/")
               ((#:albert #:formats)             . ("docbook"))
               ((#:albert #:docbook #:template)  . "book")
               ((#:albert #:docbook #:bgcolor)   . "white")
               ((#:albert #:docbook #:textcolor) . "black"))

  #+asdf-unicode :encoding #+asdf-unicode :utf-8

  :depends-on (
               "split-sequence"
               "alexandria"
               "babel"

               "com.informatimago.common-lisp"
               "com.informatimago.rdp"

               )

  :components (
               ;; Some generic utility
               (:file "logger")
               (:file "signal")
               (:file "environment")

               ;;---------------------

               ;; LSE language
               (:file "packages"            :depends-on ("signal" "logger"))

               (:file "os"                  :depends-on ("packages"))
               (:file "version"             :depends-on ("packages"))
               (:file "configuration"       :depends-on ("packages"))
               (:file "error"               :depends-on ("packages"))
               (:file "globals"             :depends-on ("packages"))
               (:file "file"                :depends-on ("packages"
                                                         "configuration" "error"
                                                         "functions"))

               (:file "documentation"       :depends-on ("packages" "globals" "terminal"))
               (:file "chapters"            :depends-on ("packages" "globals" "version" "documentation"))

               (:file "catalog"             :depends-on ("packages" "configuration"))
               (:file "variables"           :depends-on ("packages"))
               (:file "functions"           :depends-on ("packages"
                                                         "globals" "error"
                                                         "documentation"
                                                         "variables"))

               (:file "lse-scanner"         :depends-on ("packages" "globals" "error"))
               (:file "lse-parser"          :depends-on ("packages" "lse-scanner"))
               (:file "byte-code"           :depends-on ("packages"))
               (:file "compiler"            :depends-on ("packages"
                                                         "globals" "version"
                                                         "lse-scanner" "lse-parser" "byte-code"))
               (:file "vm"                  :depends-on ("packages"
                                                         "globals" "error" "byte-code" "compiler"
                                                         "variables" "functions" "file"))

               (:file "task"                :depends-on ("packages" "globals" "file" "vm" "environment"))
               (:file "terminal"            :depends-on ("packages" "globals" "error" "lse-parser"))
               (:file "io"                  :depends-on ("packages" "file" "task" "terminal"))
               (:file "commands"            :depends-on ("packages"
                                                         "globals"
                                                         "error" "version"
                                                         "documentation"
                                                         "catalog" "functions"
                                                         "os" "io" "compiler" "task"))

               ))


;;;; THE END ;;;;
