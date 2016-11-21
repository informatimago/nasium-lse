;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.manifest.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    ASD file to load the com.informatimago.manifest package.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-14 <PJB> Created this .asd file.
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


(asdf:defsystem :com.informatimago.manifest
    :description  "Generates a manifest for ASDF systems."
    :author "<PJB> Pascal J. Bourguignon <pjb@informatimago.com>"
    :version "1.0.1"
    :licence "AGPL3"
    :properties ((#:author-email                   . "pjb@informatimago.com")
                 (#:date                           . "Winter 2012")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.informatimago.manifest/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))

    #+asdf-unicode :encoding #+asdf-unicode :utf-8

    :depends-on (
                 "split-sequence"
                 ;; "alexandria
                 ;; "babel
                 "com.informatimago.common-lisp.cesarum"
                 )

    :components ((:file "manifest")))


;;;; THE END ;;;;
