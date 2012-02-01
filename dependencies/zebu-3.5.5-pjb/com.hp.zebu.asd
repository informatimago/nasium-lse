;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.hp.zebu.asd
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    ASD file to load the Zebu library.
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


;; Note: cf. zebu-sys.lisp for dependencies.

(asdf:defsystem :com.hp.zebu
    :description  "Zebu LALR parser generator."
    :author "Hewlett-Packard Company"
    :version "1.0.0"
    :licence "GPL"
    :properties ((#:date                           . "Winter 2012")
                 ((#:albert #:output-dir)          . "/tmp/documentation/com.hp.zebu/")
                 ((#:albert #:formats)             . ("docbook"))
                 ((#:albert #:docbook #:template)  . "book")
                 ((#:albert #:docbook #:bgcolor)   . "white")
                 ((#:albert #:docbook #:textcolor) . "black"))

    :serial t
    :components ((:file "zebu-package")

                 (:file "zebu-init")

                 (:file "zebu-aux")
                 (:file "zebu-kb-domain")
                 (:file "zebu-mg-hierarchy")
                 (:file "zebu-regex")
                 (:file "zebu-loader")
                 (:file "zebu-driver")
                 (:file "zebu-actions")
                 (:file "zebu-oset")
                 (:file "zebu-g-symbol")
                 (:file "zebu-loadgram")
                 (:file "zebu-generator")
                 (:file "zebu-lr0-sets")
                 (:file "zebu-empty-st")
                 (:file "zebu-first")
                 (:file "zebu-follow")
                 ;; (:file "zebu-tables")
                 (:file "zebu-slr")
                 (:file "zebu-closure")
                 (:file "zebu-lalr1")
                 (:file "zebu-dump")
                 (:file "zebu-compile")
                 (:file "zebu-tables")
                 (:file "zebu-printers") ; only for debugging

                 (:file "zebu-mg")
                 (:file "zmg-dom")
                 ;; (:file "zebu-kb-domain")
                 (:file "zebu-tree-attributes")
                 (:file "zebra-debug")

                 

                 (:file "zebu")))


;;;; THE END ;;;;
