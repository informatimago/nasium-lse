;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               grammar.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Compiles the grammars.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Extracted from loader.lisp.
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

(IN-PACKAGE "COM.INFORMATIMAGO.LSE")

(defun snoc (list item)
  "nconc the ITEM to the LIST."
  (nconc list (cons item nil)))


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *grammar-directory*
    (make-pathname :directory '(:relative "GRAMMARS") :case :common)
    "The directory where the grammars files are stored.
Used by the DEFINE-GRAMMAR macro in command.lisp")

  (ensure-directories-exist (make-pathname :name "TEST" :type "TXT" :case :common
                                           :defaults *grammar-directory*)))


(eval-when (:compile-toplevel :execute)
  ;; Compile the LSE grammar:
  (LET ((COM.HP.ZEBU::*WARN-CONFLICTS*  T)
        (COM.HP.ZEBU::*ALLOW-CONFLICTS* T))
    (COM.HP.ZEBU::ZEBU-COMPILE-FILE
     (make-pathname :name "PARSER-LSE" :type "ZB" :case :common)
     :VERBOSE nil)))


;; Load the grammar:
(zebu:delete-grammar "LSE")
(zebu-load-file (make-pathname :name "PARSER-LSE" :type "TAB" :case :common))
;; (load (compile-file "lse-domain.lisp")) ;; done by zebu-load-file


;;; THE END ;;;;
