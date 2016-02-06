;;;; -*- mode:lisp; coding:utf-8 -*-
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(IN-PACKAGE "COM.INFORMATIMAGO.LSE")

(defun snoc (list item)
  "nconc the ITEM to the LIST."
  (nconc list (cons item nil)))


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defvar *grammar-directory*
    (make-pathname :directory '(:relative "grammars") :case :common)
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
     :VERBOSE t)))


;; Load the grammar:
(zebu:delete-grammar "LSE")
(zebu-load-file (make-pathname :name "PARSER-LSE" :type "TAB" :case :common))
;; (load (compile-file "lse-domain.lisp")) ;; done by zebu-load-file



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEFINE-GRAMMAR & DEFRULE
;;;
;;;  Allows the definition of gramamr in lisp sources
;;;  instead of .zb files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#-(and)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *rules* (make-hash-table)))


#-(and)
(defmacro defrule (&whole rule   name &rest args )
  (declare (ignore args))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *rules*) ',rule)
     ',name))

#-(and)
(defmacro define-grammar (name
                          (&key (grammar-file nil grammar-file-p)
                                (output-file  nil output-file-p)
                                (package nil packagep)
                                (identifier-start-chars "")
                                (identifier-continue-chars "")
                                (intern-identifier nil)
                                (string-delimiter #\")
                                (symbol-delimiter #\')
                                (domain ())
                                (domain-file nil domain-file-p)
                                (grammar "null-grammar")
                                (white-space nil white-space-p)
                                (case-sensitive t)
                                (lex-cats nil lex-cats-p))
                          &body rules)
  (unless lex-cats-p
    (error ":LEX-CATS keyword is mandatory in the options of DEFINE-GRAMMAR."))
  (let ((grammar-file (if grammar-file-p
                          grammar-file
                          (make-pathname :name (string name)
                                         :type "ZB"
                                         :defaults *grammar-directory*
                                         :case :common)))
        (output-file (if output-file-p
                         output-file
                         (make-pathname :name (string name)
                                        :type "TAB"
                                        :defaults *grammar-directory*
                                        :case :common))))
    (with-open-file (*standard-output*
                     grammar-file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (print `(:name ,(string name)
                     :package  ,(string (if packagep package (package-name *package*)))
                     :identifier-start-chars     ,identifier-start-chars    
                     :identifier-continue-chars  ,identifier-continue-chars 
                     :intern-identifier          ,intern-identifier         
                     :string-delimiter           ,string-delimiter          
                     :symbol-delimiter           ,symbol-delimiter          
                     :case-sensitive             ,case-sensitive            
                     :grammar                    ,grammar                   
                     :domain                     ,domain                    
                     :domain-file ,(if domain-file-p
                                       domain-file
                                       (format nil "~(~A~)-domain" name))
                     ,@(when white-space-p `(:white-space ,white-space))
                     :lex-cats ,lex-cats))
      (dolist (rule rules)
        (if (and (consp rule) (eq (first rule) 'insert-rule))
            (if (gethash (second rule) *rules*)
                (print (gethash (second rule) *rules*))
                (error "Unknown rule name ~S" (second rule)))
            (print rule))))
    (LET ((COM.HP.ZEBU::*WARN-CONFLICTS*  T)
          (COM.HP.ZEBU::*ALLOW-CONFLICTS* T))
      (zebu-compile-file grammar-file :output-file output-file))
    `(progn
       (eval-when (:compile-toplevel)
         (zebu:delete-grammar ,(string name))
         ;; (zebu-load-file ',output-file)
         (defparameter ,name nil))
       (eval-when (::load-toplevel :execute)
         (zebu:delete-grammar ,(string name))
         (zebu-load-file ',output-file)
         (defparameter ,name (find-grammar ,(string name)))))))



;;; THE END ;;;;
