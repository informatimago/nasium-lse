;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               environment.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This file exports a dynamic environment for clients.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Created.
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
(defpackage "COM.INFORMATIMAGO.ENVIRONMENT"
  (:use "COMMON-LISP")
  (:export "MAKE-DEFAULT-ENVIRONMENT" "WITH-ENVIRONMENT" "ENVIRONMENT")
  (:documentation "This package binds Common Lisp dynamic variables in a user environment."))
(in-package "COM.INFORMATIMAGO.ENVIRONMENT")

(defstruct environment
  bindings)

(defun make-default-environment ()
  "
Returns a default execution environment for clients.

Basically, it captures all the special variables of Common Lisp with their default
values, but for a few specific variables (such as *PACKAGE* or *READTABLE* that
are configured specifically.
"
  (flet ((capture (special-variables)
           (mapcar (lambda (var) (cons var (symbol-value var)))
                   special-variables)))
    (make-environment
     :bindings (list* (cons '*package*      (load-time-value (find-package "COMMON-LISP-USER")))
                      (cons '*READTABLE*    (copy-readtable *READTABLE*))
                      (cons '*RANDOM-STATE* (make-random-state *RANDOM-STATE*))
                      (cons '*FEATURES*     (copy-list *FEATURES*))
                      (cons '*MODULES*      (copy-list *MODULES*))
                      (cons '*LOAD-PATHNAME* nil)
                      (cons '*LOAD-TRUENAME* nil)
                      (cons '*PRINT-RIGHT-MARGIN* 80)
                      (capture '(#+ecl SI:*PRINT-PACKAGE*
                                 #+ecl SI:*PRINT-STRUCTURE*
                                 * ** ***
                                 + ++ +++
                                 -
                                 / // ///
                                 *BREAK-ON-SIGNALS*
                                 *COMPILE-FILE-PATHNAME*
                                 *COMPILE-FILE-TRUENAME*
                                 *COMPILE-PRINT*
                                 *COMPILE-VERBOSE*
                                 *DEBUGGER-HOOK*
                                 *DEFAULT-PATHNAME-DEFAULTS*
                                 *GENSYM-COUNTER*
                                 *LOAD-PRINT*
                                 *LOAD-VERBOSE*
                                 *MACROEXPAND-HOOK*
                                 *PRINT-ARRAY*
                                 *PRINT-BASE*
                                 *PRINT-CASE*
                                 *PRINT-CIRCLE*
                                 *PRINT-ESCAPE*
                                 *PRINT-GENSYM*
                                 *PRINT-LENGTH*
                                 *PRINT-LEVEL*
                                 *PRINT-LINES*
                                 *PRINT-MISER-WIDTH*
                                 *PRINT-PPRINT-DISPATCH*
                                 *PRINT-PRETTY*
                                 *PRINT-RADIX*
                                 *PRINT-READABLY*
                                 *READ-BASE*
                                 *READ-DEFAULT-FLOAT-FORMAT*
                                 *READ-EVAL*
                                 *READ-SUPPRESS*))))))


(defmacro with-environment (environment &body body)
  "
Executes the BODY in the given ENVIRONMENT.

The environment is updated when the BODY exits (locally or not) with
the new values.  However, *PACKAGE* and *RANDOM-STATE* are reset to
default values if they are not of the right type."
  (let ((vbindings (gensym)))
    `(let ((,vbindings (environment-bindings ,environment)))
       (progv
           (mapcar (function car) ,vbindings)
           (mapcar (function cdr) ,vbindings)
         (unwind-protect
              (progn ,@body)

           ;; Restore a sane environment:
           (unless (packagep *package*)
             (setf *package* (load-time-value (find-package "COMMON-LISP-USER"))))
           (unless (random-state-p *random-state*)
             (setf *random-state* (make-random-state *RANDOM-STATE*)))
           ;; Save the changes:
           (dolist (binding ,vbindings)
             (setf (cdr binding) (symbol-value (car binding)))))))))


;;;; THE END ;;;;

