;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-cli.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This scripts generates a unix cli lse executable.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-23 <PJB> Created.
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
(in-package "COMMON-LISP-USER")

(defparameter *program-name*    "lse")
(defparameter *program-system*  :com.informatimago.lse.cli)
(defparameter *program-main*    (lambda () (intern "MAIN" "COM.INFORMATIMAGO.LSE.CLI")))
(defparameter *program-features*
  '(("LSE_COMPILE_SERVER"       . :lse-server)
    ((not "LSE_COMPILE_SERVER") . :lse-allow-lisp) ; gives access to low level lisp command and functions. 
    ;; :lse-scanner-debug    
    ;; :debugging
    :lse-case-insensitive 
    :lse-unix             
    :lse-extensions       
    #-(and)  :lse-mitra-15
    #-(and) :lse-t1600
    ))

;; #+debugging (ql:quickload :swank) ;; cannot load swank since cli.lisp would switch to swank-terminal.

(load "generate-program.lisp")



#-(and) (defun test-main ()
          (let ((bare (com.informatimago.common-lisp.cesarum.stream:bare-stream
                       *standard-input* :direction :input)))
            (com.informatimago.common-lisp.interactive.interactive:show
              (ccl::command-line-arguments)
              ccl:*command-line-argument-list*
              ccl:*unprocessed-command-line-arguments*
              (interactive-stream-p *standard-input*)
              (typep *standard-input* 'file-stream)
              bare
              (type-of *standard-input*)
              *terminal-io*       
              *standard-input*    
              *standard-output*   
              *error-output*      
              *trace-output*   
              *query-io*          
              *debug-io*
              (open (first (last ccl:*command-line-argument-list*)))
              ))
          (finish-output)
          (ccl:quit))


#||
 (print (list (find :swank *features*) (find-package "SWANK")))
 (terpri)
 (finish-output)
 (cd "/home/pjb/src/pjb/nasium-lse/src/")
 (load "generate-cli.lisp")
||#

;;;; THE END ;;;;
