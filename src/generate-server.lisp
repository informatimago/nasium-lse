;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-server.lisp
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
;;;    This program is free software: you can redistribute it and/or modify
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

(in-package "COMMON-LISP-USER")

#+ccl (setf ccl:*default-external-format*           :unix
            ccl:*default-file-character-encoding*   :utf-8
            ccl:*default-line-termination*          :unix
            ccl:*default-socket-character-encoding* :utf-8)

(load #P"~/quicklisp/setup.lisp")

(setf *print-right-margin* 80
      *print-pretty* t
      *print-case* :downcase)

(defun dirpath  (path) (make-pathname :name nil   :type nil   :version nil :defaults path))
(defun wildpath (path) (make-pathname :name :wild :type :wild :version nil :defaults path))
(defun fasldir  (system component)
  (first (asdf:output-files
          (make-instance 'asdf:compile-op)
          (asdf:find-component (asdf:find-system system) component))))

(setf *default-pathname-defaults* (dirpath (or *load-truename*
                                               *compile-file-truename*)))
(pushnew *default-pathname-defaults* asdf:*central-registry* :test 'equal)

(defparameter *program-name* "lse-server")
(defparameter *program-system*  :com.informatimago.lse.server)

(pushnew :debugging            *features*)
(pushnew :lse-case-insensitive *features*)
(pushnew :lse-unix             *features*)
(pushnew :lse-extensions       *features*)
#-(and) (pushnew :lse-mitra-15             *features*)
#-(and) (pushnew :lse-t1600                *features*)


(let ((dir (funcall (function #+windows wildpath #-windows dirpath)
                    (fasldir :com.informatimago.manifest "manifest"))))
  (format t "~%~A~%" dir) (finish-output)
  #+windows (mapc 'delete-file (directory dir))
  #-windows (asdf:run-shell-command "rm -rf ~S" (namestring dir)))



(ql:quickload *program-system*)
(ql:quickload :com.informatimago.manifest)
(shadow 'date)
(use-package "COM.INFORMATIMAGO.MANIFEST")

;;;---------------------------------------------------------------------
;;; Let's run some tests:

(in-package "COM.INFORMATIMAGO.LSE")
(format t "~2%Running a few tests.~%")
(finish-output)

(unless (fboundp 'etl)
  (format t "ETL not bound~% *features* = ~S~%" *features*)
  (finish-output)
  #+ccl (ccl:quit))

(setf  ccl:*backtrace-print-level* nil)
(test/fonctions :silence t)

#+debugging (setf *debug-vm*   '(:error)
                  *debug-repl* t)
#-debugging (setf *debug-vm*   '()
                  *debug-repl* nil)


;;;---------------------------------------------------------------------
;;; Let's generate the target.

(in-package "COMMON-LISP-USER")
(format t "~%Generating ~A~%" (executable-filename *program-name*))
(finish-output)

(write-manifest *program-name* *program-system*)


#+ccl (progn (princ "ccl:save-application will exit.") (terpri) (finish-output))
#+ccl (ccl:save-application
       (executable-filename *program-name*)
       :toplevel-function (function com.informatimago.lse.server:main)
       :init-file nil
       :error-handler :quit-quietly
       ;; :application-class ccl:lisp-development-system
       ;; :clear-clos-cache t
       :purify nil
       ;; :impurify t
       :mode #o755
       :prepend-kernel t
       ;; :native t
       ) 

#+clisp (ext:saveinitmem
         (executable-filename *program-name*)
         :quiet t
         :verbose t
         :norc t
         :init-function (lambda ()
                          (ext:exit (handler-case
                                        (com.informatimago.lse.cli:main)
                                      (error ()
                                        1))))
         :script t
         :documentation "Système & Interpréteur L.S.E"
         :start-package "COMMON-LISP-USER"
         :keep-global-handlers nil
         :executable t)
#+clisp (ext:quit)


;; (print (list (find :swank *features*) (find-package "SWANK")))
;; (terpri)
;; (finish-output)

#|
    (cd "/home/pjb/src/pjb/nasium-lse/src/")
    (load "generate-server.lisp")
|#
;;;; THE END ;;;;
