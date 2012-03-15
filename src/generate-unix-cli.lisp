;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-unix-cli.lisp
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
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "CL-USER")
(setf *print-right-margin* 80
      *print-pretty* t
      *print-case* :downcase)

#+windows-target (cd #P"/cygwin/home/pjb/src/pjb/lse-cl/src/")
#-windows-target (cd #P"/home/pjb/src/pjb/lse-cl/src/")
(pushnew (pwd) asdf:*central-registry* :test 'equal)



(defparameter *program-name* "lse")
(defparameter *program-system*  :com.informatimago.lse.unix-cli)

(pushnew :developing           *features*)
(pushnew :lse-case-insensitive *features*)
(pushnew :lse-unix             *features*)
(pushnew :lse-extensions       *features*)
#-(and) (pushnew :lse-mitra-15             *features*)
#-(and) (pushnew :lse-t1600                *features*)


#+ (and ccl linux) (asdf:run-shell-command "rm -rf /home/pjb/.cache/common-lisp/kuiper.lan.informatimago.com/ccl-1.7-f94-linux-amd64/home/pjb/src/git/pjb/lse-cl/src/")
#+ (and ccl darwin) (asdf:run-shell-command "rm -rf /Users/pjb/.cache/common-lisp/triton.lan.informatimago.com/ccl-1.7-f94-macosx-ppc32/home/pjb/src/git/pjb/lse-cl/src/")
#+(and ccl windows)
(mapc 'delete-file (directory "C:/cygwin/home/pjb/.cache/common-lisp/lassell/ccl-1.7-f95-win-amd64/C/cygwin/home/pjb/src/pjb/lse-cl/src/*.*"))
#+ (and clisp linux) (asdf:run-shell-command "rm -rf /home/pjb/.cache/common-lisp/kuiper.lan.informatimago.com/clisp-2.49-unix/home/pjb/src/git/pjb/lse-cl/src/")
#+ (and clisp darwin) (asdf:run-shell-command "rm -rf /Users/pjb/.cache/common-lisp/triton.lan.informatimago.com/clisp-2.49-unix/home/pjb/src/git/pjb/lse-cl/src/")

(ql:quickload *program-system*)
(ql:quickload :com.informatimago.manifest)
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

(setf *debug-vm*   t
      *debug-repl* t)


;;;---------------------------------------------------------------------
;;; Let's generate the target.


(format t "~%Generating ~A~%" (executable-filename *program-name*))
(finish-output)

(write-manifest *program-name* *program-system*)


#+ccl (progn (princ "ccl:save-application will exit.") (terpri) (finish-output))
#+ccl (ccl:save-application
       (executable-filename *program-name*)
       :toplevel-function (function com.informatimago.lse.unix-cli:main)
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
                                        (com.informatimago.lse.unix-cli:main)
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
    (cd "/home/pjb/src/pjb/lse-cl/src/")
    (load "generate-unix-cli.lisp")
|#
;;;; THE END ;;;;
