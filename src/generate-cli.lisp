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
(in-package "COMMON-LISP-USER")

#+ccl (setf ccl:*default-external-format*           :unix
            ccl:*default-file-character-encoding*   :utf-8
            ccl:*default-line-termination*          :unix
            ccl:*default-socket-character-encoding* :utf-8)

(load #P"~/quicklisp/setup.lisp")


;;; --------------------------------------------------

(declaim (optimize
          (speed 0)
          (space 0)
          (safety 3)
          (debug 3)
          (compilation-speed 0)
          #+:lispworks (hcl:fixnum-safety 3)))

(defpackage :cl-ppcre (:use :cl))

(defparameter cl-ppcre::*standard-optimize-settings*
  '(optimize
    (speed 0)
    (space 0)
    (safety 3)
    (debug 3)
    (compilation-speed 0)
    #+:lispworks (hcl:fixnum-safety 3))
  "Don't fuck with me!")

(defparameter cl-ppcre::*special-optimize-settings*
  cl-ppcre::*standard-optimize-settings*
  "Don't fuck with me!")

;;; --------------------------------------------------

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
(push (truename (merge-pathnames "../dependencies/"
                                 *default-pathname-defaults*))
      ql:*local-project-directories*)


(defparameter *program-name* "lse")
(defparameter *program-system*  :com.informatimago.lse.cli)


;; (pushnew :lse-scanner-debug    *features*)
;; (pushnew :debugging            *features*)
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

;; #+debugging (ql:quickload :swank) ;; cannot load swank since cli.lisp will switch to swank-terminal.

(ql:quickload *program-system*)

(defparameter *versions* (com.informatimago.lse:versions))
(loop
  :for version :in *versions*
  :for file :in '("macosx/VERSION" "macosx/VERSION_SHORT" "macosx/VERSION_LONG")
  :do (setf (com.informatimago.common-lisp.cesarum.file:text-file-contents file) version))

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
;; (in-package :ccl)
;; (defun reopen-user-libraries ()
;;   (dolist (lib *shared-libraries*)
;;     (setf (shlib.handle lib) nil
;;           (shlib.base lib) nil))
;;   (dolist (lib *shared-libraries*)
;;     (when  (shlib.soname lib)
;;       (with-cstrs ((cname (shlib.soname lib)))
;;         (let* ((handle (ff-call *dlopen-entry*
;;                                 :address cname
;;                                 :int (logior #$RTLD_GLOBAL #$RTLD_NOW)
;;                                 :address)))
;;           (unless (%null-ptr-p handle)
;;             (setf (shlib.handle lib) handle)))))))
;; (in-package "COMMON-LISP-USER")

#+ccl (dolist (lib ccl::*shared-libraries* (terpri))
        (print lib))
#+ccl (progn (princ "ccl:save-application will exit.") (terpri) (finish-output))
#+ccl (ccl:save-application
       (executable-filename *program-name*)
       :toplevel-function (function com.informatimago.lse.cli:main)
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
                                        (com.informatimago.lse.cli:main ext:*args*)
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
    (load "generate-cli.lisp")
|#
;;;; THE END ;;;;
