;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-program.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Generated a LSE system programme.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-07-10 <PJB> Extracted from generated-cli.lisp and generated-server.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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


(defun boolean-enval (var default)
  (let ((val (ccl:getenv var)))
    (if val
        (not (not (find val '("T" "Y" "TRUE" "YES" "O" "OUI" "1") :test (function string-equal))))
        default)))

(dolist (feature *program-features*)
  (if (symbolp feature)
      (pushnew feature *features*)
      (when (boolean-enval (car feature) nil)
        (pushnew (cdr feature) *features*))))




(let ((dir (funcall (function #+windows wildpath #-windows dirpath)
                    (fasldir :com.informatimago.manifest "manifest"))))
  (format t "~%~A~%" dir) (finish-output)
  #+windows (mapc 'delete-file (directory dir))
  #-windows (asdf:run-shell-command "rm -rf ~S" (namestring dir)))

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
#-(and) (progn
          (format t "~2%Running a few tests.~%")
          (finish-output)

          (unless (fboundp 'etl)
            (format t "ETL not bound~% *features* = ~S~%" *features*)
            (finish-output)
            #+ccl (ccl:quit))

          (setf  ccl:*backtrace-print-level* nil)
          (test/fonctions :silence t))

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
       :toplevel-function (coerce (if (boolean-enval "LSE_USE_EXIT" nil)
                                      `(lambda ()
                                         (#__exit (,(funcall *program-main*))))
                                      `(lambda ()
                                         (ccl:quit (,(funcall *program-main*))
                                                   :error-handler (lambda (err)
                                                                    (declare (ignore err))
                                                                    (#__exit -1)))))
                                  'function)
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
         :init-function (coerce `(lambda ()
                                   (ext:exit (handler-case
                                                 (,(funcall *program-main*) ext:*args*)
                                               (error ()
                                                 1))))
                                'function)
         :script t
         :documentation "Système & Interpréteur L.S.E"
         :start-package "COMMON-LISP-USER"
         :keep-global-handlers nil
         :executable t)
#+clisp (ext:quit)


;;;; THE END ;;;;