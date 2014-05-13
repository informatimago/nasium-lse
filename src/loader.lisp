;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               loader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loads the LSE interpreter and environment emulator.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-23 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2014
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
;;;;****************************************************************************

(in-package :cl-user)
(defun quick-reload (&rest systems)
  "Delete and reload the ASDF systems."
  (map 'list (lambda (system)
               (asdf:clear-system system)
               (ql:quickload system))
       systems))
(defun dirpath  (path) (make-pathname :name nil   :type nil   :version nil :defaults path))
(defun wildpath (path) (make-pathname :name :wild :type :wild :version nil :defaults path))
(defun fasldir  (system component)
  (first (asdf:output-files
          (make-instance 'asdf:compile-op)
          (asdf:find-component (asdf:find-system system) component))))

(setf *default-pathname-defaults* (dirpath (or *load-truename*
                                               *compile-file-truename*)))
(pushnew *default-pathname-defaults* asdf:*central-registry* :test (function equal))
(pushnew (truename (merge-pathnames "../dependencies/" *default-pathname-defaults*))
         ql:*local-project-directories* :test (function equal))
(quicklisp-client:register-local-projects)


;; (defun delete-package-and-users (package)
;;   (mapc 'delete-package-and-users  (package-used-by-list package))
;;   (unless (member package (list (find-package "COMMON-LISP")
;;                                 (find-package "COMMON-LISP-USER")))
;;     (princ "Deleting ") (princ (package-name package)) (terpri) (finish-output)
;;     (ignore-errors (delete-package package))))
;; 
;; (progn (ignore-errors (delete-package-and-users :com.informatimago.rdp))
;;        (delete-package-and-users :com.informatimago.common-lisp.parser.scanner)
;;        (delete-package-and-users :com.informatimago.common-lisp.cesarum.list))
;; 
;; 
;; (pushnew #P "/home/pjb/src/public/lisp/common-lisp/cesarum/" asdf:*central-registry*)
;; (quick-reload :com.informatimago.common-lisp.cesarum)
;; 
;; (pushnew #P "/home/pjb/src/public/lisp/common-lisp/parser/"  asdf:*central-registry*)
;; (quick-reload :com.informatimago.common-lisp.parser)
;; 
;; (pushnew #P "/home/pjb/src/public/rdp/"                      asdf:*central-registry*)
;; (quick-reload :com.informatimago.rdp)


;; (setf *features* (set-difference *features* '(:lse-scanner-debug :lse-input-debug)))
;; (pushnew :lse-scanner-debug    *features*)
(pushnew :debugging            *features*)
(pushnew :lse-case-insensitive *features*)
(pushnew :lse-unix             *features*)
(pushnew :lse-extensions       *features*)
#-(and) (pushnew :lse-mitra-15             *features*)
#-(and) (pushnew :lse-t1600                *features*)


(setf *print-right-margin* 200
      *print-pretty* t
      *print-case* :downcase)


(let ((dir (funcall (function #+windows wildpath #-windows dirpath)
                    (fasldir :com.informatimago.manifest "manifest"))))
  (format t "~%~A~%" dir) (finish-output)
  #+windows (mapc 'delete-file (directory dir))
  #-windows (asdf:run-shell-command "rm -rf ~S" (namestring dir)))


(when (and (find-package "COM.INFORMATIMAGO.RDP")
           (find-symbol "*BOILERPLATE-GENERATED*" "COM.INFORMATIMAGO.RDP")
           (boundp (find-symbol "*BOILERPLATE-GENERATED*" "COM.INFORMATIMAGO.RDP")))
  (setf (symbol-value (find-symbol "*BOILERPLATE-GENERATED*" "COM.INFORMATIMAGO.RDP")) nil))

(in-package :cl-user)
(progn (ignore-errors (delete-package :com.informatimago.lse.server))
       (ignore-errors (delete-package :com.informatimago.lse))
       (ignore-errors (delete-package :com.informatimago.lse.byte-code))
       (ignore-errors (delete-package :com.informatimago.lse.identifiers)))

(quick-reload :com.informatimago.lse.cli)
(quick-reload :com.informatimago.lse.server)
(quick-reload :com.informatimago.manifest)

(com.informatimago.common-lisp.cesarum.package:add-nickname
 :COM.INFORMATIMAGO.LSE.IDENTIFIERS :id)
(in-package   :com.informatimago.lse)
(print '(in-package   :com.informatimago.lse)) 
(print '(com.informatimago.lse.cli:main))
(print '(com.informatimago.lse.server:main))
(terpri) (finish-output)





#|
    (cd #P "/home/pjb/src/pjb/nasium-lse/src/")
    (load "loader.lisp")
|#

;;;; THE END ;;;;

