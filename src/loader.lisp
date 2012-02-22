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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2005
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
;;;;****************************************************************************

(in-package :cl-user)
(cd      #P "/home/pjb/src/pjb/lse-cl/src/")

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


(pushnew :developing           *features*)
(pushnew :LSE-CASE-INSENSITIVE *features*)
(pushnew :lse-unix             *features*)

(setf *print-right-margin* 200
      *print-pretty* t
      *print-case* :downcase)

(asdf:run-shell-command "rm -rf /home/pjb/.cache/common-lisp/kuiper.lan.informatimago.com/ccl-1.7-f94-linux-amd64/home/pjb/src/git/pjb/lse-cl/src/")

(in-package :cl-user)
(progn (ignore-errors (delete-package :com.informatimago.lse.server))
       (ignore-errors (delete-package :com.informatimago.lse))
       (ignore-errors (delete-package :com.informatimago.lse.byte-code))
       (ignore-errors (delete-package :com.informatimago.lse.identifiers)))
(pushnew #P "/home/pjb/src/pjb/lse-cl/src/"                  asdf:*central-registry*)
(quick-reload :com.informatimago.lse)
(com.informatimago.common-lisp.cesarum.package:add-nickname :COM.INFORMATIMAGO.LSE.IDENTIFIERS :id)
(in-package   :com.informatimago.lse)
(print '(in-package   :com.informatimago.lse)) (terpri) (finish-output)


;;;; THE END ;;;;
