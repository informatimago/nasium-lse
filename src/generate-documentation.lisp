;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-documentation.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This scripts generates the on-line user documentation as HTML.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-14 <PJB> Created.
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

(load "manifest.lisp")
(use-package "COM.INFORMATIMAGO.MANIFEST")


(defparameter *program-name* "lse")
(defparameter *program-system*  :com.informatimago.lse.unix-cli)

(pushnew :developing           *features*)
(pushnew :lse-case-insensitive *features*)
(pushnew :lse-unix             *features*)
(pushnew :lse-extensions       *features*)
#-(and) (pushnew :lse-mitra-15             *features*)
#-(and) (pushnew :lse-t1600                *features*)


(ql:quickload *program-system*)
(ql:quickload :com.informatimago.manifest)
(ql:quickload :com.informatimago.lse.html-doc)
(use-package "COM.INFORMATIMAGO.MANIFEST")




;;;---------------------------------------------------------------------
;;; Let's generate the target.

(format t "~%Generating ~A~%" (executable-filename *program-name*))
(finish-output)

(write-manifest *program-name* *program-system*)

#|
    (cd "/home/pjb/src/pjb/lse-cl/src/")
    (load "generate-documentation.lisp")
|#
;;;; THE END ;;;;
