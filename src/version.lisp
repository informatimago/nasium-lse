;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               version.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines contains the *version* variable,
;;;;    and the VERSION function.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-20 <PJB> Added auto-decrement of version number.
;;;;    2012-02-15 <PJB> Created.
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

(in-package "COM.INFORMATIMAGO.LSE")


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *version*
    #. (progn
         (format nil "1.0.0-~5,3F" (decf (SEXP-FILE-CONTENTS
                                          (make-pathname :name "VERSION" :type nil :version nil
                                                         :defaults *load-truename*)
                                          :if-does-not-exist :create)
                                         0.001)))
    "The version of the EMULSE system.")


  (defparameter  *license*
    #. (TEXT-FILE-CONTENTS "../COPYING")))


(defvar *title-banner* "

NASIUM L.S.E.
LANGAGE SYMBOLIQUE D'ENSEIGNEMENT
VERSION ~A
COPYRIGHT 1984 - 2012 PASCAL BOURGUIGNON

DISTRIBUE SELON LES TERMES DE LA LICENCE AGPLv3.
")


(defun system-release ()
  ;; TODO: 
  "")

(defvar *actual-version* nil)

(defun version ()
  (or *actual-version*
      (setf *actual-version*
            (format nil "~:@(~A-~A-~A~)" *version*
                         (or (cdr (assoc (lisp-implementation-type)
                                         '(("Armed Bear Common Lisp" . "abcl")
                                           ("Clozure Common Lisp"    . "ccl")
                                           ("CLISP"                  . "clisp")
                                           ("CMU Common Lisp"        . "cmucl")
                                           ("ECL"                    . "ecl")
                                           ("SBCL"                   . "sbcl"))
                                         :test (function string-equal)))
                             "cl")
                         #+bsd     "bsd"
                         #+darwin  "darwin" ;; (system-release)
                         #+linux   "linux"
                         #+(or win32 windows) "windows"
                         #+(and (not (or bsd darwin linux win32 windows)) unix) "unix"
                         #+(not (or bsd darwin linux unix win32 windows)) ""))))

;;;; THE END ;;;;
