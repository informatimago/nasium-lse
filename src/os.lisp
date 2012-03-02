;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               os.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file contains some OS function that are implementation dependant.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-02 <PJB> Created.
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

(in-package "COM.INFORMATIMAGO.LSE.OS")

(defun getenv (var)
  #+ccl             (ccl::getenv var)
  #+clisp           (ext:getenv var)
  #-(or ccl
        clisp)      (iolib.syscalls:getenv var))


(defun getuid ()
  #+ccl            (ccl::getuid)
  #+clisp          (posix:uid)
  #-(or ccl
        clisp)     (asdf::get-uid))


(defun run-shell-command (control-string &rest arguments)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *VERBOSE-OUT*.  Returns the shell's exit code."
  #-ccl (asdf:run-shell-command control-string arguments)
  #+ccl
  (let ((command (apply #'format nil control-string args)))
    (asdf-message "; $ ~A~%" command)
    (nth-value 1
               (ccl:external-process-status
                (ccl:run-program #+windows-target "C:/cygwin/bin/sh"
                                 #-windows-target "/bin/sh"
                                 (list "-c" command)
                                 :input nil :output *verbose-out*
                                 :wait t)))))


;;;; THE END ;;;;
