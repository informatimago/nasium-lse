;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               terminfo-terminal.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines a terminal specified by a terminfo entry.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-24 <PJB> Created.
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

(in-package "COM.INFORMATIMAGO.LSE.UNIX-CLI")


(defclass terminfo-terminal (standard-terminal)
  ((terminfo :initarg :terminfo
             :reader terminal-terminfo)))


(defmethod terminal-ring-bell ((terminal terminfo-terminal))
  (let* ((output (terminal-output-stream terminal))
         (terminfo:*terminfo* (terminal-terminfo terminal))
         (bell terminfo:bell))
    (when bell
      (princ bell output))
    (terminal-finish-output terminal)))


(defmethod terminal-move-up ((terminal terminfo-terminal))
  (let ((output (terminal-output-stream terminal))
        (terminfo:*terminfo* (terminal-terminfo terminal))
        (up terminfo:cursor-up))
    (when cup
      (princ up output))
    (terminal-finish-output terminal)))


(defmethod terminal-carriage-return ((terminal terminfo-terminal))
  (let ((output (terminal-output-stream terminal))
        (terminfo:*terminfo* (terminal-terminfo terminal))
        (carriage-return terminfo:carriage-return))
    (when carriage-return
      (princ carriage-return output))
    (terminal-finish-output terminal)))


(defmethod terminal-new-line ((terminal terminfo-terminal) &optional (count 1))
  (let ((output (terminal-output-stream terminal))
        (terminfo:*terminfo* (terminal-terminfo terminal))
        (newline         terminfo:newline)
        (carriage-return terminfo:carriage-return)
        (line-feed       terminfo:cursor-down))
    (cond
      (newline
       (loop
         :repeat count :do
         (princ newline output)))
      ((and carriage-return line-feed)
       (loop
         :repeat count
         :do (princ carriage-return output) (princ line-feed output)))
      (t
       (loop
         :repeat count
         :do (terpri output))))
    (terminal-finish-output terminal)))



;;;; THE END ;;;;
