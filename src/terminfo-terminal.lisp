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
;;;;    Copyright Pascal J. Bourguignon 2012 - 2013
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

(in-package "COM.INFORMATIMAGO.LSE.CLI")


(defclass terminfo-terminal (standard-terminal)
  ((terminfo :initarg :terminfo
             :initform (terminfo:set-terminal (getenv "TERM"))
             :reader terminal-terminfo)))


;; (ccl::stream-device *terminal-io*  :input)
;; (ccl::stream-device *terminal-io*  :output)

(defmethod terminal-initialize ((terminal terminfo-terminal))
  terminal)

(defmethod terminal-finalize ((terminal terminfo-terminal))
  terminal)


(defmethod terminal-ring-bell ((terminal terminfo-terminal))
  (let* ((output (terminal-output-stream terminal))
         (terminfo:*terminfo* (terminal-terminfo terminal))
         (bell terminfo:bell))
    (terminfo:tputs bell output)
    (terminal-finish-output terminal)))


;; (defmethod terminal-move-up ((terminal terminfo-terminal))
;;   (let ((output (terminal-output-stream terminal))
;;         (terminfo:*terminfo* (terminal-terminfo terminal))
;;         (cup terminfo:cursor-up))
;;     (terminfo:tputs cup output)
;;     (terminal-finish-output terminal)))


(defmethod terminal-carriage-return ((terminal terminfo-terminal))
  (let ((output (terminal-output-stream terminal))
        (terminfo:*terminfo* (terminal-terminfo terminal))
        (carriage-return terminfo:carriage-return))
    (terminfo:tputs carriage-return output)
    (terminal-finish-output terminal)))


(defmethod terminal-new-line ((terminal terminfo-terminal) &optional (count 1))
  (let ((output (terminal-output-stream terminal))
        (terminfo:*terminfo* (terminal-terminfo terminal))
        (new-line        terminfo:newline)
        (carriage-return terminfo:carriage-return)
        (line-feed       terminfo:cursor-down))
    (cond
      (new-line
       (loop
         :repeat count
         :do (terminfo:tputs new-line output)))
      ((and carriage-return line-feed)
       (loop
         :repeat count
         :do (progn (terminfo:tputs carriage-return output)
                    (terminfo:tputs line-feed output))))
      (t
       (loop
         :repeat count
         :do (terpri output))))
    (terminal-finish-output terminal)))



;;;; THE END ;;;;
