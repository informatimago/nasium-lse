;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               logger.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION:
;;;;
;;;;    Logger.
;;;;    The current implementation trivially writes to *trace-output*.
;;;;    Will have to be reimplemented using syslog.
;;;;
;;;;AUTHORS:
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Created.
;;;;BUGS:
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
;;;;**********************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(DEFPACKAGE "COM.INFORMATIMAGO.LOGGER"
  (:use "COMMON-LISP" "SPLIT-SEQUENCE")
  (:export "*IGNORE-MODULES*"
           "LEVELS" "*MINIMUM-LOG-LEVEL*" "LEVEL<"
           "*USE-COLOR*"
           "LOGGER"))
(in-package "COM.INFORMATIMAGO.LOGGER")

(defvar *ignore-modules* '()
  "LOGGER won't log messages from these modules.")



(defparameter *levels* '(:debug :info :notice :warn :error :fatal)
  "Log levels in increasing priority order.")

(defun levels ()
  "Returns the literal list of log levels in increasing priority order."
  *levels*)

(defvar *minimum-log-level* :debug
  "LOGGER won't log levels that are less than *MINIMUM-LOG-LEVEL*")

(defun level< (a b)
  "The order function between two log levels."
  (member b (rest (member a *levels*))))



(defvar *last-universal-time* 0
  "The last time we cached the universal-time.")
(defvar *last-timestamp* "00000000T000000"
  "The last cached formated universal-time.")
(defun get-current-timestamp ()
  "The current formated universal-time (taken from the cache if it's within the same second)."
  (let ((now (get-universal-time)))
    (if (= *last-universal-time* now)
        *last-timestamp*
        (multiple-value-bind (se mi ho da mo ye) (decode-universal-time now)
          (setf *last-timestamp* (format nil
                                   "~4,'0D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0D"
                                   ye mo da ho mi se))))))



(defun sgr* (parameters)
  "Return a string containing the ANSI code SGR function (Select Graphic Rendition)."
  (format nil "~C[~{~A~^;~}m" (code-char 27) parameters))

(defmacro sgr (&body expressions)
  "Given a set of symbol designators representing graphic renditions,
builds the corresponding call to the SGR* function (with numerical arguments)."
  (let ((codes   '(("NORMAL"    0)
                   ("BOLD"      1 22)
                   ("UNDERLINE" 4 24)
                   ("BLINK"     5 25)
                   ("INVERT"    7 27)))
        (colors  '(("BLACK"    . 30)
                   ("RED"      . 31)
                   ("GREEN"    . 32)
                   ("YELLOW"   . 33)
                   ("BLUE"     . 34)
                   ("MAGENTA"  . 35)
                   ("CYAN"     . 36)
                   ("WHITE"    . 37))))
    (flet ((string-designator-p (object)
             (or (stringp object) (symbolp object) (characterp object))))
      `(sgr* ',(mapcar (lambda (expression)
                         (cond
                           ((atom expression)
                            (let ((code  (cdr (assoc expression codes  :test (function string-equal)))))
                              (if code
                                  (car code)
                                  (let ((color (cdr (assoc expression colors :test (function string-equal)))))
                                    (if color
                                        color
                                        (error "~S is not a valid SGR expression." expression))))))
                           ((and (string-designator-p (first expression))
                                 (string-equal (first expression) "NO")
                                 (= 2 (length expression))
                                 (string-designator-p (second expression)))
                            (let ((code  (cdr (assoc (second expression) codes  :test (function string-equal)))))
                              (if (cdr code)
                                  (cadr code)
                                  (error "~S is not a valid SGR expression." expression))))
                           ((and (string-designator-p (first expression))
                                 (string-equal (first expression) "FOREGROUND")
                                 (= 2 (length expression))
                                 (string-designator-p (second expression)))
                            (let ((color (cdr (assoc (second expression) colors :test (function string-equal)))))
                              (if color
                                  color
                                  (error "~S is not a valid SGR expression." expression))))
                           ((and (string-designator-p (first expression))
                                 (string-equal (first expression) "BACKGROUND")
                                 (= 2 (length expression))
                                 (string-designator-p (second expression)))
                            (let ((color (cdr (assoc (second expression) colors :test (function string-equal)))))
                              (if color
                                  (+ 10 color)
                                  (error "~S is not a valid SGR expression." expression))))
                           (t
                            (error "~S is not a valid SGR expression." expression))))
                       expressions)))))



;; (sgr normal bold underline blink invert (no bold) (no underline) (no
;;      blink) (no invert) black red green yellow blue magenta cyan white
;;      (foreground black) (foreground red) (foreground green)
;;      (foreground yellow) (foreground blue) (foreground magenta)
;;      (foreground cyan) (foreground white) (background black)
;;      (background red) (background green) (background yellow)
;;      (background blue) (background magenta) (background cyan)
;;      (background white))


(defvar *use-color* nil ; doesn't work in slime
  "Whether the logs should be written with colors matching the level.")
;; (setf *use-color* nil)


(defvar *color-map*
  (list (cons :debug  (sgr bold cyan))
        (cons :info   (sgr bold green))
        (cons :notice (sgr bold yellow))
        (cons :warn   (sgr bold magenta))
        (cons :error  (sgr bold red))
        (cons :fatal  (sgr bold black (background red))))
  "An a-lisp mapping the log levels to strings containing the ANSI codes to set their color.")

(defun level-color (level)
  "A string containing the ANSI codes to set the color of the level."
  (or (cdr (assoc level *color-map*)) ""))

(defun normal-color ()
  "A string containing the ANSI codes to revert to the normal rendition."
  (sgr normal))


(defvar *log-stream*       (make-synonym-stream '*trace-output*))

(defvar *log-error-stream* (make-broadcast-stream
                            (make-synonym-stream '*error-output*)
                            (make-synonym-stream '*log-stream*)))

(defvar *last-line* nil
  "Remember last line to avoid multiple duplicates.")

(defvar *last-count* 0
  "Number of times the *LAST-LINE* was issued.")

(defvar *max-module-length* 20)

(defun logger (module level control-string &rest arguments)
  ;; TODO: We should add a MESSAGE-ID parameter to identify easily the logged messages.
  "
Writes a message to the log, on behalf of the MODULE.  The message has
an importance given by the LEVEL and is built by formating the
CONTROL-STRING with the ARGUMENTS.
The message is written
"
  (unless (or (member module *ignore-modules*)
              (level< level *minimum-log-level*))
    (let ((*print-pretty* nil)
          (*print-escape* nil)
          (*print-case*   :upcase)
          (*print-right-margin* 2000)
          (output (if (level< level :error)
                      *log-stream*
                      *log-error-stream*))
          (col (if *use-color*
                   (level-color level)
                   ""))
          (cor (if *use-color*
                   (normal-color)
                   "")))
      (setf *max-module-length* (max *max-module-length* (length (princ-to-string module))))
      (let ((line (format nil (format nil "~~15A ~~A~~6A~~A ~~VA ~~{~~A~~^~~%~~~AT~~}~~%"
                                      (+ 24 *max-module-length*))
                     (get-current-timestamp)
                     col level cor
                     *max-module-length*
                     module
                     (split-sequence #\newline (format nil "~?" control-string arguments)
                                     :remove-empty-subseqs t))))
        (if (and *last-line* (string= *last-line* line))
            (incf *last-count*)
            (progn
              (when (and *last-line* (< 1 *last-count*))
                (format output "[~D times]~%" *last-count*))
              (setf *last-line* line
                    *last-count* 1)
              (write-string line output)
              (force-output output)))))))


;;;; THE END ;;;;
