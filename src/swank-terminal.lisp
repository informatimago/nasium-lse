;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               swank-terminal.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defined some slime/swank function to deal with the
;;;;    slime REPL a little more as a terminal.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-20 <PJB> Added redefinition of simple-break to signal
;;;;                     user-interrupt.
;;;;    2012-02-16 <PJB> Created.
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


;;; In Common Lisp, we can execute emacs lisp expressions:

(defparameter *emacs-readtable*
  (let ((rt (copy-readtable)))
    (setf (readtable-case rt) :preserve)
    (set-syntax-from-char #\> #\) rt)
    (set-dispatch-macro-character
     #\# #\<
     (lambda (stream subchar dispchar)
       `(emacs-unreadable ,@(read-delimited-list #\> stream t)))
     rt)
    rt))


;; Probably more readtable patching would be in order.
;;
;; We could define CLOS proxies for emacs objects for a more seamless
;; integration. swank::eval-in-emacs process the CL form to make it
;; "emacs" (eg. downcase symbols, etc).  It could convert CLOS proxies
;; to emacs lisp forms returning the corresponding emacs object.

(defun eval-in-emacs (form &optional nowait)
  #-swank (declare (ignore form nowait))
  #-swank nil
  #+swank
  (let ((result (swank::eval-in-emacs `(format "%S" ,form) nowait))
        (*readtable* *emacs-readtable*))
    (with-input-from-string (in result)
      (let ((result (read in nil in)))
        result))))



;;----------------------------------------------------------------------
;; Swank Terminal
;;----------------------------------------------------------------------

(defclass swank-terminal (standard-terminal)
  ((last-columns   :initform 80)
   (last-rows      :initform 25)
   (buffer         :initform (make-array 80
                                         :element-type 'character
                                         :adjustable t
                                         :fill-pointer 0))
   (current-column :initform 0)))

(defmethod terminal-initialize ((terminal swank-terminal))
  (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                   (font-lock-mode -1)))
  (list (terminal-columns terminal)
        (terminal-rows    terminal)))

(defmethod terminal-finalize ((terminal swank-terminal))
  (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                   (font-lock-mode +1)))
  (values))


(defmethod terminal-columns ((terminal swank-terminal))
  (declare (ignorable terminal))
  (let ((new-columns
         (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                          (let ((windows (remove* (slime-repl-buffer) (window-list)
                                                  :test-not 'eql
                                                  :key 'window-buffer)))
                            (and windows (window-width (first windows))))))))
    (if new-columns
        (setf (slot-value terminal 'last-columns) new-columns)
        (slot-value terminal 'last-columns))))


(defmethod terminal-rows ((terminal swank-terminal))
  (declare (ignorable terminal))
  (let ((new-rows
         (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                          (let ((windows (remove* (slime-repl-buffer) (window-list)
                                                  :test-not 'eql
                                                  :key 'window-buffer)))
                            (and windows (window-height (first windows))))))))
    (if new-rows
        (setf (slot-value terminal 'last-rows) new-rows)
        (slot-value terminal 'last-rows))))


(defmethod terminal-ring-bell ((terminal swank-terminal))
  (let ((output (terminal-output-stream terminal)))
    (eval-in-emacs '(beep t)))
  (values))

(defmethod terminal-carriage-return ((terminal swank-terminal))
  (let ((output (terminal-output-stream terminal)))
    (finish-output output))
  (with-slots (current-column) terminal
    (setf current-column 0))
  (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                   (slime-repl-bol)))
  (values))

(defmethod terminal-move-up ((terminal swank-terminal))
  (let ((output (terminal-output-stream terminal)))
    (finish-output output)
    ;; TODO: When moving up, if the line is shorter, pad it with spaces…
    (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                     (previous-line 1))))
  (values))


(defmethod flush ((terminal swank-terminal))
  "Flush the terminal, ie. outputs the BUFFER line definitively."
  (eval-in-emacs `(with-current-buffer (slime-repl-buffer)
                    (delete-region
                     (progn (slime-repl-bol) (point))
                      (point-max))
                    (slime-move-point (point-max))))
  (with-slots (buffer) terminal
    (let ((output (terminal-output-stream terminal)))
      (princ buffer output)))
  (values))

(defmethod show-buffer ((terminal swank-terminal))
  "Replace the last line ('unflushed') in the slime buffer with the
contents of the BUFFER, moving the cursor too the CURRENT-COLUMN."
  (with-slots (current-column buffer) terminal
    (eval-in-emacs `(with-current-buffer (slime-repl-buffer)
                      (delete-region
                       (progn (slime-repl-bol) (point))
                       (progn (end-of-line)    (point)))
                      (insert ,buffer)
                      (slime-repl-bol)
                      (forward-char ,current-column)
                      (slime-move-point (point))))))


(defmethod terminal-new-line ((terminal swank-terminal) &optional (count 1))
  (flush terminal)
  (with-slots (current-column buffer) terminal
    (let ((output (terminal-output-stream terminal)))
      (loop :repeat count :do (terpri output))
      (terminal-finish-output terminal)
      (setf current-column 0
            (fill-pointer buffer) 0)))
  (values))


(defmethod terminal-line-feed ((terminal swank-terminal) &optional (count 1))
  (flush terminal)
  (with-slots (current-column buffer) terminal
    (let ((output (terminal-output-stream terminal)))
      (loop :repeat count :do (terpri output))
      (terminal-finish-output terminal))
    (fill buffer #\space))
  (show-buffer terminal)
  (values))



;; (let ((terminal (com.informatimago.lse::task-terminal *task*)))
;;   (terminal-new-line terminal 4)
;;   (terminal-write-string terminal "Hello")
;;   (terminal-write-string terminal (format nil "~D" (slot-value terminal 'current-column)))
;;   (terminal-line-feed terminal 4)
;;   (terminal-write-string terminal (format nil "~D" (slot-value terminal 'current-column)))
;;   (terminal-write-string terminal "world!")
;;   (terminal-carriage-return terminal)
;;   (terminal-write-string terminal "Haha!")
;;   (terminal-new-line terminal 4))
;; 
;; (eval-in-emacs `(with-current-buffer (slime-repl-buffer) 
;;                   (slime-repl-bol) (insert "xxxxxxxxxx")
;;                   (slime-repl-bol) (forward-char 6) (insert "y")
;;                   (setf deactivate-mark t)))

(defmethod terminal-write-string ((terminal swank-terminal) string &key (start 0) (end nil))
  ;; Since slime uses insert to insert new text, it doesn't override
  ;; previous text.  Therefore we do it ourselves in a buffer, and
  ;; replace the whole line.
  (with-slots (last-columns current-column buffer) terminal
    (let* ((end        (or end (length string)))
           (size       (- end start))
           (new-column (+ current-column size)))
      (when (plusp size)
        (when (< (array-dimension buffer 0) new-column)
          (setf buffer (adjust-array buffer (max (* 2 (array-dimension buffer 0))
                                                 new-column)
                                     :element-type (array-element-type buffer))))
        (setf (fill-pointer buffer) (max (fill-pointer buffer) new-column))
        (replace buffer string :start1 current-column :start2 start :end2 end)
        (setf current-column new-column)
        (show-buffer terminal))))
  (values))


(defmethod terminal-read-line ((terminal swank-terminal) &key (echo t) (beep nil) (xoff nil))
  ;; (terminal-new-line terminal 0)
  (flush terminal)
  (call-next-method))

(defmethod terminal-read ((terminal swank-terminal) &key (echo t) (beep nil) (xoff nil))
  (terminal-new-line terminal 0)
  (call-next-method))


#+swank
(in-package "SWANK")

#+swank
(defslimefun simple-break (&optional (datum "Interrupt from Emacs") &rest args)
  (with-simple-restart (continue "Continue from break.")
    (signal 'com.informatimago.signal:user-interrupt
            :signal com.informatimago.signal:+sigint+)
    (invoke-slime-debugger (coerce-to-condition datum args))))


;;;; THE END ;;;;
