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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.LSE")


;;; In Common Lisp, we can execute emacs lisp expressions:

(defparameter *emacs-readtable*
  (let ((rt (copy-readtable)))
    (setf (readtable-case rt) :preserve)
    (set-syntax-from-char #\> #\) rt)
    (set-dispatch-macro-character #\# #\<
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
  ((last-columns :initform 80)
   (last-rows    :initform 25)))

(defmethod terminal-initialize ((terminal swank-terminal))
  (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                   (font-lock-mode -1))))

(defmethod terminal-finalize ((terminal swank-terminal))
  (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                   (font-lock-mode +1))))


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
    (eval-in-emacs '(beep t))))

(defmethod terminal-beginning-of-line ((terminal swank-terminal))
  (let ((output (terminal-output-stream terminal)))
    (finish-output output)
    (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                     (delete-region
                      (progn (slime-repl-bol) (previous-line 1) (point))
                      (point-max))))))



#+swank
(in-package "SWANK")

#+swank
(defslimefun simple-break (&optional (datum "Interrupt from Emacs") &rest args)
  (with-simple-restart (continue "Continue from break.")
    (signal 'com.informatimago.signal:user-interrupt
            :signal com.informatimago.signal:+sigint+)
    (invoke-slime-debugger (coerce-to-condition datum args))))


;;;; THE END ;;;;
