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

#+swank
(in-package "SWANK")

#+swank
(defslimefun simple-break (&optional (datum "Interrupt from Emacs") &rest args)
  (with-simple-restart (continue "Continue from break.")
    (signal 'com.informatimago.signal:user-interrupt
            :signal com.informatimago.signal:+sigint+)
    (invoke-slime-debugger (coerce-to-condition datum args))))


(in-package "COM.INFORMATIMAGO.LSE.UNIX-CLI")


;;; In Common Lisp, we can execute emacs lisp expressions:

(defparameter *emacs-readtable*
  (let ((rt (copy-readtable)))
    (setf (readtable-case rt) :preserve)
    (set-syntax-from-char #\> #\) rt)
    (set-dispatch-macro-character
     #\# #\<
     (lambda (stream subchar dispchar)
       (declare (ignore subchar dispchar))
       `(emacs-unreadable ,@(read-delimited-list #\> stream t)))
     rt)
    rt))


;; Probably more readtable patching would be in order.
;;
;; We could define CLOS proxies for emacs objects for a more seamless
;; integration. swank::eval-in-emacs process the CL form to make it
;; "emacs" (eg. downcase symbols, etc).  It could convert CLOS proxies
;; to emacs lisp forms returning the corresponding emacs object.



(defvar *debug-swank* nil)
;; (setf  *debug-swank* t)
;; (setf  *debug-swank* nil)

(defun eval-in-emacs (form &optional nowait)
  #-swank (declare (ignore form nowait))
  #-swank nil
  #+swank
  (let ((emacs-form  `(format "%S" ,form)))
    (when *debug-swank*
      (let  ((*print-case* :downcase)
             (*print-escape* nil)
             (*print-right-margin* 100)
             (*package* (find-package "COM.INFORMATIMAGO.LSE.UNIX-CLI")))
        (pprint form *trace-output*))
      (terpri *trace-output*))
    (let ((result (swank::eval-in-emacs emacs-form nowait))
          (*readtable* *emacs-readtable*))
      (with-input-from-string (in result)
        (let ((result (read in nil in)))
          result)))))



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
                   (font-lock-mode -1)
                   (overwrite-mode +1))) ;; for user input.
  (list (terminal-columns terminal)
        (terminal-rows    terminal)))

(defmethod terminal-finalize ((terminal swank-terminal))
  (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                   (font-lock-mode +1)
                   (overwrite-mode -1)))
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
  (declare (ignorable terminal))
  (eval-in-emacs '(beep t))
  (values))


(defun adjust-buffer (buffer new-length)
  (if (< (array-dimension buffer 0) new-length)
      (adjust-array buffer (max (* 2 (array-dimension buffer 0))
                                new-length)
                    :element-type (array-element-type buffer)
                    :fill-pointer new-length)
      (progn
        (setf (fill-pointer buffer) new-length)
        buffer)))

(defmethod buffer-carriage-return ((terminal swank-terminal))
  (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                   (beginning-of-line)
                   (slime-move-point (point))))
  (with-slots (current-column) terminal
    (setf current-column 0))
  (values))


(defun gen-move-to-column (current-column)
  `(progn
     (beginning-of-line)
     (let ((start (point))
           (end   (point-max))
           (ccol  ,current-column))
       (when (< (- end start) ccol)
         (goto-char end)
         (insert (make-string (- ccol (- end start)) 32))
         (beginning-of-line))
       (forward-char ccol)
       (slime-move-point (point)))))

(defmethod buffer-move-to-column ((terminal swank-terminal))
  (with-slots (current-column buffer) terminal
   (eval-in-emacs
    `(with-current-buffer (slime-repl-buffer)
       (message "column = %3d buffer = %S" ,current-column ,buffer)
       ,(gen-move-to-column current-column)))))

(defmethod buffer-show ((terminal swank-terminal) &key set-input)
  "Replace the last line ('unflushed') in the slime buffer with the
contents of the BUFFER, moving the cursor to the CURRENT-COLUMN."
  (with-slots (current-column buffer) terminal
    (eval-in-emacs
     `(with-current-buffer (slime-repl-buffer)
        (message "show buffer =         %S" ,buffer)
        (delete-region
         (progn (beginning-of-line) (point))
         (progn (end-of-line)       (point)))
        (insert ,buffer)
        ,(gen-move-to-column current-column)
        ,@ (when set-input
               '((delete-region (point) (point-max))
                 (slime-mark-input-start)))))))

(defmethod buffer-flush ((terminal swank-terminal))
  "Flush the terminal, ie. outputs the BUFFER line definitively."
  (eval-in-emacs
   `(with-current-buffer (slime-repl-buffer)
      (delete-region (progn (beginning-of-line) (point))
                     (point-max))
      (slime-move-point (point-max))))
  (with-slots (current-column buffer) terminal
    (let ((output (terminal-output-stream terminal)))
      (princ buffer output))
    (buffer-move-to-column terminal))
  (values))

(defmethod buffer-update-from-slime ((terminal swank-terminal) &key (update-column nil))
  (let ((new-line (eval-in-emacs
                   '(with-current-buffer (slime-repl-buffer)
                     (buffer-substring-no-properties
                      (progn (beginning-of-line) (point))
                      (progn (end-of-line)       (point)))))))
    (eval-in-emacs `(message "           new-line = %S" ,new-line))
    (with-slots (current-column buffer) terminal
      (when update-column
        (setf current-column (length new-line)))
      (eval-in-emacs
       `(with-current-buffer (slime-repl-buffer)
          ,@ (when (< (length new-line) current-column)
               `((insert (make-string ,(- current-column (length new-line)) 32))))
             (beginning-of-line)
             (forward-char ,current-column)
             (slime-move-point (point))))
      (let ((new-length (max (length new-line) current-column)))
        (setf buffer (adjust-buffer buffer new-length))
        (replace buffer new-line)
        (fill buffer #\space :start (length new-line) :end new-length)
        (eval-in-emacs `(message "column = %3d buffer = %S" ,current-column ,buffer)))))
  (values))

        ;; (setf *b* (COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY:COPY-ARRAY buffer
        ;;            :COPY-FILL-POINTER t :COPY-ADJUSTABLE t))


(defmethod terminal-carriage-return ((terminal swank-terminal))
  (eval-in-emacs '(message "carriage return"))
  (let ((output (terminal-output-stream terminal)))
    (finish-output output))
  (buffer-carriage-return terminal))

(defmethod terminal-move-up ((terminal swank-terminal))
  (eval-in-emacs '(message "move up"))
  (let ((output (terminal-output-stream terminal)))
    (finish-output output))
  (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                   (previous-line 1)))
  (buffer-update-from-slime terminal :update-column nil)
  (values))

(defmethod terminal-new-line ((terminal swank-terminal) &optional (count 1))
  (eval-in-emacs '(message "new line"))
  (buffer-flush terminal)
  (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                   (end-of-line)))
  (with-slots (current-column buffer) terminal
    (let ((output (terminal-output-stream terminal)))
      (loop :repeat count :do (terpri output))
      (terminal-finish-output terminal)
      (setf current-column 0
            (fill-pointer buffer) 0)))
  (buffer-move-to-column terminal)
  (values))

(defmethod terminal-line-feed ((terminal swank-terminal) &optional (count 1))
  (eval-in-emacs '(message "line feed"))
  (buffer-flush terminal)
  (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                   (end-of-line)))
  (with-slots (current-column buffer) terminal
    (let ((output (terminal-output-stream terminal)))
      (loop :repeat count :do (terpri output))
      (terminal-finish-output terminal))
    (setf (fill-pointer buffer) current-column)
    (fill buffer #\space))
  (buffer-show terminal)
  (buffer-move-to-column terminal)
  (values))

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
          (setf buffer (adjust-buffer buffer new-column)))
        (setf (fill-pointer buffer) (max (fill-pointer buffer) new-column))
        (replace buffer string :start1 current-column :start2 start :end2 end)
        (setf current-column new-column)
        (buffer-show terminal))))
  (values))


(defmethod terminal-read-line ((terminal swank-terminal) &key (echo t) (beep nil) (xoff nil))
  (declare (ignorable echo beep xoff))
  (buffer-show terminal :set-input t)
  (prog1 (call-next-method)
    (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                     (previous-line 1)))
    (buffer-update-from-slime terminal :update-column t)
    ;; (with-slots (current-column buffer) terminal
    ;;   (eval-in-emacs
    ;;    `(with-current-buffer (slime-repl-buffer)
    ;;       (forward-line -1)
    ;;       (end-of-line)
    ;;       (delete-region (point) (point-max))
    ;;       (slime-move-point (point))
    ;;       (prog1 (- (point)
    ;;                 (progn (beginning-of-line) (point)))
    ;;         (end-of-line))))
    ;;   (let* ((size       (length input))
    ;;          (new-column (+ current-column size)))
    ;;     (when (plusp size)
    ;;       (when (< (array-dimension buffer 0) new-column)
    ;;         (setf buffer (adjust-array buffer (max (* 2 (array-dimension buffer 0))
    ;;                                                new-column)
    ;;                                    :element-type (array-element-type buffer))))
    ;;       (setf (fill-pointer buffer) (max (fill-pointer buffer) new-column))
    ;;       (replace buffer input :start1 current-column)
    ;;       (setf current-column new-column))))
    ))

(defmethod terminal-read ((terminal swank-terminal) &key (echo t) (beep nil) (xoff nil))
  (declare (ignorable echo beep xoff))
  (buffer-show terminal :set-input t)
  (progn (format *trace-output* "Before read~%") (force-output *trace-output*))
  (prog1 (call-next-method)
    (progn (format *trace-output* "After read~%") (force-output *trace-output*))
    (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                     (previous-line 1)))
    (buffer-update-from-slime terminal :update-column t)
    (progn (format *trace-output* "Done~%") (force-output *trace-output*))

    ;; (setf (slot-value terminal 'current-column)
    ;;       (eval-in-emacs
    ;;        `(with-current-buffer (slime-repl-buffer)
    ;;           (forward-line -1)
    ;;           (end-of-line)
    ;;           (delete-region (point) (point-max))
    ;;           (slime-move-point (point))
    ;;           (prog1 (- (point)
    ;;                     (progn (beginning-of-line) (point)))
    ;;             (end-of-line)))))
    ))




;;;; THE END ;;;;
