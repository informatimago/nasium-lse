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


(in-package "COM.INFORMATIMAGO.LSE.CLI")


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
;; integration. swank::eval-in-emacs processes the CL form to make it
;; "emacs" (eg. downcase symbols, etc).  It could convert CLOS proxies
;; to emacs lisp forms returning the corresponding emacs object.



(defvar *debug-swank* nil)
;; (defvar *debug-swank* #+debugging t #-debugging nil)
;; (setf  *debug-swank* t)
;; (setf  *debug-swank* nil)

(defun eval-in-emacs (form &optional nowait)
  #-swank (declare (ignore form nowait))
  #-swank nil
  #+swank
  (when *debug-swank*
    (let  ((*print-case*         :downcase)
           (*print-escape*       nil)
           (*print-right-margin* 100)
           (*package*            (find-package "COM.INFORMATIMAGO.LSE.CLI")))
      (pprint form *trace-output*)
      (terpri *trace-output*)))
  #+swank
  (let* ((*package*   (load-time-value (find-package "COM.INFORMATIMAGO.LSE.EMACS")))
         (result      (swank::eval-in-emacs `(let ((print-length nil)
                                                   (print-circle t)
                                                   (print-level nil))
                                               (format "%S" ,form))
                                            nowait))
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
   (output-buffer  :initform (make-array 80
                                         :element-type 'character
                                         :adjustable t
                                         :fill-pointer 0))
   (current-column :initform 0)
   (input-buffer   :initform (make-array 80
                                         :element-type 'character
                                         :adjustable t
                                         :fill-pointer 0))
   (input-cursor   :initform 0
                   :type (integer 0)
                   :documentation "Position of the first character remaining to read from the in the input-buffer.")
   (input-finished :initform nil
                   :type   boolean)))


(defun send-swank-terminal-function-to-emacs ()
  "Sends to emacs a set of functions that will be called by the various methods of swank-terminal."
  (eval-in-emacs
   '(progn

     (defun lse-swank-terminal--send-xoff ()
       (interactive)
       (insert 10)
       (slime-repl-return :end-of-input))
     
     (defun lse-swank-terminal--initialize ()
       (with-current-buffer (slime-repl-buffer)
         (font-lock-mode -1)
         (overwrite-mode +1)
         (local-set-key (kbd "C-s") 'lse-swank-terminal--send-xoff)
         nil))

     (defun lse-swank-terminal--finalize ()
       (with-current-buffer (slime-repl-buffer)
         (font-lock-mode +1)
         (overwrite-mode -1)
         (local-unset-key (kbd "C-s"))
         nil))

     (defun lse-swank-terminal--columns ()
       (with-current-buffer (slime-repl-buffer)
         (let ((windows (remove* (slime-repl-buffer) (window-list)
                                 :test-not 'eql
                                 :key 'window-buffer)))
           (and windows (window-width (first windows))))))     

     (defun lse-swank-terminal--height ()
       (with-current-buffer (slime-repl-buffer)
         (let ((windows (remove* (slime-repl-buffer) (window-list)
                                 :test-not 'eql
                                 :key 'window-buffer)))
           (and windows (window-height (first windows))))))

     (defun lse-swank-terminal--carriage-return ()
       (with-current-buffer (slime-repl-buffer)
         (beginning-of-line)
         (slime-move-point (point))))

     (defun lse-swank-terminal--move-to-column (current-column)
       (with-current-buffer (slime-repl-buffer)
         (beginning-of-line)
         (let ((start (point))
               (end   (point-max)))
           (when (< (- end start) current-column)
             (goto-char end)
             (insert (make-string (- current-column (- end start)) 32))
             (beginning-of-line))
           (forward-char current-column)
           (slime-move-point (point)))))

     (defun lse-swank-terminal--show (output-buffer current-column set-input)
       (with-current-buffer (slime-repl-buffer)
         (delete-region
          (progn (beginning-of-line) (point))
          (progn (end-of-line)       (point)))
         (insert output-buffer)
         (lse-swank-terminal--move-to-column  current-column)
         (when set-input
           (delete-region (point) (point-max))
           (slime-mark-input-start))))

     (defun lse-swank-terminal--flush ()
       (with-current-buffer (slime-repl-buffer)
         (delete-region (progn (beginning-of-line) (point))
                        (point-max))
         (slime-move-point (point-max))))     
     
     (defun lse-swank-terminal--get-current-line ()
       (with-current-buffer (slime-repl-buffer)
         (buffer-substring-no-properties
          (progn (beginning-of-line) (point))
          (progn (end-of-line)       (point)))))

     (defun lse-swank-terminal--end-of-line ()
       (with-current-buffer (slime-repl-buffer)
         (end-of-line)))

     (defun lse-swank-terminal--backward-delete-char ()
       (with-current-buffer (slime-repl-buffer)
         (delete-region (1- (point)) (point))))

     (mapc 'byte-compile '(lse-swank-terminal--initialize
                           lse-swank-terminal--finalize
                           lse-swank-terminal--columns
                           lse-swank-terminal--height
                           lse-swank-terminal--carriage-return
                           lse-swank-terminal--move-to-column
                           lse-swank-terminal--show
                           lse-swank-terminal--flush
                           lse-swank-terminal--get-current-line
                           lse-swank-terminal--end-of-line
                           lse-swank-terminal--backward-delete-char)))))



(defmethod terminal-initialize ((terminal swank-terminal))
  (send-swank-terminal-function-to-emacs)
  (eval-in-emacs '(lse-swank-terminal--initialize))
  (list (terminal-columns terminal)
        (terminal-rows    terminal)))

(defmethod terminal-finalize ((terminal swank-terminal))
  (eval-in-emacs '(lse-swank-terminal--finalize))
  (values))

(defmethod terminal-columns ((terminal swank-terminal))
  (declare (ignorable terminal))
  (let ((new-columns (eval-in-emacs '(lse-swank-terminal--columns))))
    (if new-columns
        (setf (slot-value terminal 'last-columns) new-columns)
        (slot-value terminal 'last-columns))))

(defmethod terminal-rows ((terminal swank-terminal))
  (declare (ignorable terminal))
  (let ((new-rows (eval-in-emacs '(lse-swank-terminal--height))))
    (if new-rows
        (setf (slot-value terminal 'last-rows) new-rows)
        (slot-value terminal 'last-rows))))

(defmethod terminal-keysym-label ((terminal swank-terminal) keysym)
  (declare (ignorable terminal))
  (ecase keysym
    (:escape    (values "[CONTRÔLE-C] [CONTRÔLE-C]" t))
    (:attention (values "(PAS DISPONIBLE)" nil))
    (:xoff      (values "[ENTRÉE]" t))
    (:delete    (values "[EFFACEMENT]" t))
    (:return    (values "[ENTRÉE]" t))))



;;; terminal output:
;;; --------------------

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


(defmethod output-buffer-carriage-return ((terminal swank-terminal))
  (eval-in-emacs '(lse-swank-terminal--carriage-return))
  (with-slots (current-column) terminal
    (setf current-column 0))
  (values))


(defmethod output-buffer-move-to-column ((terminal swank-terminal))
  (with-slots (current-column) terminal
    (eval-in-emacs `(lse-swank-terminal--move-to-column ,current-column))))


(defmethod output-buffer-show ((terminal swank-terminal) &key set-input)
  "Replace the last line ('unflushed') in the slime output-buffer with the
contents of the OUTPUT-BUFFER, moving the cursor to the CURRENT-COLUMN."
  (with-slots (output-buffer current-column) terminal
    (eval-in-emacs `(lse-swank-terminal--show ,output-buffer ,current-column ,set-input))))


(defmethod output-buffer-flush ((terminal swank-terminal))
  "Flush the terminal, ie. outputs the OUTPUT-BUFFER line definitively."
  (eval-in-emacs '(lse-swank-terminal--flush))
  (with-slots (current-column output-buffer) terminal
    (let ((output (terminal-output-stream terminal)))
      (princ output-buffer output))
    (output-buffer-move-to-column terminal))
  (values))



(defmethod output-buffer-update-from-slime ((terminal swank-terminal) &key (update-column nil))
  (let ((new-line (eval-in-emacs '(lse-swank-terminal--get-current-line))))
    (with-slots (current-column output-buffer) terminal
      (when update-column
        (setf current-column (length new-line)))
      (output-buffer-move-to-column terminal)
      (let ((new-length (max (length new-line) current-column)))
        (setf output-buffer (adjust-buffer output-buffer new-length))
        (replace output-buffer new-line)
        (fill output-buffer #\space :start (length new-line) :end new-length))))
  (values))


(defmethod terminal-carriage-return ((terminal swank-terminal))
  ;; (eval-in-emacs '(message "carriage return"))
  (let ((output (terminal-output-stream terminal)))
    (finish-output output))
  (output-buffer-carriage-return terminal))


(defmethod terminal-new-line ((terminal swank-terminal) &optional (count 1))
  ;; (eval-in-emacs '(message "new line"))
  (output-buffer-flush terminal)
  (eval-in-emacs '(lse-swank-terminal--end-of-line))
  (with-slots (current-column output-buffer) terminal
    (loop
      :with output = (terminal-output-stream terminal)
      :repeat count :do (terpri output))
    (terminal-finish-output terminal)
    (setf current-column 0
          (fill-pointer output-buffer) 0))
  (output-buffer-move-to-column terminal)
  (values))


(defmethod terminal-line-feed ((terminal swank-terminal) &optional (count 1))
  ;; (eval-in-emacs '(message "line feed"))
  (output-buffer-flush terminal)
  (eval-in-emacs '(lse-swank-terminal--end-of-line))
  (with-slots (current-column output-buffer) terminal
    (let ((output (terminal-output-stream terminal)))
      (loop :repeat count :do (terpri output))
      (terminal-finish-output terminal))
    (setf (fill-pointer output-buffer) current-column)
    (fill output-buffer #\space))
  (output-buffer-show terminal)
  (values))


(defmethod terminal-write-string ((terminal swank-terminal) string &key (start 0) (end nil))
  ;; Since slime uses insert to insert new text, it doesn't override
  ;; previous text.  Therefore we do it ourselves in a output-buffer, and
  ;; replace the whole line.
  (with-slots (last-columns current-column output-buffer) terminal
    (let* ((end        (or end (length string)))
           (size       (- end start))
           (new-column (+ current-column size)))
      (when (plusp size)
        (when (< (array-dimension output-buffer 0) new-column)
          (setf output-buffer (adjust-buffer output-buffer new-column)))
        (setf (fill-pointer output-buffer) (max (fill-pointer output-buffer) new-column))
        (replace output-buffer string :start1 current-column :start2 start :end2 end)
        (setf current-column new-column)
        (output-buffer-show terminal))))
  (values))




;;; terminal input functions:
;;; --------------------------

(defmethod terminal-read-string ((terminal swank-terminal))
  (terminal-finish-output terminal)
  (output-buffer-show terminal :set-input t)
  (let ((line (read-line (terminal-input-stream terminal) nil nil)))
    (eval-in-emacs '(lse-swank-terminal--backward-delete-char))
    (output-buffer-update-from-slime terminal :update-column t)
    line))



(defmethod terminal-fill-input-buffer ((terminal swank-terminal))
  (with-slots (input-buffer input-cursor input-finished ) terminal
    (let* ((input    (terminal-read-string terminal))
           (start    (fill-pointer input-buffer))
           (new-size (+ start (length input))))
      (setf input-buffer (adjust-buffer input-buffer (length input))
            (fill-pointer input-buffer) new-size)
      (replace input-buffer input :start1 start)
      (setf input-finished t))))



(defmethod terminal-get-next-char ((terminal swank-terminal))
  (with-slots (input-buffer
               input-cursor
               input-finished) terminal
    (let ((ch (loop
                :named get-a-byte
                ;; :do     (terminal-write-string terminal (format nil "~S~%" (list :buffer input-buffer :cursor input-cursor :finished input-finished :end-of-file input-end-of-file :fill-buffer (fill-pointer input-buffer) :length (length input-buffer)  :stream input-stream)))
                :do (cond
                      ((< input-cursor (length input-buffer))
                       (return-from get-a-byte (prog1 (aref input-buffer input-cursor)
                                                 (incf input-cursor))))
                      (input-finished
                       (setf input-finished nil
                             input-cursor 0
                             (fill-pointer input-buffer) 0)
                       (return-from get-a-byte #\Newline))
                      (t
                       (terminal-fill-input-buffer terminal))))))
      #+lse-input-debug (terminal-write-string
                         terminal
                         (format nil "~%~S input-cursor=~S input-buffer=~S input-finished=~S~%"
                                 'terminal-get-next-char
                                 input-cursor input-buffer input-finished))
      ch)))


(defmethod terminal-skip-characters ((terminal swank-terminal) characters)
  (loop
    :named reading
    :for ch = (terminal-read-buffered-character terminal)
    :while (find ch characters)
    :finally (let ((ch (terminal-keysym-character terminal ch)))
               (with-slots (input-buffer input-cursor) terminal
                 #+lse-input-debug (terminal-write-string
                                    terminal
                                    (format nil "~%~S input-cursor=~S input-buffer=~S~%"
                                            'terminal-skip-characters
                                            input-cursor input-buffer))
                 (cond
                   ((plusp input-cursor)
                    (decf input-cursor)
                    (setf (aref input-buffer input-cursor) ch))
                   ((zerop (fill-pointer input-buffer))
                    (vector-push ch input-buffer))
                   ((< (fill-pointer input-buffer) (array-dimension input-buffer 0))
                    (incf (fill-pointer input-buffer))
                    (replace input-buffer input-buffer :start1 0 :start2 1)
                    (setf (aref input-buffer 0) ch))
                   (t ;; should not occur since we've just called terminal-read-buffered-character
                    (lse-error "ERREUR INTERNE: LIMITE DE TAILLE DE TAMPON D'ENTRÉE ATTEINTE")))))))

;;;; THE END ;;;;
