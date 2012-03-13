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
   (output-buffer  :initform (make-array 80
                                         :element-type 'character
                                         :adjustable t
                                         :fill-pointer 0))
   (current-column :initform 0)
   (input-buffer   :initform "")
   (input-read     :initform 0)
   (input-finished :initform nil)))

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


(defun adjust-output-buffer (output-buffer new-length)
  (if (< (array-dimension output-buffer 0) new-length)
      (adjust-array output-buffer (max (* 2 (array-dimension output-buffer 0))
                                new-length)
                    :element-type (array-element-type output-buffer)
                    :fill-pointer new-length)
      (progn
        (setf (fill-pointer output-buffer) new-length)
        output-buffer)))

(defmethod output-buffer-carriage-return ((terminal swank-terminal))
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

(defmethod output-buffer-move-to-column ((terminal swank-terminal))
  (with-slots (current-column output-buffer) terminal
   (eval-in-emacs
    `(with-current-buffer (slime-repl-buffer)
       ;; (message "column = %3d output-buffer = %S" ,current-column ,output-buffer)
       ,(gen-move-to-column current-column)))))

(defmethod output-buffer-show ((terminal swank-terminal) &key set-input)
  "Replace the last line ('unflushed') in the slime output-buffer with the
contents of the OUTPUT-BUFFER, moving the cursor to the CURRENT-COLUMN."
  (with-slots (current-column output-buffer) terminal
    (eval-in-emacs
     `(with-current-buffer (slime-repl-buffer)
        ;; (message "show output-buffer =         %S" ,output-buffer)
        (delete-region
         (progn (beginning-of-line) (point))
         (progn (end-of-line)       (point)))
        (insert ,output-buffer)
        ,(gen-move-to-column current-column)
        ,@ (when set-input
               '((delete-region (point) (point-max))
                 (slime-mark-input-start)))))))

(defmethod output-buffer-flush ((terminal swank-terminal))
  "Flush the terminal, ie. outputs the OUTPUT-BUFFER line definitively."
  (eval-in-emacs
   `(with-current-buffer (slime-repl-buffer)
      (delete-region (progn (beginning-of-line) (point))
                     (point-max))
      (slime-move-point (point-max))))
  (with-slots (current-column output-buffer) terminal
    (let ((output (terminal-output-stream terminal)))
      (princ output-buffer output))
    (output-buffer-move-to-column terminal))
  (values))

(defmethod output-buffer-update-from-slime ((terminal swank-terminal) &key (update-column nil))
  (let ((new-line (eval-in-emacs
                   '(with-current-buffer (slime-repl-buffer)
                     (buffer-substring-no-properties
                      (progn (beginning-of-line) (point))
                      (progn (end-of-line)       (point)))))))
    ;; (eval-in-emacs `(message "           new-line = %S" ,new-line))
    (with-slots (current-column output-buffer) terminal
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
        (setf output-buffer (adjust-output-buffer output-buffer new-length))
        (replace output-buffer new-line)
        (fill output-buffer #\space :start (length new-line) :end new-length)
        ;; (eval-in-emacs `(message "column = %3d output-buffer = %S" ,current-column ,output-buffer))
        )))
  (values))

        ;; (setf *b* (COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY:COPY-ARRAY output-buffer
        ;;            :COPY-FILL-POINTER t :COPY-ADJUSTABLE t))


(defmethod terminal-carriage-return ((terminal swank-terminal))
  ;; (eval-in-emacs '(message "carriage return"))
  (let ((output (terminal-output-stream terminal)))
    (finish-output output))
  (output-buffer-carriage-return terminal))

;; (defmethod terminal-move-up ((terminal swank-terminal))
;;   (eval-in-emacs '(message "move up"))
;;   (let ((output (terminal-output-stream terminal)))
;;     (finish-output output))
;;   (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
;;                    (previous-line 1)))
;;   (output-buffer-update-from-slime terminal :update-column nil)
;;   (values))

(defmethod terminal-new-line ((terminal swank-terminal) &optional (count 1))
  ;; (eval-in-emacs '(message "new line"))
  (output-buffer-flush terminal)
  (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                   (end-of-line)))
  (with-slots (current-column output-buffer) terminal
    (let ((output (terminal-output-stream terminal)))
      (loop :repeat count :do (terpri output))
      (terminal-finish-output terminal)
      (setf current-column 0
            (fill-pointer output-buffer) 0)))
  (output-buffer-move-to-column terminal)
  (values))

(defmethod terminal-line-feed ((terminal swank-terminal) &optional (count 1))
  ;; (eval-in-emacs '(message "line feed"))
  (output-buffer-flush terminal)
  (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                   (end-of-line)))
  (with-slots (current-column output-buffer) terminal
    (let ((output (terminal-output-stream terminal)))
      (loop :repeat count :do (terpri output))
      (terminal-finish-output terminal))
    (setf (fill-pointer output-buffer) current-column)
    (fill output-buffer #\space))
  (output-buffer-show terminal)
  (output-buffer-move-to-column terminal)
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
          (setf output-buffer (adjust-output-buffer output-buffer new-column)))
        (setf (fill-pointer output-buffer) (max (fill-pointer output-buffer) new-column))
        (replace output-buffer string :start1 current-column :start2 start :end2 end)
        (setf current-column new-column)
        (output-buffer-show terminal))))
  (values))


(defmethod terminal-read-line ((terminal swank-terminal) &key (echo t) (beep nil))
  (declare (ignorable echo))
  (when beep
    (terminal-ring-bell terminal))
  (terminal-finish-output terminal)
  (output-buffer-show terminal :set-input t)
  (prog1 (call-next-method)
    (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
                     (delete-region (1- (point)) (point))))
    (output-buffer-update-from-slime terminal :update-column t)
    ;; (with-slots (current-column output-buffer) terminal
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
    ;;       (when (< (array-dimension output-buffer 0) new-column)
    ;;         (setf output-buffer (adjust-array output-buffer (max (* 2 (array-dimension output-buffer 0))
    ;;                                                new-column)
    ;;                                    :element-type (array-element-type output-buffer))))
    ;;       (setf (fill-pointer output-buffer) (max (fill-pointer output-buffer) new-column))
    ;;       (replace output-buffer input :start1 current-column)
    ;;       (setf current-column new-column))))
    ))

(defmethod terminal-read ((terminal swank-terminal) &key (echo t) (beep nil))

  (with-slots (input-buffer input-finished input-read) terminal
    (flet ((finish ()
             (handler-case
                 (destructuring-bind (donnee position) (parse-donnee-lse buffer :start input-read)
                   (if (< position (length buffer))
                       (setf input-read position)
                       (setf input-read 0
                             (fill-pointer buffer) 0
                             input-finished nil))
                   donnee)
               (error ()
                 (let ((donnee (subseq buffer input-read)))
                   (setf input-read 0
                         (fill-pointer buffer) 0
                         input-finished nil)
                   (lse-error "DONNEE INVALIDE ~S, ATTENDU UN NOMBRE" donnee))))))
      (if input-finished
          (finish)
          (loop
            (read-one-char terminal)
            (when input-finished
              (return (finish)))))))
  
  ;; (declare (ignorable echo))
  ;; (when beep
  ;;   (terminal-ring-bell terminal))
  ;; (terminal-finish-output terminal)
  ;; (output-buffer-show terminal :set-input t)
  ;; (prog1 (call-next-method)
  ;;   (eval-in-emacs '(with-current-buffer (slime-repl-buffer)
  ;;                    (delete-region (1- (point)) (point))))
  ;;   (output-buffer-update-from-slime terminal :update-column t)
  ;; 
  ;;   ;; (setf (slot-value terminal 'current-column)
  ;;   ;;       (eval-in-emacs
  ;;   ;;        `(with-current-buffer (slime-repl-buffer)
  ;;   ;;           (forward-line -1)
  ;;   ;;           (end-of-line)
  ;;   ;;           (delete-region (point) (point-max))
  ;;   ;;           (slime-move-point (point))
  ;;   ;;           (prog1 (- (point)
  ;;   ;;                     (progn (beginning-of-line) (point)))
  ;;   ;;             (end-of-line)))))
  ;;   )
  )



(defmethod terminal-key ((terminal swank-terminal) keysym)
  (declare (ignorable terminal))
  (ecase keysym
    (:escape    "[CONTRÔLE-C] [CONTRÔLE-C]")
    (:attention "(PAS DISPONIBLE)")
    (:xoff      "[ENTRÉE]")
    (:delete    "[EFFACEMENT]")
    (:return    "[ENTRÉE]")))


;;;; THE END ;;;;
