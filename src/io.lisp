;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               io.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    EMULSE : L.S.E. [ EMULATION MITRA-15 ]
;;;;    
;;;;    An emultator of the CII MITRA-15 L.S.E. System 
;;;;    and programming language interpreter.
;;;;    
;;;;    This is the L.S.E. I/O module.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-08-02 <PJB> Converted to Common-Lisp.
;;;;    2000-12-09 <PJB> Added this header comment.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2000 - 2014
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
;;;;****************************************************************************
(in-package "COM.INFORMATIMAGO.LSE")


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defconstant +TAPE-READER-ON+   DC1)
  (defconstant +TAPE-PUNCHER-ON+  DC2)
  (defconstant +TAPE-PUNCHER-OFF+ DC4)
  (defconstant +TAPE-READER-OFF+  NAK)

  );;eval-when


(defun slog (ctrlstring &rest args)
  (with-open-file (logstream "/tmp/log.txt"
                             :direction :output
                             :if-does-not-exist :create
                             :if-exists :append)
    (format logstream "~?" ctrlstring args)))


;;----------------------------------------------------------------------
;;; Tape I/O
;;----------------------------------------------------------------------


(defmethod io-stop-tape-reader ((task t))
  (setf (task-input  task) (terminal-input-stream (task-terminal task))))

(defmethod io-stop-tape-puncher ((task t))
  (setf (task-output task) (terminal-output-stream (task-terminal task))))


(defmethod io-start-tape-reader ((task t))
  (setf (task-input  task) (task-tape-input task)))

(defmethod io-start-tape-puncher ((task t))
  (setf (task-output task) (task-tape-output task)))


(defmethod io-standard-redirection ((task t))
  (io-stop-tape-puncher task)
  (io-stop-tape-reader  task)
  (setf (task-silence task) nil)
  (values))


;;----------------------------------------------------------------------

(defmethod io-terminal-output-p ((task t))
  (eql (task-output task) (terminal-output-stream (task-terminal task))))

(defmethod io-terminal-input-p  ((task t))
  (eql (task-input task) (terminal-input-stream (task-terminal task))))

(defmethod io-tape-output-p ((task t))
  (eql (task-output task) (task-tape-output task)))

(defmethod io-tape-input-p  ((task t))
  (eql (task-input task) (task-tape-input task)))


;;----------------------------------------------------------------------

;; attributes
;; --------------------

(defmethod io-echo ((task t))
  (terminal-echo (task-terminal task)))

(defmethod (setf io-echo) (new-echo (task t))
  (setf (terminal-echo (task-terminal task)) new-echo))

;; output
;; --------------------

(defmethod io-bell ((task t))
  (when (task-allow-bell-output task)
    (terminal-ring-bell (task-terminal task))))

(defmethod io-carriage-return ((task t))
  (if (io-terminal-output-p task)
      (terminal-carriage-return (task-terminal task))
      (princ #\Return (task-output task))))


(defvar *page-height* nil "For the pager: number of lines in screen.")
(defvar *line-count*  nil "For the pager: number of lines written so far.")

(defun call-with-pager (task thunk)
  (let ((paging (task-paging task)))
    (let ((*page-height* (case paging
                           ((nil)     nil)
                           ((t)       (terminal-rows (task-terminal task)))
                           (otherwise paging)))
          (*line-count*  (when paging 0)))
      (funcall thunk))))

(defmacro with-pager (task &body body)
  `(call-with-pager ,task (lambda () ,@body)))

(defun pager-new-line (task &optional (count 1))
  (when (and *page-height* *line-count*)
    (incf *line-count* count)
    (unless (< (1+ *line-count*) *page-height*)
      (multiple-value-bind (label available) (terminal-keysym-label (task-terminal task) :xoff)
        (unless available
          (multiple-value-setq (label available) (terminal-keysym-label (task-terminal task) :return)))
        (let ((*page-height* nil)
              (*line-count*  nil))
          (io-new-line task)
          (with-open-file (out #P "~/Desktop/out.txt" :direction :output
                               :if-exists :append :if-does-not-exist :create)
            (format out "Pager ~S~%" (multiple-value-list (decode-universal-time (get-universal-time)))))
          (io-format task "Taper ~A pour continuer:" label)
          (io-read-line task :beep t :echo nil)
          (io-carriage-return task)))
      (setf *line-count* 0))))

(defmethod io-line-feed ((task t) &optional (count 1))
  (if (io-terminal-output-p task)
      (progn
        (pager-new-line task count)
        (terminal-line-feed (task-terminal task) count))
      (format (task-output task) "~V,,,VA" count LF "")))

(defmethod io-new-line ((task t) &optional (count 1))
  (if (io-terminal-output-p task)
      (progn
        (pager-new-line task count)
        (terminal-new-line (task-terminal task) count))
      (terpri (task-output task))))

(defmethod io-finish-output ((task t))
  (if (io-terminal-output-p task)
      (terminal-finish-output (task-terminal task))
      (finish-output (task-output task))))


;;; Some more output:

(defparameter *dectech-leftwards-arrow* (or (ignore-errors (code-char #xfb)) #\_))
(defparameter *dectech-upwards-arrow*   (or (ignore-errors (code-char #xfc)) #\^))

;; (Some implementations have unicode but don't know #\upwards_arrow, etc.)

;; LEFTWARDS_ARROW and UPWARDS_ARROW
(defparameter *unicode-leftwards-arrow* (or (ignore-errors (code-char 8592)) #\_))
(defparameter *unicode-upwards-arrow*   (or (ignore-errors (code-char 8593)) #\^))

;; HALFWIDTH_LEFTWARDS_ARROW and HALFWIDTH_UPWARDS_ARROW
(defparameter *unicode-halfwidth-leftwards-arrow* (or (ignore-errors (code-char 65513)) #\_))
(defparameter *unicode-halfwidth-upwards-arrow*   (or (ignore-errors (code-char 65514)) #\^))


(defun output-substitute (upcase accented arrows string)
  "
RETURN: A string transformed according to the flags UPCASE and
        ACCENTED, and the choice of ARROWS.
"
  (if upcase
      (if accented
          (case arrows
            (:dectech           (with-output-to-string (out)
                                  (loop
                                    :for ch :across string
                                    :do (princ (case ch
                                                 ((#\_) *dectech-leftwards-arrow*)
                                                 ((#\^) *dectech-upwards-arrow*)
                                                 (otherwise
                                                  (if (lower-case-p ch)
                                                      (char-upcase ch)
                                                      ch)))
                                               out))))
            (:unicode           (with-output-to-string (out)
                                  (loop
                                    :for ch :across string
                                    :do (princ (case ch
                                                 ((#\_) *unicode-leftwards-arrow*)
                                                 ((#\^) *unicode-upwards-arrow*)
                                                 (otherwise
                                                  (if (lower-case-p ch)
                                                      (char-upcase ch)
                                                      ch)))
                                               out))))
            (:unicode-halfwidth (with-output-to-string (out)
                                  (loop
                                    :for ch :across string
                                    :do (princ (case ch
                                                 ((#\_) *unicode-halfwidth-leftwards-arrow*)
                                                 ((#\^) *unicode-halfwidth-upwards-arrow*)
                                                 (otherwise
                                                  (if (lower-case-p ch)
                                                      (char-upcase ch)
                                                      ch)))
                                               out))))
            (otherwise          (with-output-to-string (out)
                                  (loop
                                    :for ch :across string
                                    :do (princ (if (lower-case-p ch)
                                                   (char-upcase ch)
                                                   ch)
                                               out)))))
          (case arrows
            (:dectech           (with-output-to-string (out)
                                  (loop
                                    :for ch :across string
                                    :do (princ (case ch
                                                 ((#\_) *dectech-leftwards-arrow*)
                                                 ((#\^) *dectech-upwards-arrow*)
                                                 (otherwise
                                                  (character-fold (if (lower-case-p ch)
                                                                      (char-upcase ch)
                                                                      ch))))
                                               out))))
            (:unicode           (with-output-to-string (out)
                                  (loop
                                    :for ch :across string
                                    :do (princ (case ch
                                                 ((#\_) *unicode-leftwards-arrow*)
                                                 ((#\^) *unicode-upwards-arrow*)
                                                 (otherwise
                                                  (character-fold (if (lower-case-p ch)
                                                                      (char-upcase ch)
                                                                      ch))))
                                               out))))
            (:unicode-halfwidth (with-output-to-string (out)
                                  (loop
                                    :for ch :across string
                                    :do (princ (case ch
                                                 ((#\_) *unicode-halfwidth-leftwards-arrow*)
                                                 ((#\^) *unicode-halfwidth-upwards-arrow*)
                                                 (otherwise
                                                  (character-fold (if (lower-case-p ch)
                                                                      (char-upcase ch)
                                                                      ch))))
                                               out))))
            (otherwise          (with-output-to-string (out)
                                  (loop
                                    :for ch :across string
                                    :do (princ (character-fold (if (lower-case-p ch)
                                                                   (char-upcase ch)
                                                                   ch))
                                               out))))))
      (if accented
          (case arrows
            (:dectech           (with-output-to-string (out)
                                  (loop
                                    :for ch :across string
                                    :do (princ (case ch
                                                 ((#\_) *dectech-leftwards-arrow*)
                                                 ((#\^) *dectech-upwards-arrow*)
                                                 (otherwise ch))
                                               out))))
            (:unicode           (with-output-to-string (out)
                                  (loop
                                    :for ch :across string
                                    :do (princ (case ch
                                                 ((#\_) *unicode-leftwards-arrow*)
                                                 ((#\^) *unicode-upwards-arrow*)
                                                 (otherwise ch))
                                               out))))
            (:unicode-halfwidth (with-output-to-string (out)
                                  (loop
                                    :for ch :across string
                                    :do (princ (case ch
                                                 ((#\_) *unicode-halfwidth-leftwards-arrow*)
                                                 ((#\^) *unicode-halfwidth-upwards-arrow*)
                                                 (otherwise ch))
                                               out))))
            (otherwise           string))
          (case arrows
            (:dectech           (with-output-to-string (out)
                                  (loop
                                    :for ch :across string
                                    :do (princ (case ch
                                                 ((#\_) *dectech-leftwards-arrow*)
                                                 ((#\^) *dectech-upwards-arrow*)
                                                 (otherwise (character-fold ch)))
                                               out))))
            (:unicode           (with-output-to-string (out)
                                  (loop
                                    :for ch :across string
                                    :do (princ (case ch
                                                 ((#\_) *unicode-leftwards-arrow*)
                                                 ((#\^) *unicode-upwards-arrow*)
                                                 (otherwise (character-fold ch)))
                                               out))))
            (:unicode-halfwidth (with-output-to-string (out)
                                  (loop
                                    :for ch :across string
                                    :do (princ (case ch
                                                 ((#\_) *unicode-halfwidth-leftwards-arrow*)
                                                 ((#\^) *unicode-halfwidth-upwards-arrow*)
                                                 (otherwise (character-fold ch)))
                                               out))))
            (otherwise          (remove-accents string))))))


(defmethod io-substitute ((task t) string)
  (if (io-terminal-output-p task)
      (output-substitute (task-upcase-output task)
                         (task-accented-output task)
                         (task-arrows task)
                         string)
      string))


(defparameter *io-active-codes* (vector +TAPE-READER-ON+   +TAPE-PUNCHER-ON+
                                        +TAPE-PUNCHER-OFF+ +TAPE-READER-OFF+
                                        XOFF CR LF))


(defmethod io-format ((task t) control-string &rest arguments)
  (let* ((buffer (io-substitute task (apply (function format) nil control-string arguments)))
         (start  0)
         (end    (length buffer)))
    (loop
      :while (< start end)
      :do (let ((chunk-end (or (position-if (lambda (ch) (find (char-code ch) *io-active-codes*))
                                            buffer :start start)
                               end)))
            (if (io-terminal-output-p task)
                (terminal-write-string (task-terminal task) buffer :start start :end chunk-end)
                (write-string          buffer (task-output   task) :start start :end chunk-end))
            (when (< chunk-end end)
              (ecase (char-code (aref buffer chunk-end))
                ((#.CR)                 (io-carriage-return    task))
                ((#.LF #.XOFF)          (io-new-line           task)) ; not line-feed.
                ((#.+TAPE-READER-ON+)   (io-start-tape-reader  task))
                ((#.+TAPE-PUNCHER-ON+)  (io-start-tape-puncher task))
                ((#.+TAPE-PUNCHER-OFF+) (io-stop-tape-puncher  task))
                ((#.+TAPE-READER-OFF+)  (io-stop-tape-reader   task)))
              (incf chunk-end))
            (setf start chunk-end)))))




;; input
;; --------------------


(defun push-chaine-buffer (ch buffer)
  (if (<= (1+ (length buffer)) chaine-maximum)
      (vector-push-extend ch buffer (min (- chaine-maximum (length buffer)) (length buffer)))
      (lse-error "CHAINE TROP GRANDE.")))

(defun push-nombre-buffer (ch buffer)
  (let ((bufmax 80))
    (if (<= (1+ (length buffer)) bufmax)
        (vector-push-extend ch buffer (min (- bufmax (length buffer)) (length buffer)))
        (lse-error "SAISIE POUR NOMBRE TROP GRANDE."))))

(declaim (inline push-chaine-buffer push-nombre-buffer))


(defgeneric io-read-buffered-character (task)
    (:documentation "
Return a character or a keyword representing a key, read from a
possibly buffered source.  The possible keywords are: :xoff :delete
:return

When this function receives the escape character (ESC), it signals a
USER-INTERRUPT condition with SIGINT+ as USER-INTERRUPT-SIGNAL.

When it receives the attention character (C-a), it signals a
USER-INTERRUPT with +SIGQUIT+ as USER-INTERRUPT-SIGNAL.

Those user-interrupt can be implemented by the kernel terminal driver,
instead of methods of this functions for some implementations.

Upon end-of-file, NIL is returned.
")
  (:method ((task task))
    (let* ((terminal (task-terminal task))
           (input    (if (io-terminal-input-p task)
                         terminal
                         (task-input task)))
           (keysym   (terminal-read-buffered-character input)))
      (case keysym
        ((:escape)
         (signal 'user-interrupt :signal +sigint+)
         ;; Should not occur, but in case:
         (io-read-buffered-character task))
        ((:attention)
         (signal 'user-interrupt :signal +sigquit+)
         ;; Should not occur, but in case:
         (io-read-buffered-character task))
        ((#\Return)
         (if (task-x))
         )
        (otherwise
         keysym)))))


(defmethod io-read-string ((task t) &key (echo t) (beep t))
  "
DO:         Reads a string.

read characters until RET or C-s or ESC or string full.
ESC is interrupt.
string full is an error condition, 
RET is included in the string, C-s not.  (It's virtual RET/C-s, real RET can be mapped to C-s).

ECHO: Whether this input must be done echoing the characters (default T).
BEEP: Whether the terminal should beep before reading the string (default NIL).

RETURN: The LSE string read.
"
  (let ((terminal (task-terminal task)))
    (with-temporary-echo (terminal echo)
      (when (and beep (task-allow-bell-output task)) (terminal-ring-bell terminal))
      (loop
        :named reading
        :with buffer = (make-array 8 :adjustable t :fill-pointer 0 :element-type 'character)
        :for ch = (io-read-buffered-character task)
        :do #+lse-input-debug (io-format task "~%io-read-buffered-character -> ~A .~A.~%" ch (when (characterp ch) (char-code ch)))
            (case ch
              ((:xoff)
               (return-from reading buffer))
              ((:return)
               (push-chaine-buffer #\Return buffer)
               (return-from reading buffer))
              ((:delete)
               (when (plusp (fill-pointer buffer))
                 (decf (fill-pointer buffer))))
              (otherwise
               (if (characterp ch)
                   (push-chaine-buffer ch buffer)
                   (lse-error "ERREUR INTERNE: VALEUR INATTENDUE DE ~S: ~S of type ~S"
                              (list 'io-read-buffered-character (class-name (class-of input)))
                              ch (type-of ch)))))))))


(defmethod io-read-line ((task t) &key (echo t) (beep nil))
  (io-read-string task :echo echo :beep beep))


(defparameter *terminators*
  (remove-duplicates (vector #\space :xoff :return
                             #\Newline
                             #+has-tab      #\Tab
                             #+has-return   #\return
                             #+has-linefeed #\linefeed
                             #+has-page     #\page
                             #+has-vt       #\vt)))


(defmethod io-read-number ((task t) &key (echo t) (beep t))
  "
DO:         Reads a number.

skip spaces as many as you want.
read characters until RET or C-s or spaces or ESC or buffer full.
ESC is interrupt.
string full is an error condition, 
parse the buffer and return the number or signal an error.

ECHO: Whether this input must be done echoing the characters (default T).
BEEP: Whether the terminal should beep before reading the string (default NIL).

RETURN: The LSE number read.
"
  (let* ((terminal (task-terminal task))
         (input    (if (io-terminal-input-p task)
                       terminal
                       (task-input task))))
    (with-temporary-echo (terminal echo)
      (when (and beep (task-allow-bell-output task)) (terminal-ring-bell terminal))
      (terminal-skip-characters input *terminators*)
      (let ((buffer
             (loop
               :named reading
               :with buffer = (make-array 8 :adjustable t :fill-pointer 0 :element-type 'character)
               :for ch = (io-read-buffered-character task)
               :do (cond
                     ((find ch *terminators*)
                      (return-from reading buffer))
                     ((eql ch :delete)
                      (when (plusp (fill-pointer buffer))
                        (decf (fill-pointer buffer))))
                     ((characterp ch)
                      (push-nombre-buffer ch buffer))
                     (t
                      (lse-error "ERREUR INTERNE: VALEUR INATTENDUE DE ~S: ~S of type ~S"
                                 (list 'io-read-buffered-character (class-name (class-of input)))
                                 ch (type-of ch)))))))
        (handler-case
            (destructuring-bind (donnee position) (parse-donnee-lse buffer)
              (if (< position (length buffer))
                  (lse-error "SYNTAXE INVALIDE ~S, ATTENDU UN NOMBRE" buffer)
                  donnee))
          (error ()
            (lse-error "DONNEE INVALIDE ~S, ATTENDU UN NOMBRE" buffer)))))))




;;;; THE END ;;;;
