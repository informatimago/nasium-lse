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
;;;;    Copyright Pascal J. Bourguignon 2000 - 2004
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
;;;;****************************************************************************

(in-package "COM.INFORMATIMAGO.LSE")


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defconstant XON              DC1)
  (defconstant XOFF             DC3)

  (defconstant +TAPE-READER-ON+   DC1)
  (defconstant +TAPE-PUNCHER-ON+  DC2)
  (defconstant +TAPE-PUNCHER-OFF+ DC4)
  (defconstant +TAPE-READER-OFF+  NAK)

  );;eval-when


;;----------------------------------------------------------------------
;;; Tape I/O
;;----------------------------------------------------------------------


(defun io-stop-tape-reader (task)
  (setf (task-input  task) (terminal-input-stream (task-terminal task))))

(defun io-stop-tape-puncher (task)
  (setf (task-output task) (terminal-output-stream (task-terminal task))))


(defun io-start-tape-reader (task)
  (setf (task-input  task) (task-tape-input task)))

(defun io-start-tape-puncher (task)
  (setf (task-output task) (task-tape-output task)))


(defun io-standard-redirection (task)
  (io-stop-tape-puncher task)
  (io-stop-tape-reader  task)
  (setf (task-silence task) nil)
  (values))


;;----------------------------------------------------------------------
;; Terminal Class
;;----------------------------------------------------------------------

(defclass terminal ()
  ())

(defgeneric terminal-initialize (terminal)
  (:documentation "Initialize the terminal (remote device).")
  (:method (terminal) terminal))

(defgeneric terminal-finalize (terminal)
  (:documentation "Finalize the terminal (remote device).")
  (:method (terminal) terminal))

(defgeneric terminal-columns (terminal)
  (:documentation "Returns the number of columns in the terminal.")
  (:method (terminal)
    (declare (ignorable terminal))
    80))

(defgeneric terminal-rows (terminal)
  (:documentation "Returns the number of rows in the terminal.")
  (:method (terminal)
    (declare (ignorable terminal))
    25))

(defgeneric terminal-input-stream (terminal)
  (:documentation "Returns the input stream used to read from the terminal.")
  (:method (terminal)
    (declare (ignorable terminal))
    *terminal-io*))

(defgeneric terminal-output-stream (terminal)
  (:documentation "Returns the output stream used to write to the terminal.")
  (:method (terminal)
    (declare (ignorable terminal))
    *terminal-io*))
  
(defgeneric terminal-ring-bell (terminal)
  (:documentation "Ring a bell on the terminal.")
  (:method (terminal)
    (declare (ignorable terminal))
    (values)))

(defgeneric terminal-carriage-return (terminal)
  (:documentation "Move the cursor to the beginning of the line (Carriage Return).")
  (:method (terminal)
    (declare (ignorable terminal))
    (values)))

(defgeneric terminal-new-line (terminal &optional count)
  (:documentation "Write COUNT new lines (Line Feed)")
  (:method (terminal &optional count)
    (declare (ignorable terminal count))
    (values)))

(defgeneric terminal-write-string (terminal string &key start end)
  (:documentation "Write a sub-string of STRING from START to END to the terminal.")
  (:method (terminal string &key (start 0) (end nil))
    (declare (ignorable terminal string start end))
    (values)))

(defgeneric terminal-finish-output (terminal)
  (:documentation "Flush output buffers.")
  (:method (terminal)
    (declare (ignorable terminal))
    (values)))

(defgeneric terminal-read-line (terminal &key echo beep)
  (:documentation "Read a line from the terminal.")
  (:method (terminal &key echo beep)
    (declare (ignorable terminal echo beep))
    ""))

(defgeneric terminal-read (terminal &key echo beep)
  (:documentation "Read an object from the terminal.")
  (:method (terminal &key echo beep)
    (declare (ignorable terminal echo beep))
    nil))

(defgeneric terminal-echo (terminal)
  (:documentation "Returns the current echo status of the terminal.")
  (:method (terminal)
    (declare (ignorable terminal))
    t))

(defgeneric (setf terminal-echo) (new-echo terminal)
  (:documentation "Sets the echo status of the terminal.
When true, the input on that terminal is echoed automatically.
When false, no automatic echo occurs.")
  (:method (new-echo terminal)
    (declare (ignorable terminal))
    new-echo))

(defgeneric terminal-yield (terminal)
  (:documentation "Allows the terminal to check for interruptions and signal user-interrupt conditions.")
  (:method (terminal)
    (declare (ignorable terminal))
    (values)))

;;----------------------------------------------------------------------
;; Standard I/O Terminal
;;----------------------------------------------------------------------

(defclass standard-terminal (terminal)
  ((input-stream  :initarg :input-stream
                  :reader terminal-input-stream
                  :initform *standard-input*)
   (output-stream :initarg :output-stream
                  :reader terminal-output-stream
                  :initform *standard-output*)))

(defparameter *io-bell-char*
  (or (ignore-errors (read-from-string "#\\Bell"))
      (code-char BEL)))

(defparameter *io-line-feed*
  (or (ignore-errors (read-from-string "#\\Linefeed"))
      (code-char LF)))

(defmethod terminal-ring-bell ((terminal standard-terminal))
  (let ((output (terminal-output-stream terminal)))
    (princ *io-bell-char* output)
    (terminal-finish-output terminal)))

(defmethod terminal-carriage-return ((terminal standard-terminal))
  (let ((output (terminal-output-stream terminal)))
    (write-char #\Return output)
    (terminal-finish-output terminal)))

(defmethod terminal-line-feed ((terminal standard-terminal) &optional (count 1))
  (let ((output (terminal-output-stream terminal)))
    (loop :repeat count :do (princ *io-line-feed* output))
    (terminal-finish-output terminal)))

(defmethod terminal-new-line ((terminal standard-terminal) &optional (count 1))
  (let ((output (terminal-output-stream terminal)))
    (loop :repeat count :do (terpri output))
    (terminal-finish-output terminal)))




(defmethod terminal-write-string ((terminal standard-terminal) string &key (start 0) (end nil))
  (write-sequence string (terminal-output-stream terminal) :start start :end end))

(defmethod terminal-finish-output ((terminal standard-terminal))
  (finish-output (terminal-output-stream terminal)))


(defmacro with-temporary-echo ((terminal echo) &body body)
  (let  ((vterminal   (gensym))
         (vecho       (gensym))
         (vsaved-echo (gensym)))
    `(let* ((,vterminal ,terminal)
            (,vecho ,echo)
            (,vsaved-echo (terminal-echo ,vterminal)))
       (setf (terminal-echo ,vterminal) ,vecho)
       (unwind-protect (progn ,@body)
         (setf (terminal-echo ,vterminal) ,vsaved-echo)))))


(defmethod terminal-read-line ((terminal standard-terminal) &key (echo t) (beep nil))
  (with-temporary-echo (terminal echo)
    (when beep
      (terminal-ring-bell terminal))
    (read-line (terminal-input-stream terminal))))


(defparameter *lse-readtable*
  (loop
    :with rt =  (copy-readtable nil)
    :for i :below char-code-limit
    :for ch = (code-char i)
    :when ch
    :do (set-macro-character ch nil nil rt)
    :finally (return rt))
  "A readtable to read L.S.E. data.  Ie, just numbers.
Symbols will signal an error,
and strings are read with read-line.")


(defmethod terminal-read ((terminal standard-terminal) &key (echo t) (beep nil))
  (with-temporary-echo (terminal echo)
    (when beep
      (terminal-ring-bell terminal))
    (handler-case (let ((*read-eval* nil)
                        (*readtable* *lse-readtable*))
                    (read (terminal-input-stream terminal)))
      (reader-error ()
        (clear-input (terminal-input-stream terminal))
        (lse-error "ENTREE INVALIDE")))))

;;----------------------------------------------------------------------

(defun io-terminal-output-p (task)
  (eql (task-output task) (terminal-output-stream (task-terminal task))))

(defun io-terminal-input-p  (task)
  (eql (task-input task) (terminal-input-stream (task-terminal task))))

(defun io-tape-output-p (task)
  (eql (task-output task) (task-tape-output task)))

(defun io-tape-input-p  (task)
  (eql (task-input task) (task-tape-input task)))



(defun io-bell (task)
  (terminal-ring-bell (task-terminal task)))

(defun io-carriage-return (task)
  (if (io-terminal-output-p task)
      (terminal-carriage-return (task-terminal task))
      (princ #\Return (task-output task))))

(defun io-line-feed (task &optional (count 1))
  (if (io-terminal-output-p task)
      (terminal-line-feed (task-terminal task) count)
      (format (task-output task) "~V,,,VA" count LF "")))

(defun io-new-line (task &optional (count 1))
  (if (io-terminal-output-p task)
      (terminal-new-line (task-terminal task) count)
      (terpri (task-output task))))

(defun io-finish-output (task)
  (if (io-terminal-output-p task)
      (terminal-finish-output (task-terminal task))
      (finish-output (task-output task))))

(defun io-read-line (task &key (echo t) (beep nil))
  (if (io-terminal-input-p task)
      (terminal-read-line (task-terminal task) :echo echo :beep beep)
      (read-line (task-input task))))

(defun io-read (task)
  (if (io-terminal-input-p task)
      (terminal-read (task-terminal task))
      (read (task-input task))))

(defun io-read-string (task)
  (io-read-line task))

(defun io-read-number (task)
  (let ((value (io-read task)))
    (typecase value
      (integer (un-nombre value))
      (nombre  value)
      (t (lse-error "UN NOMBRE ETAIT ATTENDU, PAS ~S" value)))))

(defun io-echo (task)
   (terminal-echo (task-terminal task)))

(defun (setf io-echo) (new-echo task)
  (setf (terminal-echo (task-terminal task)) new-echo))




(defparameter *dectech-leftwards-arrow* (or (ignore-errors (code-char #xfb)) #\_))
(defparameter *dectech-upwards-arrow*   (or (ignore-errors (code-char #xfc)) #\^))
(defparameter *unicode-leftwards-arrow* (or (ignore-errors (code-char 8592)) #\_))
(defparameter *unicode-upwards-arrow*   (or (ignore-errors (code-char 8593)) #\^))
;; (Some implementations have unicode but don't know #\upwards_arrow, etc.)


(defun io-substitute (task string)
  (if (io-terminal-output-p task)
      (with-output-to-string (out)
        (loop
          :with dectech = (task-dectech task)
          :with unicode = (task-unicode task)
          :for ch :across string
          :do (case ch
                ((#\_) (cond
                         (dectech (princ *dectech-leftwards-arrow* out))
                         (unicode (princ *unicode-leftwards-arrow* out))
                         (t       (princ ch out))))
                ((#\^) (cond
                         (dectech (princ *dectech-upwards-arrow* out))
                         (unicode (princ *unicode-upwards-arrow* out))
                         (t       (princ ch out))))
                (otherwise
                 (if (task-upcase-output task)
                     (if (lower-case-p ch)
                         (princ (char-upcase ch) out)
                         (princ ch out))
                     (princ ch out))))))
      string))



(defparameter *io-active-codes* (vector +TAPE-READER-ON+   +TAPE-PUNCHER-ON+
                                        +TAPE-PUNCHER-OFF+ +TAPE-READER-OFF+
                                        XOFF CR LF))


(defun io-format (task control-string &rest arguments)
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




;; Le thread principal peut lire directement le ruban, 
;; mais pour le terminal, il doit prendre les caracteres dans tampon_entree,
;; et attendre sur le thread terminal.
;;
;; Le thread terminal lit chaque caractere venant du terminal et 
;; les aiguille :
;;     - interuption (ESC),
;;     - signal (CTRL-A),
;;     - tampon_entree lorsque le thread principal attend une entrée terminal,
;;     - /dev/null lorsque le thread principal n'attend pas d'entrée terminal,
;;     - echo sur le terminal lorsqu'on est pas en mode SILENCE.
;;     - fin de l'entrée et signal du thread principal sur terminateur.
;;
;; On va faire lire le ruban  par le thread terminal. Il fera un poll
;; (on pourrait avoir  plus de 32 descripteurs ouverts  en même temps
;; avec 16  consoles+rubans) sur  le terminal et  le ruban.  Quand le
;; ruban est lu,  on ignore toute entrée sur  le terminal sauf CTRL-A
;; et ESC.


;; (ext:with-keyboard
;;     (loop 
;;        for ch = (read-char-no-hang ext:*keyboard-input*)
;;        until (equalp ch #S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\C))
;;        do (when ch (print ch))))
;; 
;; 
;; NIL or:
;; #S(SYSTEM::INPUT-CHARACTER :CHAR #\f :BITS 0 :FONT 0 :KEY NIL) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR #\s :BITS 0 :FONT 0 :KEY NIL) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR #\c :BITS 0 :FONT 0 :KEY NIL) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR #\s :BITS 0 :FONT 0 :KEY NIL) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR #\x :BITS 0 :FONT 0 :KEY NIL) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\S) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\S) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\C) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR #\q :BITS 0 :FONT 0 :KEY NIL) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\C) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\C) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\C) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\Q) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\Q) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\C) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\C) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\Z) 
;; #S(SYSTEM::INPUT-CHARACTER :CHAR #\Escape :BITS 0 :FONT 0 :KEY NIL) 


;; (defun io-valid-tape-name-p (name)
;;   "
;; RETURN: Whether name is a string with the following format:
;;         name ::= part | part '/' name .
;;         part ::= [A-Za-z][A-Za-z0-9]{0,4} /
;; "
;;   (do ((i 0)
;;        (l 0)
;;        (state :first))
;;       ((>= i (length name)) t)
;;     (if (eq state :first)
;;         (if (alpha-char-p (char name i))
;;             (setf i (1+ i) l 1 state :rest)
;;             (return-from io-valid-tape-name-p nil))
;;         (cond
;;           ((alphanumericp (char name i))
;;            (setf i (1+ i) l (1+ l))
;;            (when (> l 5) (return-from io-valid-tape-name-p nil)))
;;           ((char= (character "/") (char name i))
;;            (setf i (1+ i) state :first))
;;           (t
;;            (return-from io-valid-tape-name-p nil))))))
;; 
;; 
;; (defun io-redirect-input-from-tape (task tape-name)
;;   "
;; RETURN: The task-tape-input or nil.
;; "
;;   (when (task-tape-input task)
;;     (close (task-tape-input task))
;;     (setf  (task-tape-input task) nil))
;;   (setf (task-input task) (terminal-input-stream (task-terminal task)))
;;   (let ((path (catalog-make-path :tape name)))
;;     (if path
;;         (if (setf (task-tape-input task)
;;                   (open path :direction :input :if-does-not-exists nil))
;;             (setf (task-input task) (task-tape-input task))
;;             (error 'file-error-file-is-inaccessible :pathname path))
;;         (error 'file-error-file-does-not-exist :pathname path)))
;;   (task-tape-input task))
;; 
;; 
;; (defun io-redirect-output-to-tape (task tape-name)
;;   "
;; RETURN: The task-tape-output or nil.
;; "
;;   (when (task-tape-output task)
;;     (close (task-tape-output task))
;;     (setf  (task-tape-output task) nil))
;;   (setf (task-output task) (terminal-output-stream (task-terminal task)))
;;   (let ((path (catalog-make-path :tape name)))
;;     (if path
;;         (if (setf (task-tape-output task)
;;                   (open path :direction :output
;;                         :if-does-not-exists :create
;;                         :if-exists :supersede))
;;             (setf (task-output task) (task-tape-output task))
;;             (error 'file-error-file-is-inaccessible :pathname path))
;;         (error 'file-error-file-does-not-exist :pathname path)))
;;   (task-tape-output task))




;;;; THE END ;;;;
