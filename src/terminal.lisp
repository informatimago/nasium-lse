;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               terminal.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the abstract TERMINAL class and the concrete STANDARD-TERMINAL class.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2013-07-27 <PJB> Extracted from io.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2013 - 2014
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LSE")


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defconstant XON                DC1)
  (defconstant XOFF               DC3)

  );;eval-when


;; reading numbers or strings from tape or from a terminal is done by
;; the same routines (io-read-string, io-read-number).


;; TODO: while reading from the tape, echoing to the terminal is
;; subject to the same conditions as when reading from the terminal
;; (SIlence, ESC).   See also if DC1, DC3, DC4 and NAK should be
;; processed when read from tape (IIRC, they should). cf. IO-FORMAT



(defgeneric terminal-character-keysym (terminal character)
  (:documentation "Maps the CHARACTER to a keysym or to itself.")
  (:method (terminal character)
    (declare (ignorable terminal))
    (case character
      
      #+has-ascii-code
      ((#.(code-char SOH))      :attention)

      #+(and has-escape
         (or (not has-ascii-code)
             #.(cl:if (cl:char= (cl:code-char COM.INFORMATIMAGO.LSE::ESC) #\Escape) '(:and) '(:or))))
      ((#\Escape)               :escape)
      
      #+(and has-ascii-code
         (or (not has-escape)
             #.(cl:if (cl:char/= (cl:code-char COM.INFORMATIMAGO.LSE::ESC) #\Escape) '(:and) '(:or))))
      ((#.(code-char ESC))      :escape)


      #+(and has-return
         (or (not has-ascii-code)
             #.(cl:if (cl:char= (cl:code-char COM.INFORMATIMAGO.LSE::CR) #\Return) '(:and) '(:or))))
      ((#\Return)               :return)

      #+(and has-ascii-code
         (or (not has-return)
             #.(cl:if (cl:char/= (cl:code-char COM.INFORMATIMAGO.LSE::CR) #\Return) '(:and) '(:or))))
      ((#.(code-char CR))       :return)
            

      #+has-ascii-code ((#.(code-char XOFF))     :xoff)
      ((#\Newline)                               :xoff)

      #+has-backspace  ((#\Backspace)            :delete)
      #+has-rubout     ((#\Rubout)               :delete)
      ((#\\)                                     :delete)
      
      (otherwise                                 character))))

(defgeneric terminal-keysym-character (terminal keysym)
  (:documentation "Maps the keysym to a CHARACTER or NIL if not supported.")
  (:method (terminal keysym)
    (declare (ignorable terminal))
    (case keysym
      (:escape     (or #+has-escape     #\Escape
                       #+has-ascii-code #.(code-char ESC)))
      (:attention  (or #+has-ascii-code #.(code-char SOH)))
      (:xoff       (or #+has-ascii-code #.(code-char XOFF)))
      (:delete     (or #+has-rubout     #\Rubout
                       #+has-ascii-code #.(code-char DEL)
                       #\\))
      (:return     (or #+has-return     #\Return
                       #+has-ascii-code #.(code-char CR)
                       #\Newline))
      (:bell       (or #+has-bell       #\Bell))
      (:line-feed  (or #+has-linefeed   #\Linefeed))
      (:backspace  (or #+has-backspace  #\Backspace))
      (:page       (or #+has-page       #\Page))
      (otherwise
       (when (characterp keysym)
         keysym)))))


(defgeneric terminal-keysym-label (terminal keysym)
  (:documentation "
Maps the keysym to a string describing the key-chord that must be typed on that terminal.
The second value indicates whether that keysym is available.
KEYSYM: (MEMBER :ESCAPE :ATTENTION :XOFF :DELETE :RETURN)
")
  (:method (terminal keysym)
    (declare (ignorable terminal))
    (let ((available (terminal-keysym-character terminal keysym)))
      (ecase keysym
        (:escape    (values (if available "[ECHAPEMENT]" "(NON DISPONIBLE)") available))
        (:attention (values (if available "[CONTRÔLE-A]" "(NON DISPONIBLE)") available))
        (:xoff      (values (if available "[CONTRÔLE-S]" "(NON DISPONIBLE)") available))
        (:delete    (values (if available
                                (or #+has-rubout     "[EFFACEMENT]"
                                    #+has-ascii-code "[EFFACEMENT]"
                                    #\\)
                                "(NON DISPONIBLE)") available))
        (:return    (values "[ENTRÉE]" t))))))


(defgeneric terminal-get-next-char (source)
  (:method ((stream stream))
    (read-char stream nil nil)))


(defgeneric terminal-read-buffered-character (source)
  (:documentation "
Return a character or a keyword representing a key, read from a
possibly buffered source.  The possible keywords are: :xoff :delete
:return

Upon end-of-file, NIL is returned.
")
  (:method (source)
    (let* ((ch     (terminal-get-next-char source))
           (keysym (terminal-character-keysym source ch)))
      #+lse-input-debug (io-format *task* "~&~S -> ~A .~A.~%" 'terminal-read-buffered-character ch (when (characterp ch) (char-code ch)))
      keysym)))


(defgeneric terminal-skip-characters (source characters)
  (:documentation "
Read characters with TERMINAL-READ-BUFFERED-CHARACTER while they're in
the sequence CHARACTERS.  The first character out of that sequence
will be read by the next TERMINAL-READ-BUFFERED-CHARACTER call.
")
  (:method ((stream stream) characters)
    (loop
      :named reading
      :for ch = (read-char stream nil nil)
      :while (find ch characters)
      :finally (when ch (unread-char ch stream)))))



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

;; terminal attributes:
;; --------------------------

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



(defgeneric terminal-number-terminators (terminal)
  (:documentation "Return a sequence of keysym or characters that terminate reading numbers (spaces, x-off, return, etc)."))


;; (defparameter *io-bell-char*   (or #+has-bell     #\Bell     (code-char BEL)))
;; (defparameter *io-ctrl-a*      (code-char SOH))
;; (defparameter *io-escape*      (or #+has-escape   #\Escape   (code-char ESC)))
;; (defparameter *io-line-feed*   (or #+has-linefeed #\Linefeed (code-char LF)))
;; (defparameter *io-return*      (or #+has-return   #\Return   (code-char CR)))
;; (defparameter *io-x-off*       (code-char XOFF))



;; terminal output functions:
;; --------------------------

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


;; terminal input functions:
;; --------------------------

(defgeneric terminal-yield (terminal)
  (:documentation "Allows the terminal to check for interruptions and signal user-interrupt conditions.")
  (:method (terminal)
    (declare (ignorable terminal))
    (values)))



;;----------------------------------------------------------------------
;; Standard I/O Terminal
;;----------------------------------------------------------------------

(defclass standard-terminal (terminal)
  ((output-stream     :initarg  :output-stream
                      :reader terminal-output-stream
                      :initform      *standard-output*)
   (input-stream      :initarg  :input-stream
                      :reader terminal-input-stream
                      :initform      *standard-input*)
   (input-end-of-file :initform nil
                      :type   boolean
                      :documentation "Weither an end-of-file condition has been detected on the terminal input stream.")))


;; terminal output functions:
;; --------------------------

(defmethod terminal-ring-bell ((terminal standard-terminal))
  (let ((output (terminal-output-stream terminal))
        (bell   (terminal-keysym-character terminal :bell)))
    (when bell
      (princ bell output))
    ;; Note: we call finish-output all the same, since bell is often used before reading input.
    (terminal-finish-output terminal)))

(defmethod terminal-carriage-return ((terminal standard-terminal))
  (let ((output (terminal-output-stream terminal))
        (carret (terminal-keysym-character terminal :return)))
    (princ (or carret #\Newline) output)
    (terminal-finish-output terminal)))

(defmethod terminal-line-feed ((terminal standard-terminal) &optional (count 1))
  (let ((output (terminal-output-stream terminal))
        (linfee (or (terminal-keysym-character terminal :linefeed)
                    #\Newline)))
    (loop :repeat count :do (princ linfee output))
    (terminal-finish-output terminal)))

(defmethod terminal-new-line ((terminal standard-terminal) &optional (count 1))
  (let ((output (terminal-output-stream terminal)))
    (loop :repeat count :do (terpri output))
    (terminal-finish-output terminal)))

(defmethod terminal-write-string ((terminal standard-terminal) string &key (start 0) (end nil))
  (write-sequence string (terminal-output-stream terminal) :start start :end end))

(defmethod terminal-finish-output ((terminal standard-terminal))
  (finish-output (terminal-output-stream terminal)))


;; terminal input functions:
;; --------------------------

(defmethod terminal-get-next-char ((terminal standard-terminal))
  (let ((ch (read-char (terminal-input-stream terminal) nil nil)))
    (with-slots (input-end-of-file) terminal
      (setf input-end-of-file (null ch)))
    ch))


(defmethod terminal-skip-characters ((terminal standard-terminal) characters)
  (loop
    :named reading
    :for ch = (terminal-read-buffered-character terminal)
    :when (null ch)
      :do (return-from terminal-skip-characters)
    :while (find ch characters)
    :finally (unread-char (terminal-keysym-character terminal ch)
                          (terminal-input-stream terminal))))




;;;; THE END ;;;;
