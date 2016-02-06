(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

#-(and) "

- un sifflement par instruction LIRE.

- valeur numérique assignée à la variable à la lecture de X-OFF ou
  SPC.

- valeur chaîne assignée à la variable à la lecture de X-OFF ou RET
  sur Mitra-15 (mais RET inclus).



Do we want to signal an error as soon as an invalid character is read?

-> we may want rather to wait for the space that finish the number
   input, to parse the previous text and see if there are invalid
   characters, or invalid syntax, since the user may delete previous
   characters with \ (or BS, DEL, etc).

So we may scan until the terminator (space, newline, xoff) anyways,
and parse the read string.


"


(defmacro with-source (source &body body)
  `(let ((*source* ,source))
     ,@body))

(defmacro possibly (expression)
  )

(defmacro repeat (expression)
  )

(defmacro sequence (&body sequents)
  )

(defun mark ()
  (push))
(defmacro alternative (&body alternatives)
  `(or ,@(loop
           :for alternative :in alternatives
           :collect `(prog1 (progn (mark) ,alternatives)
                       (unmark)))))

(defun digit ()
  (digit-char-p (next-character)))

(defun spaces ()
  (find (next-character) #(#\space #\tab)))

(defun new-lines ()
  (find (next-character) #(#\newline #\return #\linefeed)))

(defun x-off ()
  (char= (next-character) #.(code-char 19)))

(defun sign ()
  (alternative (expect #\-) (expect #\+)))

(defun expect (ch)
  (char= ch (next-character)))

(define-condition unexpected-character (error)
  ())

(define-condition buffer-overflow (error)
  ())


(defmethod terminal-read-number ((terminal terminal) &key (echo t) (beep nil))
  (with-temporary-echo (terminal echo)
    (when beep
      (terminal-ring-bell terminal))
    (terminal-finish-output terminal)
    ;; " *[-+]?[0-9]+(.[0-9]+([Ee][-+]?[0-9]+?)?)? *($| .*)"
    (handler-case
        (with-source (read-char
                      unread-char)
         (sequence (possibly (repeat (spaces)))
                   (possibly (sign))
                   (repeat (digit))
                   (possibly (sequence (expect #\.)
                                       (repeat (digit))
                                       (possibly (alternative (expect #\E) (expect #\e))
                                                 (possibly (sign))
                                                 (repeat (digit)))))
                   (alternative (spaces)
                                (x-off)
                                (new-lines))))
      (unexpected-character
       (err)
       (lse-error "DONNEE INVALIDE ~S, ATTENDU UN NOMBRE" donnee))
      (buffer-overflow
       (lse-error "DONNEE INVALIDE ~S, ATTENDU UN NOMBRE" donnee))
      (end-of-file
       (err)
       (lse-error "DONNEE INVALIDE ~S, ATTENDU UN NOMBRE" donnee))
      (error
       (err)
       (lse-error "DONNEE INVALIDE ~S, ATTENDU UN NOMBRE" donnee)))))

