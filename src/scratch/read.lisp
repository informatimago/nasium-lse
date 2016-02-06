(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

#-(and) "

- un sifflement par instruction LIRE.

- valeur numérique assignée à la variable à la lecture de X-OFF ou
  SPC.

- valeur chaîne assignée à la variable à la lecture de X-OFF ou RET
  sur Mitra-15 (mais RET inclus).




standard

read line by line.  The standard doesn't preclude reading character by
character, but there's no conforming way to configure it.


swank

read line by line (but possibly also character by character with
support from emacs).


unix

read character by character, with full support for ESC, C-a, C-s etc.


reading strings:

read characters until RET or C-s or ESC or string full.
ESC is interrupt.
string full is an error condition, 
RET is included in the string, C-s not.  (It's virtual RET/C-s, real RET can be mapped to C-s).


reading numbers:

skip spaces as many as you want.
read characters until RET or C-s or spaces or ESC or buffer full.
ESC is interrupt.
string full is an error condition, 
parse the buffer and return the number or signal an error.

Now the problem is that reading can be buffered already, so we may get a (unfinished) line.


Interrupts (ESC, C-a) are processed by the low level I/O.
"





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
;; (defmethod io-redirect-input-from-tape ((task t) tape-name)
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
;; (defmethod io-redirect-output-to-tape ((task t) tape-name)
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

