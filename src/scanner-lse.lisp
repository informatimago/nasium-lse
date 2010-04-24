;;****************************************************************************
;;FILE:               scanner-lse.lisp
;;LANGUAGE:           Common-Lisp
;;SYSTEM:             Common-Lisp
;;USER-INTERFACE:     NONE
;;DESCRIPTION
;;    
;;    EMULSE : L.S.E. [ EMULATION MITRA-15 ]
;;    
;;    An emultator of the CII MITRA-15 L.S.E. System 
;;    and programming language interpreter.
;;    
;;    A scanner for LSE.
;;    
;;AUTHORS
;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;MODIFICATIONS
;;    2005-08-21 <PJB> Created
;;BUGS
;;LEGAL
;;    GPL
;;    
;;    Copyright Pascal Bourguignon 2005 - 2005
;;
;;    This file is part of EMULSE :  L.S.E.  [ EMULATION MITRA-15 ]
;;    
;;    This program is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU General Public License
;;    as published by the Free Software Foundation; either version
;;    2 of the License, or (at your option) any later version.
;;    
;;    This program is distributed in the hope that it will be
;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;    PURPOSE.  See the GNU General Public License for more details.
;;    
;;    You should have received a copy of the GNU General Public
;;    License along with this program; if not, write to the Free
;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;    Boston, MA 02111-1307 USA
;;****************************************************************************


(defconstant +maybe-commentaire+ 0 "LSE Scanner State")
(defconstant +not-commentaire+   1 "LSE Scanner State")
(defconstant +in-format+         2 "LSE Scanner State")


;; TODO: Move these maps to the scanner object as attributes.
(defparameter *motcle-map* nil
  "Maps LSE keyword tokens. ''TOKEN'' --> (tok-en . zebu-index)")
(defparameter *token-map* nil
  "Maps non-keyword tokens. ''TOKEN'' --> (tok-en . zebu-index)")

;; TODO: Move this to the initialization of the scanner object.
(let* ((grammar     (or (find-grammar "LSE")
                        (error "JE NE TROUVE PAS MA GRAMMAIRE LSE")))
       (end-index   (zebu::grammar-end-symbol-index grammar))
       (lex-cat-map (zebu::grammar-lex-cat-map      grammar))
       (lexic (mapcar
               (lambda (x) (List (first x) (delete (character "\\") (second x))))
               (zebu::grammar-lex-cats grammar)))
       (tokenp (lambda (item)
                 (and (char= (character "{") (char (second item) 0))
                      (char= (character "}")
                             (char (second item)
                                   (1- (length (second item))))))))
       (motcles (remove-if     tokenp lexic))
       (tokens  (remove-if-not tokenp lexic)))
  (labels ((lex-cat (tok)
             (find (ignore-errors (symbol-function (first tok)))
                   lex-cat-map :key (function cdr)))
           (enter (slot tok table)
             (let ((lex-cat (lex-cat tok)))
               (unless (or lex-cat (eq (first tok) 'tok-erreur))
                 (error "JE NE TROUVE PAS LE LEXEME <~A>" (first tok)))
               (setf (gethash (funcall slot tok) table)
                     (list (or (car lex-cat) 0) (first tok)
                           (second tok) (or (cdr lex-cat) (constantly nil)))))))
    (setf *motcle-map* (make-hash-table :test (function equal))
          *token-map*  (make-hash-table :test (function eq)))
    (dolist (tok motcles) (enter (function second) tok *motcle-map*))
    (dolist (tok tokens)  (enter (function first)  tok *token-map*))
    (setf (gethash 'tok-eol *token-map*) (list end-index 'tok-eol "{EOL}" nil)
          (gethash 'tok-eof *token-map*) (list end-index 'tok-eof "{EOF}" nil))
    ;; The following forbid loading this file twice after parser-lse.zb...
    (dolist (tok lexic)
      (let ((lex-cat (lex-cat tok)))
        (setf (cdr lex-cat)
              (let ((fun (cdr lex-cat)))
                (lambda (STRING &OPTIONAL (START 0) (END (LENGTH STRING)))
                  (format *trace-output*
                    "~&FONCTION DU DOMAINE LSE ~S APPELEE~%" fun)
                  (funcall fun string start end))))))
    (values)))


(defclass token ()
  ((kind       :initarg :kind
               :accessor token-kind
               :initform nil
               :type symbol)
   (index      :initarg :index
               :accessor token-index
               :initform 0
               :type (integer 0))
   (text       :accessor token-text
               :initarg :text
               :initform ""
               :type     string)
   (column     :accessor token-column
               :initarg :column
               :initform 0
               :type (integer 0))
   (line       :accessor token-line
               :initarg :line
               :initform 0
               :type (integer 0))))

(defmethod print-object ((self token) stream)
  (print-unreadable-object (self stream :type t :identity t)
    (format stream "~D:~D: <~A> ~S"
            (token-line self) (token-column self)
            (token-kind self) (token-text self)))
  self)

(defclass erreur         (token) ())
(defclass commentaire    (token) ())
(defclass motcle         (token) ())

(defclass identificateur (token)
  ((nom        :accessor identificateur-nom
               :initarg :nom
               :initform nil
               :type     symbol)))

(defmethod initialize-instance :after ((self identificateur) &rest args)
  (declare (ignore args))
  (setf (identificateur-nom self)
        (intern (slot-value self 'text)
                (find-package "COM.INFORMATIMAGO.LSE.IDENTIFIERS"))
        (token-text self) "")
  self)

(defmethod token-text ((self identificateur))
  (symbol-name (identificateur-nom self)))

(defclass numero (token)
  ((valeur     :accessor numero-valeur
               :initarg :valeur
               :initform 0
               :type     (integer 0))))

(defmethod initialize-instance :after ((self numero) &rest args)
  (declare (ignore args))
  (setf (numero-valeur self) (parse-integer (token-text self)))
  self)


(defclass nombre (token)
  ((valeur     :accessor nombre-valeur
               :initarg :valeur
               :initform 0.0
               :type     double-float)))

(defmethod initialize-instance :after ((self nombre) &rest args)
  (declare (ignore args))
  (setf (nombre-valeur self) (read-from-string (token-text self)))
  self)


(defclass chaine (token)
  ((valeur     :accessor chaine-valeur
               :initarg :value
               :initform ""
               :type     string)))

(defmethod initialize-instance :after ((self chaine) &rest args)
  (declare (ignore args))
  (setf (chaine-valeur self)
        (loop
           :with literal = (slot-value self 'text)
           :with result  = (make-array (length (slot-value self 'text))
                                       :element-type 'character
                                       :fill-pointer 0)
           :with j = -1
           :with i = 1
           :while (< (1+ i) (length literal))
           :do
           (vector-push (aref literal i) result)
           (incf i (if (and (char= (character "'") (aref literal i))
                            (char= (character "'") (aref literal (1+ i))))
                       2 1))
           :finally (return result))
        (token-text self) "")
  self)

(defmethod token-text ((self chaine))
  (with-output-to-string (out)
    (princ "'" out)
    (loop for ch across (chaine-valeur self) do
         (if (char= (character "'") ch)
             (princ "''" out)
             (princ ch out)))
    (princ "'" out)))


(defmethod eolp ((self t))       nil)
(defmethod eolp ((self motcle))  (eq (token-kind self) 'tok-eol))

(defmethod eofp ((self t))       nil)
(defmethod eofp ((self motcle))  (eq (token-kind self) 'tok-eof))


(defclass SCANNER ()
  ((SOURCE     :ACCESSOR SOURCE
               :INITARG :SOURCE
               :TYPE (or null STREAM))
   (buffer     :accessor buffer
               :type     (or null string))
   (column     :accessor column
               :initform 0
               :type (integer 0))
   (line       :accessor line
               :initform 0
               :type (integer 0))
   (state      :accessor state
               :initform 0
               :type (integer 0 2))
   (token      :accessor token))
  (:DOCUMENTATION "A scanner for L.S.E."))


(defmethod initialize-instance :after ((self scanner) &rest args)
  (declare (ignore args))
  (setf (token self) (make-eol self))
  self)


(defmethod make-eol ((self scanner))
  (make-instance 'motcle
    :kind    'tok-eol
    :index   (first (gethash 'tok-eol *token-map*))
    :text    (third  (gethash 'tok-eol *token-map*))
    :column  (column self)
    :line    (line   self)))


(defmethod make-eof ((self scanner))
  (make-instance 'motcle
    :kind    'tok-eof
    :index   (first (gethash 'tok-eof *token-map*))
    :text    (third  (gethash 'tok-eof *token-map*))
    :column  (column self)
    :line    (line   self)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +code-limit+ 128 "Number of characters in the LSE charset"))
                                        ; ASCII

(defmacro case-state (statevar codevar &rest transitions)
  "
STATEVAR:   Variable used in the 1st CASE. Holds a state index.    Unevaluated.
CODEVAR:    Variable used in the 2nd CASE. Holds a character code. Unevaluated.
TRANSITION: (state-name (string-expr body-expr...)...) ...
            STATE-NAME:  They are collected and symbol-macrolet'ed with the
                         corresponding state index.
            STRING-EXPR: A string containing the characters firing this
                         transition. Evaluated at macro-expansion time.
            BODY-EXPR:   Actions executed when the transition fires.
"
  (flet ((generate-state-values (transitions)
           (let ((state-value -1))
             (mapcar (lambda (transition) `(,(car transition) ,(incf state-value)))
                     transitions)))
         (generate-state-transition (codevar transition)
           (let* ((others-p    (find 'others transition :key (function car)))
                  (other-codes (and others-p
                                    (make-array +code-limit+ :element-type 'bit
                                                :initial-element 1))))
             (when others-p
               ;; compute other-codes:
               (dolist (ch (apply (function concatenate) 'list
                                  (mapcar (lambda (codes) (eval (car codes)))
                                          (remove 'others transition
                                                  :key (function car)))))
                 (if (< (char-code ch) +code-limit+)
                     (setf (aref other-codes (char-code ch)) 0)
                     (error "The character ~C (~D ~:*#x~X) specified ~
                           in a transition is out of bound (~D)"
                            ch (char-code ch) +code-limit+))))
             ;; the codevar case:
             (let ((base-case
                    `(,(if others-p 'case 'ecase)
                       (the (integer 0 #.(1- +code-limit+)) ,codevar)
                       ,@(mapcar
                          (lambda (codes)
                            `(,(if (eq 'others (car codes))
                                   (loop for i below +code-limit+
                                      when (plusp (aref other-codes i))
                                      collect i)
                                   (map 'list (function char-code)
                                        (eval (car codes))))
                               ,@(cdr codes)))
                          transition))))
               (if others-p
                   `(if (<= +code-limit+ ,codevar)
                        (progn ,@(cdr others-p))
                        ,base-case)
                   base-case)))))
    `(symbol-macrolet ,(generate-state-values transitions)
       (loop
          (ecase ,statevar
            ,@(let ((state-value -1))
                   (mapcar
                    (lambda (transition)
                      `(,(incf state-value)
                         ,(generate-state-transition codevar (cdr transition))))
                    transitions)))))))

                                                                
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Needed at macro-expansion-time:
  (defun ! (&rest args) (apply (function concatenate) 'string args))
  (defparameter letters         "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
  (defparameter digits          "0123456789")
  (defparameter spaces          " ")
  (defparameter ampersand       "&")
  (defparameter apostrophe      "'")
  (defparameter dot             ".")
  (defparameter star            "*")
  (defparameter signs           "+-")
  (defparameter specials        "!#(),/;=?[]^_")
  (defparameter specials-2      "<>" "may take an additionnal '=' character.")
  (defparameter ident-first     (! letters ampersand))
  (defparameter ident-next      (! letters digits)))


(defmethod scan ((self scanner))
  (let* ((buffer      (buffer self))
         (buflen      (length (buffer self)))
         (index       (column self))
         (state       (state self))
         (start       0))
    (macrolet
        ((scan-error (fctrl &rest args)
           `(progn (setf (column self) index)
                   (return-from scan (values (token tok-erreur) start index
                                             (format nil ,fctrl ,@args)))))
         (skip ()
           `(if (< (incf index) buflen)
                (setf code (char-code (aref buffer index)))
                (return-from scan (values (token tok-eol) index index))))
         (start () `(setf start index))
         (advance (token &rest args)
           `(if (< (incf index) buflen)
                (setf code (char-code (aref buffer index)))
                ,(if (eq token :error-on-eof)
                     `(scan-error ,@args)
                     `(produce ,token))))
         (produce (token)   `(progn
                               (setf (column self) index)
                               (return-from scan (values ,token start index))))
         (shift (new-state) `(setf state ,new-state))
         (motcle ()         `(gethash (subseq buffer start index) *motcle-map*))
         (token  (tok)      `(gethash ',tok *token-map*)))
      (when (<= buflen index)
        (return-from scan (values (token tok-eol) buflen buflen)))
      (setf code (char-code (aref buffer index)))
      (case-state
       state code

       (maybe-commentaire               ; must be state 0
        (spaces       (skip))
        (digits       (start) (shift numero-or-nombre))
        (ampersand    (start) (advance :error-on-eof "IDENTIFICATEUR TROP COURT")
                      (shift identificateur-or-motcle))
        (letters      (start) (advance (token tok-identificateur)) ; 1 = ident
                      (shift identificateur-or-motcle))
        (apostrophe   (start) (advance :error-on-eof "CHAINE NON-TERMINEE")
                      (shift litchaine))
        (specials-2   (start) (advance (motcle)) (shift speciaux-2))
        (star         (start) (shift commentaire))
        ((! specials dot signs)
         (start) (advance (motcle)) (produce (motcle)))
        (others     (scan-error "CARACTERE INVALIDE '~C' (~D)"
                                (code-char code) code)))
       
       (not-commentaire                 ; must be state 1
        ;; same as maybe-commentaire, but star goes to specials
        (spaces       (skip))
        (digits       (start) (shift numero-or-nombre))
        (ampersand    (start) (advance :error-on-eof "IDENTIFICATEUR TROP COURT")


                      (shift identificateur-or-motcle))
        (letters      (start) (advance (token tok-identificateur)) ; 1 = ident
                      (shift identificateur-or-motcle))
        (apostrophe   (start) (advance :error-on-eof "CHAINE NON-TERMINEE")
                      (shift litchaine))
        (specials-2   (start) (advance (motcle)) (shift speciaux-2))
        ((! specials dot star signs)
         (start) (advance (motcle)) (produce (motcle)))
        (others     (scan-error "CARACTERE INVALIDE '~C' (~D)"
                                (code-char code) code)))
       
       (in-format                       ; must be state 2
        ;; same as not-commentaire, but ident-first may be formatspec.
        (spaces       (skip))
        (digits       (start) (shift numero-in-format))
        (letters      (start) (incf index)
                      (produce (or (motcle) (token tok-identificateur))))
        (apostrophe   (start) (advance :error-on-eof "CHAINE NON-TERMINEE")
                      (shift litchaine))
        (specials-2   (start) (advance (motcle)) (shift speciaux-2))
        ((! specials dot star signs)
         (start) (advance (motcle)) (produce (motcle)))
        (others     (scan-error "CARACTERE INVALIDE '~C' (~D)"
                                (code-char code) code)))

       (numero-in-format
        (digits (advance (token tok-numero)))
        (others (produce (token tok-numero))))
       
       ;;    (tok-numero         "[0-9]+")
       ;;    (tok-nombre         "[0-9]+\\(\\.[0-9]*\\)?\\(E[-+]?[0-9]+\\)?")
       (numero-or-nombre
        (digits   (advance (token tok-numero)))
        (dot      (advance (token tok-nombre))
                  (shift nombre-mantissa))
        ("E"      (advance :error-on-eof "IL MANQUE L'EXPOSANT APRES 'E'")
                  (shift nombre-exponent))
        ((! spaces specials specials-2 star signs)
         (produce (token tok-numero)))
        (others (scan-error "CARACTERE '~C' (~D) INVALIDE DANS UN NOMBRE '~A'"
                            (code-char code)  code
                            (subseq buffer start (1+ index)))))
       (nombre-mantissa
        (digits   (advance (token tok-nombre)))
        ("E"      (advance :not-eof  "IL MANQUE L'EXPOSANT APRES 'E'")
                  (shift nombre-exponent))
        ((! spaces specials specials-2 dot star signs)
         (produce (token tok-nombre)))
        (others (scan-error "CARACTERE '~C' (~D) INVALIDE DANS UN NOMBRE '~A'"
                            (code-char code)  code
                            (subseq buffer start (1+ index)))))
       (nombre-exponent
        (signs     (advance :not-eof "IL MANQUE DES CHIFFRES DANS L'EXPOSANT")
                   (shift nombre-signed-exponent))
        (digits    (shift nombre-signed-exponent))
        (others (scan-error "CARACTERE '~C' (~D) INVALIDE DANS UN NOMBRE '~A'"
                             (code-char code) code
                            (subseq buffer start (1+ index)))))
       (nombre-signed-exponent
        (digits    (advance (token tok-nombre)))
        ((! spaces specials specials-2 dot star signs)
         (produce (token tok-nombre)))
        (others (scan-error "CARACTERE '~C' (~D) INVALIDE DANS UN NOMBRE '~A'"
                            (code-char code)  code
                            (subseq buffer start (1+ index)))))
       
       ;;    (tok-identificateur "&?[A-Z][0-9A-Z]?[0-9A-Z]?[0-9A-Z]?[0-9A-Z]?")
       (identificateur-or-motcle
        (ident-next (advance (or (motcle) (token tok-identificateur))))
        (others     (produce (or (and (= (1+ start) index)
                                      (token tok-identificateur))
                                 ;; (skip 1-letter format specifiers).
                                 (motcle) (token tok-identificateur)))))
;;;        (identificateur-or-motcle-or-formatspec
;;;         (ident-next (advance (or (motcle) (token tok-identificateur))))
;;;         (others     (produce (or (motcle) (token tok-identificateur)))))

       ;;    (Tok-Litchaine      "\\('[^']*'\\)\\('[^']*'\\)*")
       (Litchaine
        (apostrophe
         (if (and (< (1+ index) buflen)
                  (char= (character apostrophe) (aref buffer (1+ index))))
             (progn (advance :error-on-eof "PAS POSSIBLE")
                    (advance :error-on-eof "CHAINE NON-TERMINEE"))
             (progn (incf index) (produce (token tok-litchaine)))))
        (others 
         (advance :error-on-eof "CHAINE NON-TERMINEE")))

       (speciaux-2
        ("="    (advance (motcle)) (produce (motcle)))
        (others                    (produce (motcle))))

       ;;    (tok-commentaire    "\\*.*$")
       (commentaire
        (star  (setf index buflen) (produce (token tok-commentaire))))   ))))


(defmethod print-error ((self scanner) start end message &rest args)
  (format *error-output*
    "~&~A~%~VA~A~%~:[~:*~A:~D:~D: ~;~2*~]~A~%"
    (buffer self) start "" (make-string (- end start)
                                        :initial-element (character "^"))
    (ignore-errors (pathname (source self)))
    (line self) (column self)
    (apply (function format) nil message args)))


(defmethod advance-token ((self scanner))
  "RETURN: The new current token, old next token"
  (cond
    ((eofp (token self)))               ; End of File -- don't move
    ((eolp (token self))   (advance-line self) (advance-token self))
    (t
     (setf (token self) 
           (multiple-value-bind (tok-desc start end errstr) (scan self)
             (macrolet ((tcode (x) `(first  ,x))
                        (tsymb (x) `(second ,x))
                        (ttext (x) `(third  ,x))
                        (tmatf (x) `(fourth ,x)))
               (if (eq (tsymb tok-desc) 'tok-erreur)
                   (progn
                     (print-error self start end errorstr)
                     (incf (column self))
                     (make-instance 'erreur
                       :kind   'tok-erreur
                       :index  (second (gethash 'tok-erreur *token-map*))
                       :text   errstr
                       :column start
                       :line   (line self)))
                   (make-instance  (case (tsymb tok-desc)
                                     (tok-identificateur 'identificateur)
                                     (tok-numero         'numero)
                                     (tok-nombre         'nombre)
                                     (tok-litchaine      'chaine)
                                     (tok-commentaire    'commentaire)
                                     (otherwise          'motcle))
                     :kind   (tsymb tok-desc)
                     :index  (tcode tok-desc)
                     :text   (case (tsymb tok-desc)
                                      ((tok-numero tok-nombre tok-identificateur
                                                   tok-litchaine tok-commentaire)
                                       (make-array  (- end start)
                                                    :element-type 'character
                                                    :displaced-to (buffer self)
                                                    :displaced-index-offset start))
                                      (otherwise (ttext tok-desc)))
                     :column start
                     :line   (line self))))))))
  (token self))


(defmethod advance-line ((self scanner))
  "RETURN: The new current token, old next token"
  (cond
    ((eofp (token self)))               ; End of File -- don't move.
    ((setf (buffer self) (read-line (source self) nil nil))
                                        ; got a line -- advance a token.
     (setf (column self) 0
           (state  self) +maybe-commentaire+)
     (incf (line   self))
     (setf (token self) nil))
    (t                                  ; Just got EOF
     (setf (token self) (make-eof self))))
  (token self))


(defun possible-token-list (expected lexicon)
  (map 'list (lambda (action) (svref lexicon (car action))) expected))


(defun possible-token-p (token expected)
  (find (token-index token) expected :key (function car)))


(defmethod get-next-token-function ((self scanner))
  (let ((last-token 'tok-eol)
        (lexicon (zebu::grammar-lexicon (find-grammar "LSE"))))
    (lambda (actionv)
      ;;(print `(actionv = ,actionv))
      ;;(print `(lexicon = ,lexicon))
      ;;(print `(last-token = ,last-token))
      ;;(print (possible-token-list actionv lexicon))
      (let ((token (advance-token self)))
        (unless (possible-token-p token actionv)
          (print-error self
                       (token-column token)
                       (+ (token-column token)(length (token-text token)))
                       "LEXEME ILLEGAL ~A; ~%LEXEMES POSSIBLES: ~A~%"
                       token
                       (possible-token-list actionv lexicon))
          (error "ERREUR DE SYNTAXE"))
        (case  (token-kind token)
          (tok-numero
           (when (eq 'tok-eol last-token)
             (setf (state self) +maybe-commentaire+)))
          (tok-ptvirg
           (setf (state self) +maybe-commentaire+))
          (tok-crogauche
           (if (or (eq 'tok-afficher last-token)
                   (eq 'tok-affic    last-token))
               (setf (state self) +in-format+)
               (setf (state self) +not-commentaire+)))
          (tok-crodroite
           (setf (state self) +not-commentaire+))
          (otherwise
           (when (eql (state self) +maybe-commentaire+)
             (setf (state self) +not-commentaire+))))
        (setf last-token (token-kind (token self)))
        ;;(format t "~&next-token --> (~A  ~A)~%"
        ;;        token (token-index token))
        (values token (token-index token))))))


(defun test-scan-file (path)
  (with-open-file (src path)
    (terpri)
    (let ((scanner (make-instance 'scanner :source src)))
      (advance-token scanner)
      (loop until (eofp (token scanner)) do
           (when (typep (token scanner) 'numero)
             (princ (state scanner)) (princ " ")
             (princ (token scanner)) (terpri)
             (setf (state scanner) +maybe-commentaire+)
             (advance-token scanner))
           (loop
              with last-token = nil
              until (eolp (token scanner)) do
              (case  (token-kind (token scanner))
                (tok-ptvirg
                 (setf (state scanner) +maybe-commentaire+))
                (tok-crogauche
                 (if (or (eq 'tok-afficher last-token)
                         (eq 'tok-affic last-token))
                     (setf (state scanner) +in-format+)
                     (setf (state scanner) +not-commentaire+)))
                (tok-crodroite
                 (setf (state scanner) +not-commentaire+))
                (otherwise
                 (when (eql (state scanner) +maybe-commentaire+)
                   (setf (state scanner) +not-commentaire+))))
              (setf last-token (token-kind (token scanner)))
              (format t "~&~A ~A~%" (state scanner) (token scanner))
              (advance-token scanner)
              finally (format t "~&~A ~A~%" (state scanner) (token scanner)))
           (advance-line scanner) (advance-token scanner)
         finally (format t "~&~A ~A~%" (state scanner) (token scanner))))))



(defun test-parse-file (path)
  (with-open-file (src path)
    (terpri)
    (let* ((scanner    (make-instance 'scanner :source src))
           (next-token (get-next-token-function scanner)))
      (loop do
           (advance-line scanner)
           (when (eofp (token scanner)) (loop-finish))
           (format t "~2%~A~%" (buffer scanner))
           (restart-case
               ;; stuff
               (format t "~&~S~%" (lr-parse next-token
                                            (lambda (msg) (error "ERREUR: ~A" msg))
                                            (find-grammar "LSE")))
             (advance-line ()
               :report "PASSER A LA LIGNE SUIVANTE"))))))




