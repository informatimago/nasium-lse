;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               lse-scanner.lisp
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
;;;;    A scanner for LSE.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-21 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2013
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


;;----------------------------------------------------------------------
;; tokens
;;----------------------------------------------------------------------

(defclass lse-token (token)
  ())


(defun clean-regexp (reg)
  (if (position #\\ reg)
      (with-output-to-string (out)
        (loop
          :with i = 0
          :while (< i (length reg))
          :do (let ((ch (aref reg i)))
                (incf i)
                (case ch
                  (#\\       (when (< i (length reg))
                               (princ (aref reg i) out)
                               (incf i)))
                  (otherwise (princ ch out))))))
      reg))

(defun token-kind-label (kind)
  (case kind
    (tok-litchaine      "LITERAL CHAINE")
    (tok-commentaire    "COMMENTAIRE")
    (tok-identificateur "IDENTIFICATEUR")
    (tok-procident      "IDENTIFICATEUR DE PROCEDURE")
    (tok-motcle         "MOT CLE")
    (tok-nombre         "NOMBRE")
    (tok-numero         "NUMERO")
    (otherwise
     (let* ((grammar (grammar-named 'lse))
            (entry   (assoc kind (grammar-terminals grammar))))
       (cond
         (entry
          (clean-regexp (second entry)))
         ((and (< 4 (length (string kind)))
               (string= "TOK-" kind :end2 4))
          (subseq (string kind) 4))
         (t
          kind))))))


(defmethod print-object ((self lse-token) stream)
  (if *print-escape*
      (print-unreadable-object (self stream :type t :identity t)
        (format stream "~@[~D:~]~@[~D:~] ~:[~A ~S~;~S~*~]"
                (and (slot-boundp self 'com.informatimago.common-lisp.parser.scanner::line)
                     (token-line self))
                (and (slot-boundp self 'com.informatimago.common-lisp.parser.scanner::column)
                     (token-column self))
                (string= (token-kind-label (token-kind self))
                         (token-text self))
                (token-kind-label (token-kind self))
                (token-text self)))
      (format stream "#<~A ~S>"
              (token-kind-label (token-kind self))
              (token-text self)))
  self)


(defclass tok-commentaire    (lse-token) ())
(defclass tok-motcle         (lse-token) ())


(defclass tok-identificateur (lse-token)
  ((nom        :accessor identificateur-nom
               :initarg :nom
               :initform nil
               :type     symbol)))

(defmethod initialize-instance :after ((self tok-identificateur) &rest args)
  (declare (ignore args))
  (setf (identificateur-nom self)
        (intern (token-text self) (find-package "COM.INFORMATIMAGO.LSE.IDENTIFIERS")))
  self)


(defclass tok-procident (lse-token)
  ((nom        :accessor procident-nom
               :accessor identificateur-nom
               :initarg :nom
               :initform nil
               :type     symbol)))

(defmethod initialize-instance :after ((self tok-procident) &rest args)
  (declare (ignore args))
  (setf (procident-nom self)
        (intern (token-text self) (find-package "COM.INFORMATIMAGO.LSE.IDENTIFIERS")))
  self)



(defclass tok-numero (lse-token)
  ((valeur     :accessor numero-valeur
               :initarg :valeur
               :initform 0
               :type     (integer 0))))

(defmethod initialize-instance :after ((self tok-numero) &rest args)
  (declare (ignore args))
  (setf (numero-valeur self) (parse-integer (token-text self)))
  self)





(defclass tok-nombre (lse-token)
  ((valeur     :accessor nombre-valeur
               :initarg :valeur
               :initform 0.0
               :type     single-float)))

(defmethod initialize-instance :after ((self tok-nombre) &rest args)
  (declare (ignore args))
  (setf (nombre-valeur self) (read-from-string (token-text self)))
  self)




(defclass tok-litchaine (lse-token)
  ((valeur     :accessor chaine-valeur
               :initarg :value
               :initform ""
               :type     string)))

(defmethod initialize-instance :after ((self tok-litchaine) &rest args)
  (declare (ignore args))
  (setf (chaine-valeur self)
        (loop
           :with literal = (token-text self)
           :with result  = (make-array (length literal)
                                       :element-type 'character
                                       :fill-pointer 0)
           :with i = 1
           :while (< (1+ i) (length literal))
           :do
           (vector-push (aref literal i) result)
           (incf i (if (and (char= (character "'") (aref literal i))
                            (char= (character "'") (aref literal (1+ i))))
                       2 1))
           :finally (return result)))
  self)




(defclass tok-eol (lse-token)
  ())

(defmethod make-eol ((self scanner))
  (make-instance 'tok-eol
      :kind    'tok-eol
      :text    "<END OF LINE>"
      :column  (scanner-column self)
      :line    (scanner-line   self)))

(defgeneric eolp (token)
  (:documentation "Returns whether the token is an end-of-line token")
  (:method ((token t))       nil)
  (:method ((token tok-eol)) t))




(defclass tok-eof (lse-token)
  ())

(defmethod make-eof ((self scanner))
  (make-instance 'tok-eof
      :kind    'tok-eof
      :text    "<END OF FILE>"
      :column  (scanner-column self)
      :line    (scanner-line   self)))

(defgeneric eofp (token)
  (:documentation "Returns whether the token is an end-of-file token")
  (:method ((token t))       nil)
  (:method ((token tok-eof)) t)
  (:method ((token tok-eol)) t))




;;----------------------------------------------------------------------
;; tokens
;;----------------------------------------------------------------------

(define-condition lse-scanner-error (lse-error scanner-error)
  ((buffer
    :initarg :buffer
    :accessor scanner-error-buffer
    :type     (or null string)
    :initform nil))
  (:report print-scanner-error))


(defmethod initialize-instance :after ((self lse-scanner-error) &key &allow-other-keys)
  (setf (slot-value self 'line-number) (scanner-error-line self)))



(define-condition lse-source-error (lse-error)
  ((buffer
    :initarg :buffer
    :accessor lse-source-error-buffer
    :type     (or null string)
    :initform nil)))



(define-condition lse-scanner-error (lse-source-error scanner-error)
  ()
  (:report print-scanner-error))

(defmethod initialize-instance :after ((self lse-scanner-error) &key &allow-other-keys)
  (setf (slot-value self 'line-number) (scanner-error-line self)))

(defmethod print-scanner-error ((err lse-scanner-error) stream)
  (let ((token-length (length (token-text (scanner-error-current-token err))))
        (*print-pretty* nil))
    (format stream
            "~:[~*COLONNE ~D : ~;~:*~A:~D:~D: ~]~?~%~A~%~V@A~A"
            (let ((source (scanner-source (scanner-error-scanner err))))
              (unless (stringp source) (ignore-errors (pathname source))))
            (scanner-error-line err) (scanner-error-column err)

            (scanner-error-format-control err)
            (scanner-error-format-arguments err)
            
            (lse-source-error-buffer err)
            (1- (scanner-error-column err)) ""
            (make-string token-length :initial-element (character "^")))))

(define-condition lse-scanner-error-invalid-character (lse-scanner-error)
  ((invalid-character :initarg :invalid-character :initform nil :reader scanner-error-invalid-character)))


(define-condition lse-parser-error (lse-source-error parser-error)
  ()
  (:report print-parser-error))

(defmethod initialize-instance :after ((self lse-parser-error) &key &allow-other-keys)
  (setf (slot-value self 'line-number) (parser-error-line self)
        (slot-value self 'buffer) (scanner-buffer (parser-error-scanner self))))

(defmethod print-parser-error ((err lse-parser-error) stream)
  (let ((*print-pretty* nil))
    (format stream
            "~:[~*COLONNE ~D : ~;~:*~A:~D:~D: ~]~?~%~:[~;~:*~A~%~V@A~A~]"
            (let ((source (scanner-source (parser-error-scanner err))))
              (unless (stringp source) (ignore-errors (pathname source))))
            (parser-error-line err) (parser-error-column err)

            (parser-error-format-control err)
            (parser-error-format-arguments err)
            
            (lse-source-error-buffer err)
            (1- (parser-error-column err)) ""
            (character "^"))))

(define-condition lse-parser-error-end-of-source-not-reached (lse-parser-error)
  ())

(define-condition lse-parser-error-unexpected-token (lse-parser-error)
  ((expected-token :initarg :expected-tokken
                   :initform nil
                   :reader parser-error-expected-token)))




(defclass lse-scanner (scanner)
  ((buffer
    :accessor scanner-buffer
    :type     (or null string)
    :initform nil)
   (previous-token-kind
    :accessor scanner-previous-token-kind
    :initform 'tok-eol
    :documentation "The token kind of the previous token."))
  (:DOCUMENTATION "A scanner for L.S.E."))

(defvar *scanner* nil
  "The current scanner.")

;; (defmethod scanner-current-token ((scanner lse-scanner))
;;   (slot-value scanner 'com.informatimago.common-lisp.parser.scanner::current-token))
;; (defmethod scanner-current-text ((scanner lse-scanner))
;;   (token-text (scanner-current-token scanner)))

(defmethod scanner-end-of-source-p ((scanner lse-scanner))
  (eofp (scanner-current-token scanner)))

(defmethod word-equal ((token lse-token) (kind symbol))
  (eql (token-kind token) kind))
(defmethod word-equal ((kind symbol) (token lse-token))
  (eql (token-kind token) kind))

(defmethod accept ((scanner lse-scanner) token)
  (if (word-equal token (scanner-current-token scanner))
      (prog1 (scanner-current-token scanner)
        ;; (list (token-kind (scanner-current-token scanner))
        ;;       (scanner-current-text scanner)
        ;;       (scanner-column scanner))
        (scan-next-token scanner))
      (error 'lse-parser-error-unexpected-token
             :line   (scanner-line scanner)
             :column (token-column (scanner-current-token scanner))
             :grammar (grammar-named 'lse)
             :scanner scanner
             :non-terminal-stack (copy-list *non-terminal-stack*)
             :expected-token token
             :format-control
             #+developing "ATTENDU ~S, PAS ~:[~A ~S~;FIN DE LIGNE~2*~]~%PILE NON TERMINAUX: ~A~%PRODUCTION: ~{~A --> ~A~}"
             #-developing "ATTENDU ~S, PAS ~:[~A ~S~;FIN DE LIGNE~2*~]"
             :format-arguments
             (list
              (token-kind-label (token-kind token))
              (or (eolp (scanner-current-token scanner))
                  (eofp (scanner-current-token scanner)))
              (token-kind-label (token-kind (scanner-current-token scanner)))
              (token-text (scanner-current-token scanner))
              *non-terminal-stack*
              (clean-up-rule (assoc (first *non-terminal-stack*)
                                     (grammar-rules (grammar-named 'lse))))))))


(defun clean-up-rule (rule)
  (labels ((clean-up-rhs (rhs)
             (if (atom rhs)
                 rhs
                 (ecase (first rhs)
                   ((seq)
                    (if (null (rest (second rhs)))
                        (clean-up-rhs (first (second rhs)))
                        (cons 'seq
                              (mapcar (function clean-up-rhs)
                                      (second rhs)))))
                   ((rep)
                    (cons 'rep
                          (mapcar (function clean-up-rhs)
                                  (second rhs))))
                   ((opt)
                    (cons 'opt
                          (mapcar (function clean-up-rhs)
                                  (second rhs))))
                   ((alt)
                    (if (null (rest (second rhs)))
                        (clean-up-rhs (first (second rhs)))
                        (cons 'alt
                              (mapcar (function clean-up-rhs)
                                      (second rhs)))))))))
    (list (first rule) (clean-up-rhs (second rule)))))


(defmethod initialize-instance :after ((self lse-scanner) &rest args)
  (declare (ignore args))
  (setf (scanner-current-token self) (make-eol self))
  self)

(defmethod print-object ((self lse-scanner) out)
  (print-parseable-object (self out :type t :identity t)
                          line column current-token source buffer previous-token-kind))

;; (macroexpand '(print-parseable-object (self out :type t :identity t)
;;                           (:line          (scanner-line          self))
;;                           (:column        (scanner-column        self))
;;                           (:current-token (scanner-current-token self))
;;                           (:source        (scanner-source        self))
;;                           (:buffer        (scanner-buffer        self))
;;                           (:previous-token-kind (scanner-previous-token-kind self))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; (defconstant +code-limit+ 128 "Number of characters in the LSE charset") ; ASCII
  (defconstant +code-limit+ #x110000 "Number of characters in the LSE charset") ; Unicode
  );;eval-when

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
                     (error  "The character ~C (~D ~:*#x~X) specified ~
                                             in a transition is out of bound (~D)"
                             ch (char-code ch) +code-limit+))))
             ;; the codevar case:
             (let ((base-case
                    `(,(if others-p 'case 'ecase)
                       (the (integer 0 #.(1- +code-limit+)) ,codevar)
                       ,@(mapcar
                          (lambda (codes)
                            `(,(if (eq 'others (car codes))
                                   #-(and)
                                   (loop
                                     :for i :below +code-limit+
                                      :when (plusp (aref other-codes i))
                                      :collect i)
                                   'otherwise
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
         #+lse-scanner-debug (print (list 'state '= ,statevar 'code '= ,codevar (format nil "~S" (code-char ,codevar))))
         #+lse-scanner-debug (finish-output)
         (ecase ,statevar
           ,@(let ((state-value -1))
                  (mapcar
                   (lambda (transition)
                     `(,(incf state-value)
                        ,(generate-state-transition codevar (cdr transition))))
                   transitions)))))))





(defparameter *tokens*
  '((tok-AFFICHER  . "AFFICHER")
    (tok-ALLER     . "ALLER")
    (tok-ALORS     . "ALORS")
    (tok-CHAINE    . "CHAINE")
    (tok-CHARGER   . "CHARGER")
    (tok-DEBUT     . "DEBUT")
    (tok-EN        . "EN")
    (tok-ET        . "ET")
    (tok-EXECUTER  . "EXECUTER")
    (tok-FIN       . "FIN")
    (tok-FAIRE     . "FAIRE")
    (tok-GARER     . "GARER")
    (tok-JUSQUA    . "JUSQUA")
    (tok-LIBERER   . "LIBERER")
    (tok-LIRE      . "LIRE")
    (tok-LOCAL     . "LOCAL")
    (tok-NON       . "NON")
    (tok-OU        . "OU")
    (tok-PAS       . "PAS")
    (tok-PAUSE     . "PAUSE")
    (tok-POUR      . "POUR")
    (tok-PROCEDURE . "PROCEDURE")
    (tok-QUE       . "QUE")
    (tok-RESULTAT  . "RESULTAT")
    (tok-RETOUR    . "RETOUR")
    (tok-SI        . "SI")
    (tok-SINON     . "SINON")
    (tok-SUPPRIMER . "SUPPRIMER")
    (tok-TABLEAU   . "TABLEAU")
    (tok-TANT      . "TANT")
    (tok-TERMINER  . "TERMINER")
    ;; caractères spéciaux
    (tok-GT        . ">")
    (tok-GE        . ">=")
    (tok-LT        . "<")
    (tok-LE        . "<=")
    (tok-NE        . "#")
    (tok-EQ        . "=")
    (tok-affic     . "?")
    (tok-plus      . "+")
    (tok-moins     . "-")
    (tok-concat    . "!")
    (tok-fois      . "*")
    (tok-divise    . "/")
    (tok-puissance . "^")
    (tok-puissance . "￪")
    (tok-puissance . "↑")
    (tok-virgule   . ",")
    (tok-point     . ".")
    (tok-pargauche . "(")
    (tok-pardroite . ")")
    (tok-crogauche . "[")
    (tok-crodroite . "]")
    (tok-fleche    . "_")
    (tok-fleche    . "￩")
    (tok-fleche    . "←")
    (tok-ptvirg    . ";")
    (tok-at        . "@")))


(defparameter *tokens+format-specifiers*
  (append '( ;; keyword or identifier:            
            (tok-x         . "X")
            (tok-C         . "C")
            (tok-L         . "L")
            (tok-f         . "F")
            (tok-e         . "E")
            (tok-u         . "U"))
          *tokens*))


(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Needed at macro-expansion-time:
  (defun concat (&rest args) (apply (function concatenate) 'string args))
  (defparameter letters
    #-LSE-CASE-INSENSITIVE "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    #+LSE-CASE-INSENSITIVE "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
  (defparameter exponent
    #-LSE-CASE-INSENSITIVE "E"
    #+LSE-CASE-INSENSITIVE "Ee")
  (defparameter digits             "0123456789")
  (defparameter spaces             " ")
  (defparameter ampersand          "&")
  (defparameter at                 "@")
  (defparameter apostrophe         "'")
  (defparameter dot                ".")
  (defparameter star               "*")
  (defparameter signs              "+-")
  (defparameter specials           "!#(),/;=?[]^_←↑￩ ￪")
  (defparameter specials-2         "<>" "may take an additionnal '=' character.")
  (defparameter ident-first         letters)
  (defparameter ident-next          (concat letters digits))
  (defparameter procident-first     ampersand)
  (defparameter procident-second    letters)
  (defparameter procident-rest      (concat letters digits)))


(defconstant +maybe-commentaire+ 0 "LSE Scanner State")
(defconstant +not-commentaire+   1 "LSE Scanner State")
(defconstant +in-format+         2 "LSE Scanner State")



(defmethod scan-lse-token ((scanner lse-scanner))
  (let* ((buffer      (scanner-buffer scanner))
         (buflen      (length (scanner-buffer scanner)))
         (index       (scanner-column scanner))
         (state       (scanner-state scanner))
         (start       0)
         (code        0))
    (labels ((scan-error (fctrl &rest args)
               #+lse-scanner-debug (print `(scan-error ,fctrl ,@args))
               #+lse-scanner-debug (finish-output)
               (error 'lse-scanner-error
                      :line    (scanner-line scanner)
                      :column  (setf (scanner-column scanner) index)
                      :state   (setf (scanner-state  scanner) state)
                      :current-token (scanner-current-token scanner)
                      :scanner scanner
                      :format-control fctrl
                      :format-arguments args
                      :buffer (copy-seq (scanner-buffer scanner))))
             (invalid-character (ch &optional (format-control "") &rest format-arguments)
               #+lse-scanner-debug (print `(invalid-character ,ch ,format-control ,@format-arguments))
               #+lse-scanner-debug (finish-output)
               (error 'lse-scanner-error-invalid-character
                      :line    (scanner-line scanner)
                      :column  (setf (scanner-column scanner) index)
                      :state   (setf (scanner-state  scanner) state)
                      :current-token (scanner-current-token scanner)
                      :scanner scanner
                      :format-control "CARACTÈRE INVALIDE ~A~:[~*~; (~D)~]~?"
                      :format-arguments (list (if (<= 32 (char-code ch))
                                                  (format nil "'~C'" ch)
                                                  (format nil ".~A." (char-code ch)))
                                              (<= 32 (char-code ch))
                                              code
                                              format-control format-arguments)
                      :buffer (copy-seq (scanner-buffer scanner))))
             (skip ()
               #+lse-scanner-debug (print `(skip))
               #+lse-scanner-debug (finish-output)
               (if (< (incf index) buflen)
                   (setf code (char-code (aref buffer index)))
                   (return-from scan-lse-token
                     (setf (scanner-state scanner) state
                           (scanner-column scanner) index
                           (scanner-current-token scanner) (make-eol scanner)))))
             (start ()
               #+lse-scanner-debug (print `(start))
               #+lse-scanner-debug (finish-output)
               (setf start index))
             (produce (token)
               #+lse-scanner-debug (print `(produce ,token))
               #+lse-scanner-debug (finish-output)
               (return-from scan-lse-token
                 (setf (scanner-state scanner) state
                       (scanner-column scanner) index
                       (scanner-current-token scanner) token)))
             (shift (new-state)
               #+lse-scanner-debug (print `(shift ,new-state))
               #+lse-scanner-debug (finish-output)
               (setf state new-state))
             (motcle ()
               #+lse-scanner-debug (print `(motcle))
               #+lse-scanner-debug (finish-output)
               (let* ((text (subseq buffer start index))
                      (entry (rassoc text (if (= state +in-format+)
                                              *tokens+format-specifiers*
                                              *tokens*)
                                     :test (if (task-case-insensitive *task*)
                                               (function string-equal)
                                               (function string=)))))
                 (when entry
                   #+lse-scanner-debug (print `(motcle ,(car entry) ,text))
                   #+lse-scanner-debug (finish-output)
                   (make-instance 'tok-motcle
                       :kind (car entry)
                       :text text
                       :line (scanner-line scanner)
                       :column (scanner-column scanner)))))
             (token (tok)
               #+lse-scanner-debug (print `(tok ,tok ,(if (member tok '(tok-litchaine tok-commentaire)) 
                                                          (subseq buffer start index)
                                                          (if (task-case-insensitive *task*)
                                                              (string-upcase (subseq buffer start index))
                                                              (subseq buffer start index)))))
               #+lse-scanner-debug (finish-output)
               (make-instance tok
                   :kind tok
                   :text (if (member tok '(tok-litchaine tok-commentaire)) 
                             (subseq buffer start index)
                             (if (task-case-insensitive *task*)
                                 (string-upcase (subseq buffer start index))
                                 (subseq buffer start index)))
                   :line (scanner-line scanner)
                   :column start)))
      (macrolet ((advance (token &rest args)
                   #+lse-scanner-debug `(progn (print `(advance ,',token ,',args))
                                              (finish-output)
                                              (if (< (incf index) buflen)
                                                   (setf code (char-code (aref buffer index)))
                                                   ,(if (eq token :error-on-eof)
                                                        `(scan-error ,@args)
                                                        `(produce ,token))))
                   #-lse-scanner-debug
                   `(if (< (incf index) buflen)
                        (setf code (char-code (aref buffer index)))
                        ,(if (eq token :error-on-eof)
                             `(scan-error ,@args)
                             `(produce ,token)))))
       (when (<= buflen index)
         (return-from scan-lse-token
           (setf (scanner-state scanner) state
                 (scanner-column scanner) buflen
                 (scanner-current-token scanner) (make-eol scanner))))
       (setf code (char-code (aref buffer index)))
       (case-state
        state code

        (maybe-commentaire              ; must be state 0
         (spaces       (skip))
         (digits       (start) (shift numero-or-nombre))
         (at           (start) (shift not-commentaire) (advance (motcle)) (produce (motcle)))
         (ampersand    (start) (advance :error-on-eof "IDENTIFICATEUR TROP COURT")
                       (shift procident))
         (letters      (start) (advance (token 'tok-identificateur)) ; 1 = ident
                       (shift identificateur-or-motcle))
         (apostrophe   (start) (advance :error-on-eof "CHAINE NON-TERMINEE")
                       (shift litchaine))
         (specials-2   (start) (advance (motcle)) (shift speciaux-2))
         (star         (start) (shift commentaire))
         ((concat specials dot signs)
          (start) (advance (motcle)) (produce (motcle)))
         (others       (invalid-character (code-char code))))
       
        (not-commentaire                ; must be state 1
         ;; same as maybe-commentaire, but star goes to specials
         (spaces       (skip))
         (digits       (start) (shift nombre))
         (at           (start) (advance (motcle)) (produce (motcle)))
         (ampersand    (start) (advance :error-on-eof "IDENTIFICATEUR TROP COURT")
                       (shift procident))
         (letters      (start) (advance (token 'tok-identificateur)) ; 1 = ident
                       (shift identificateur-or-motcle))
         (apostrophe   (start) (advance :error-on-eof "CHAINE NON-TERMINEE")
                       (shift litchaine))
         (specials-2   (start) (advance (motcle)) (shift speciaux-2))
         ((concat specials dot star signs)
          (start) (advance (motcle)) (produce (motcle)))
         (others       (invalid-character (code-char code))))
       
        (in-format                      ; must be state 2
         ;; same as not-commentaire, but ident-first may be formatspec.
         (spaces       (skip))
         (digits       (start) (shift numero-in-format))
         (letters      (start) (incf index)
                       (produce (or (motcle) (token 'tok-identificateur))))
         (apostrophe   (start) (advance :error-on-eof "CHAINE NON-TERMINEE")
                       (shift litchaine/in-format))
         ;; (specials-2   (start) (advance (motcle)) (shift speciaux-2))
         ((concat specials dot star signs)
          (start) (advance (motcle)) (produce (motcle)))
         (others     (invalid-character (code-char code))))

        (numero-in-format               ; 3
         (digits (advance (token 'tok-numero)))
         (others (shift in-format) (produce (token 'tok-numero))))
       
        ;;    (tok-numero         "[0-9]+")
        ;;    (tok-nombre         "[0-9]+\\(\\.[0-9]*\\)?\\(E[-+]?[0-9]+\\)?")
        (numero-or-nombre               ; 4
         (digits   (advance (token 'tok-numero)))
         (dot      (advance (token 'tok-nombre))
                   (shift nombre-mantissa))
         (exponent (advance :error-on-eof "IL MANQUE L'EXPOSANT APRES 'E'")
                   (shift nombre-exponent))
         ((concat spaces star)
          (shift maybe-commentaire) (produce (token 'tok-numero)))
         ((concat specials specials-2 signs)
          (shift not-commentaire) (produce (token 'tok-numero)))
         (others (invalid-character (code-char code)
                                    "INVALIDE DANS UN NOMBRE '~A'"
                                    (subseq buffer start (1+ index)))))

        (nombre                         ; 5
         (digits   (advance (token 'tok-numero)))
         (dot      (advance (token 'tok-nombre))
                   (shift nombre-mantissa))
         (exponent (advance :error-on-eof "IL MANQUE L'EXPOSANT APRES 'E'")
                   (shift nombre-exponent))
         ((concat spaces star specials specials-2 signs)
          (shift not-commentaire) (produce (token 'tok-numero)))
         (others   (invalid-character (code-char code)
                                      "INVALIDE DANS UN NOMBRE '~A'"
                                      (subseq buffer start (1+ index)))))
       
        (nombre-mantissa                ; 6
         (digits   (advance (token 'tok-nombre)))
         (exponent (advance :not-eof  "IL MANQUE L'EXPOSANT APRES 'E'")
                   (shift nombre-exponent))
         ((concat spaces specials specials-2 dot star signs)
          (shift not-commentaire) (produce (token 'tok-nombre)))
         (others   (invalid-character (code-char code)
                                      "INVALIDE DANS UN NOMBRE '~A'"
                                      (subseq buffer start (1+ index)))))
        (nombre-exponent                ; 7
         (signs     (advance :not-eof "IL MANQUE DES CHIFFRES DANS L'EXPOSANT")
                    (shift nombre-signed-exponent))
         (digits    (shift nombre-signed-exponent))
         (others    (invalid-character (code-char code)
                                       "INVALIDE DANS UN NOMBRE '~A'"
                                       (subseq buffer start (1+ index)))))
        (nombre-signed-exponent         ; 8
         (digits    (advance (token 'tok-nombre)))
         ((concat spaces specials specials-2 dot star signs)
          (shift not-commentaire) (produce (token 'tok-nombre)))
         (others    (invalid-character (code-char code)
                                       "INVALIDE DANS UN NOMBRE '~A'"
                                       (subseq buffer start (1+ index)))))
       
        ;;    (tok-identificateur "&?[A-Z][0-9A-Z]?[0-9A-Z]?[0-9A-Z]?[0-9A-Z]?")
        (identificateur-or-motcle       ; 9
         (ident-next (advance (or (motcle) (token 'tok-identificateur))))
         (others     (shift not-commentaire)
                     (produce (or (and (= (1+ start) index)
                                       (token 'tok-identificateur))
                                  ;; (skip 1-letter format specifiers).
                                  (motcle)
                                  (token 'tok-identificateur)))))

;;;        (identificateur-or-motcle-or-formatspec
;;;         (ident-next (advance (or (motcle) (token 'tok-identificateur))))
;;;         (others     (produce (or (motcle) (token 'tok-identificateur)))))


        (procident                      ; 10
         (procident-second (shift procident/rest) (advance (token 'tok-procident)))
         (others           (invalid-character (code-char code) "INVALIDE APRES '&', UN IDENTIFICATEUR DE PROCEDURE DOIT AVOIR AU MOINS UNE LETTRE.")))
        (procident/rest                 ; 11
         (procident-rest   (advance (token 'tok-procident)))
         (others           (shift not-commentaire)
                           (produce (token 'tok-procident))))


        ;;    (tok-litchaine      "\\('[^']*'\\)\\('[^']*'\\)*")
        (litchaine                      ; 12
         (apostrophe
          (if (and (< (1+ index) buflen)
                   (char= (character apostrophe) (aref buffer (1+ index))))
              (progn (advance :error-on-eof "PAS POSSIBLE")
                     (advance :error-on-eof "CHAINE NON-TERMINEE"))
              (progn (incf index)
                     (shift not-commentaire)
                     (produce (token 'tok-litchaine)))))
         (others 
          (advance :error-on-eof "CHAINE NON-TERMINEE")))

        (litchaine/in-format            ; 13
         (apostrophe
          (if (and (< (1+ index) buflen)
                   (char= (character apostrophe) (aref buffer (1+ index))))
              (progn (advance :error-on-eof "PAS POSSIBLE")
                     (advance :error-on-eof "CHAINE NON-TERMINEE"))
              (progn (incf index)
                     (shift in-format)
                     (produce (token 'tok-litchaine)))))
         (others                        ; 14
          (advance :error-on-eof "CHAINE NON-TERMINEE")))

        (speciaux-2                     ; 15
         ("="    (advance (motcle)) (shift not-commentaire) (produce (motcle)))
         (others                    (shift not-commentaire) (produce (motcle))))

        ;;    (tok-commentaire    "\\*.*$")
        (commentaire                    ; 16
         (star  (setf index buflen) (shift maybe-commentaire) (produce (token 'tok-commentaire)))))))))



(defmethod advance-line ((scanner lse-scanner))
  "RETURN: The new current token, old next token"
  (cond
    ((typep (scanner-current-token scanner) 'tok-eof) #|End of File -- don't move.|#)   
    ((setf (scanner-buffer scanner) (readline (slot-value scanner 'stream)))
                                        ; got a line -- advance a token.
     (setf (scanner-column scanner) 0
           (scanner-state  scanner) +maybe-commentaire+)
     (when (plusp (scanner-line   scanner))
       (incf (scanner-line   scanner)))
     (setf (scanner-current-token scanner) nil)
     (scan-next-token scanner))
    (t                                  ; Just got EOF
     (setf (scanner-current-token scanner) (make-eof scanner))))
  (scanner-current-token scanner))


(defmethod scan-next-token ((scanner lse-scanner) &optional parser-data)
  (declare (ignore parser-data))
  (typecase (scanner-current-token scanner)
    (tok-eof
     #|End of File -- don't move|#)
    (tok-eol
     ;; (typecase (advance-line scanner)
     ;;   (tok-eof #|nothing|#)
     ;;   (t       (setf (scanner-current-token scanner) (scan-lse-token scanner))))
     (advance-line scanner))
    (t
     (setf (scanner-current-token scanner) (scan-lse-token scanner))
     ;; (print (slot-value  (scanner-current-token scanner) 'text))
     ))
  (case (token-kind (scanner-current-token scanner))
    (tok-numero
     (when (eq 'tok-eol (scanner-previous-token-kind scanner))
       (setf (scanner-state scanner) +maybe-commentaire+)))
    (tok-ptvirg
     (setf (scanner-state scanner) +maybe-commentaire+))
    (tok-crogauche
     (if (member (scanner-previous-token-kind scanner) '(tok-afficher tok-affic))
         (setf (scanner-state scanner) +in-format+)
         (setf (scanner-state scanner) +not-commentaire+)))
    (tok-crodroite
     (setf (scanner-state scanner) +not-commentaire+))
    (otherwise
     (when (eql (scanner-state scanner) +maybe-commentaire+)
       (setf (scanner-state scanner) +not-commentaire+))))
  (setf (scanner-previous-token-kind scanner) (token-kind (scanner-current-token scanner)))
  (scanner-current-token scanner))


(defun test/scan-stream (src)
  (loop
    :with scanner = (make-instance 'lse-scanner :source src :state 0)
    :initially (progn
                 (advance-line scanner)
                 (format t "~2%;; ~A~%;; ~A~%"
                         (scanner-buffer scanner)
                         (scanner-current-token scanner)))
    :do (progn
          (typecase (scanner-current-token scanner)
            (tok-eol
             (advance-line scanner)
             ;; (scan-next-token scanner)
             (format t ";; ~A~%" (scanner-buffer scanner)))
            (t
             (scan-next-token scanner)))
          (format t "~&~3A ~20A ~20S ~3A ~3A ~20A ~A~%"
                  (scanner-state scanner)
                  (token-kind (scanner-current-token scanner))
                  (token-text (scanner-current-token scanner))
                  (eolp (scanner-current-token scanner))
                  (eofp (scanner-current-token scanner))
                  (scanner-previous-token-kind scanner)
                  (type-of (scanner-current-token scanner)))
          (finish-output))
    :until (typecase (scanner-current-token scanner)
             (tok-eof t)
             (t nil))))

(defun test/scan-file (path)
  (with-open-file (src path)
    (test/scan-stream src)))

(defun test/scan-string (source)
  (with-input-from-string (src source)
    (test/scan-stream src)))


;; (test/scan-file #P"~/src/pjb/nasium-lse/SYNTERR.LSE")
;; (test/scan-file #P"~/src/pjb/nasium-lse/TESTCOMP.LSE")
;; (test/scan-string "18*")
;;;; THE END ;;;;

