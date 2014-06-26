;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               fonctions.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    LSE functions.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2014
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

(defun   indefinip  (x) (eq x :unbound))
(deftype indefini   () '(satisfies indefinip))

(defun   nombrep    (x) (typep x 'single-float))
(deftype nombre     () 'single-float)

(defun   vecteurp   (x) (typep x '(vector single-float)))
(deftype vecteur    (&optional (s '*)) `(vector single-float ,s))

(defun   tableaup   (x) (typep x '(array single-float (* *))))
(deftype tableau    (&optional (w '*) (h '*)) `(array single-float (,w ,h)))


;; On T1600, chaines are limited to 256 characters, but on Mitra-15 they're "unlimited".
#+LSE-T1600 (defconstant chaine-maximum 256)
#+LSE-T1600 (defun   chainep    (x)  (and (stringp x) (<= (length x) chaine-maximum)))
#+LSE-T1600 (deftype chaine     ()   '(satisfies chainep))

#-LSE-T1600 (defconstant chaine-maximum (min (* 64 1024) array-dimension-limit))
#-LSE-T1600 (defun   chainep    (x)     (stringp x))
#-LSE-T1600 (deftype chaine     ()      'string)


(defun   identificateurp  (x)  (and (symbolp x)
                                    (eq (symbol-package x)
                                        (find-package "COM.INFORMATIMAGO.LSE.IDENTIFIERS"))))
(deftype identificateur   () '(satisfies identificateurp))

(defstruct booleen value)
(defparameter vrai (make-booleen :value :vrai))
(defparameter faux (make-booleen :value :faux))


(define-condition incomparable      (lse-error)
  ((op :initarg :op :reader incomparable-operator)
   (a  :initarg :a  :reader incomparable-argument-a)
   (b  :initarg :b  :reader incomparable-argument-b))
  (:report print-incomparable-error))

(defmethod print-incomparable-error ((err incomparable) stream)
  (format stream "ON NE PEUT PAS COMPARER (~A) UN OBJET DE TYPE ~A (~S) AVEC UN OBJECT DE TYPE ~A (~S)"
          (ecase (incomparable-operator err)
            (:eg "=")
            (:ne "#")
            (:lt "<")
            (:le "<=")
            (:gt ">")
            (:ge ">="))
          (type-of (incomparable-argument-a err))  (incomparable-argument-a err)
          (type-of (incomparable-argument-b err))  (incomparable-argument-b err)))


(define-condition pas-implemente    (lse-error)
  ((what :initarg :what :initform "QUELQUE CHOSE" :reader pas-implemented-what))
  (:report print-pas-implemente-error))

(defmethod print-pas-implemente-error ((err pas-implemente) stream)
  (format stream "~A N'EST PAS ENCORE IMPLEMENTE" (pas-implemented-what err)))


(define-condition argument-invalide (lse-error)
  ((op            :initarg :op       :reader argument-invalide-operator)
   (index         :initarg :index    :reader argument-invalide-argument-index)
   (argument      :initarg :argument :reader argument-invalide-argument)
   (reason        :initarg :reason   :reader argument-invalide-reason))
  (:report print-argument-invalide-error))

(defmethod print-argument-invalide-error ((err argument-invalide) stream)
  (format stream "LE ~AE ARGUMENT POUR ~A, ~S, N'EST PAS VALIDE: ~A"
          (argument-invalide-argument-index err)
          (argument-invalide-operator err)
          (argument-invalide-argument err)
          (argument-invalide-reason err)))



(defmacro un-nombre  (a)
  `(coerce ,a 'nombre))

(defmacro le-booleen (a)
  `(etypecase ,a
     (booleen ,a)
     (null faux)
     (t    vrai)))

(defmacro un-booleen (a)
  `(etypecase ,a
     (booleen ,a)))

(defmacro le-nombre  (a)
  `(etypecase ,a
     (nombre  ,a)))

(defmacro la-chaine  (a)
  `(etypecase ,a
     (chaine  ,a)))


(defun ou    (a b) (if (let ((a (eq vrai (un-booleen a)))
                             (b (eq vrai (un-booleen b)))) (or a b))
                       vrai faux))

(defun et    (a b) (if (let ((a (eq vrai (un-booleen a)))
                             (b (eq vrai (un-booleen b)))) (and a b))
                       vrai faux))

(defun non   (a)   (if (eq (un-booleen a) vrai) faux vrai))

;; a b  =  /=   <  <=   >  >=
;; 1 1  1   0   0   1   0   1
;; 1 0  0   1   0   0   1   1
;; 0 1  0   1   1   1   0   0
;; 0 0  1   0   0   1   0   1
;;      eq ne
;;              <  : (and (not a) b) : ====>
;;              <= : (or  (not a) b) : <=/==
;;              >  : (and (not b) a) : <====
;;              >= : (or  (not b) a) : ==/=>

(defun <===> (a b) (if (eq (un-booleen a) (un-booleen b)) vrai faux))
(defun <=/=> (a b) (non (<===> a b)))
(defun ====> (a b) (et  (non a) b))
(defun <==== (a b) (et  (non b) a))
(defun ==/=> (a b) (non (====> a b)))
(defun <=/== (a b) (non (<==== a b)))


(defmethod eg ((a t)              (b t))              (error 'incomparable :op 'eg :a a :b b))
(defmethod ne ((a t)              (b t))              (error 'incomparable :op 'ne :a a :b b))
(defmethod lt ((a t)              (b t))              (error 'incomparable :op 'lt :a a :b b))
(defmethod le ((a t)              (b t))              (error 'incomparable :op 'le :a a :b b))
(defmethod ge ((a t)              (b t))              (error 'incomparable :op 'ge :a a :b b))
(defmethod gt ((a t)              (b t))              (error 'incomparable :op 'gt :a a :b b))

(defmacro defcompare (class eg ne lt le gt ge)
  `(progn
     (defmethod eg ((a ,class) (b ,class)) (le-booleen (,eg a b)))
     (defmethod ne ((a ,class) (b ,class)) (le-booleen (,ne a b)))
     (defmethod lt ((a ,class) (b ,class)) (le-booleen (,lt a b)))
     (defmethod le ((a ,class) (b ,class)) (le-booleen (,le a b)))
     (defmethod gt ((a ,class) (b ,class)) (le-booleen (,gt a b)))
     (defmethod ge ((a ,class) (b ,class)) (le-booleen (,ge a b)))))

;; We need to use classes here:
(defcompare number          =       /=       <       <=       >       >=)
(defcompare string    string= string/= string< string<= string> string>=)
(defcompare symbol    string= string/= string< string<= string> string>=)
(defcompare booleen     <===>   <=/=>    ====>    <=/==   <====    ==/=>)


(defchapter ("SITERNAIRE" "FONCTIONS")
    "SI condition ALORS expr1 SINON expr2

Résultat: si condition est vrai alors le résultat de l'expression
expr1, sinon le résultat de l'expression expr2.")


(defchapter ("COMPARAISON" "FONCTIONS")
    "A<B, A<=B, A>B, A>=B, A=B, A#B

A et B doivent être du même type, soit tous les deux nombres, soit
tous les deux chaînes.

Résultat: vrai ou faux, selon l'ordre de A et B.")


(defchapter ("CONJONCTION" "FONCTIONS")
    "comparaison1 ET comparaison2

Résultat: conjonction des deux comparaisons.")


(defchapter ("DISJONCTION" "FONCTIONS")
    "conjonction1 OU conjonction2

Résultat: disjonction des deux conjonctions.")


(defchapter ("NEGATION" "FONCTIONS")
    "NON condition

Résultat: La négation logique de la condition.")


(defchapter ("PRIORITE" "FONCTIONS")
    "Priorité des opérateurs

Hors présence de parenthèses, les opérations sont évaluées par ordre
de priorité décroissante:

-  SI ternaire, appel de fonction, référence à une variable ou un tableau.

-  A^B Élévation à la puissance.

-  A*B et A/B, multiplication et division.

-  -A. opposé.

-  A+B, A-B et C!D, addition et soustraction de nombres, et
   concaténation de chaînes.

-  A<B, A<=B, A>B, A>=B, A=B et A#B, comparaisons.

-  NON EB, négation logique.

-  A ET B, conjonction.

-  A OU B, disjonction.")



(defmacro defunction (name parameters one-liner docstring &body body)
  (let ((lisp-name (if (atom name)
                       name
                       (first name)))
        (title (if (atom name)
                   (string name)
                   (second name))))
    `(progn
       (defchapter (,title "FONCTIONS" ,one-liner) ,docstring)
       (defun ,lisp-name ,parameters ,@body))))



(defunction (add "PLUS") (a b)
  "Somme de deux nombres"
  "A+B

Résultat: La somme des deux nombres."
  (+        (un-nombre a) (un-nombre b)))



(defunction (sub "MOINS")  (a b)
  "Différence de deux nombres"
  "A-B

Résultat: La difference entre A et B."
  (-        (un-nombre a) (un-nombre b)))



(defunction (mul "FOIS")  (a b)
  "Produit de deux nombres"
  "A*B

Résultat: Le produit de la multiplication entre A et B."
  (*        (un-nombre a) (un-nombre b)))



(defunction (div "DIVISE")  (a b)
  "Quotient de deux nombres"
  "A/B

Résultat: Le produit de la multiplication entre A et B.

B doit être non-nul, sinon un erreur est détectée."
  (/        (un-nombre a) (un-nombre b)))



(defunction (pow "PUISSANCE")  (a b)
  "Élévation à la puissance"
  "A^B

Résultat: A à la puissance B.

Si A est négatif, alors B doit être entier.

Note sur un terminal Unicode, ^ s'affiche comme une flêche vers le
haut.  Sur un terminal ASCII, c'est un chapeau, AltGr-9 ou chapeau
espace sur un clavier AZERTY, Shift-6 sur un clavier QWERTY."
  (when (and (< (un-nombre a) 0.0) (/= (truncate (un-nombre b)) b))
    (lse-error 'argument-invalide
           :op "POW"
           :index 2
           :argument b
           :reason "QUAND LE PREMIER ARGUMENT EST NEGATIF, LE SECOND DOIT ETRE ENTIER."))
  (un-nombre (expt a (if (< a 0.0) (truncate b) b))))



(defunction (ent "ENT")  (a)
  "Partie entière"
  "ENT(A)

Résultat: La partie entière de A."
  (let ((a (deref *vm* a))) (un-nombre (floor (un-nombre a)))))



(defunction (neg "OPPOSÉ")  (a)
  "Changement de signe"
  "-A

Résultat: L'opposé de A."
  (let ((a (deref *vm* a))) (-        (un-nombre a))))


(defunction (abso "ABS") (a)
  "Valeur absolue"
  "ABS(A)

Résultat: La valeur absolue de A."  
  (let ((a (deref *vm* a))) (abs      (un-nombre a))))



(defunction (expo "EXP") (a)
  "exponentiation e^A"
  "EXP(A)

Résultat: e à la puissance A.

e=2.7182817

LGN est l'inverse de EXP."
  (let ((a (deref *vm* a))) (exp      (un-nombre a))))



(defunction (sinu "SIN") (a)
  "Sinus"
  "SIN(A)

Résultat: Le Sinus de l'angle A exprimé en radian."
  (let ((a (deref *vm* a))) (sin      (un-nombre a))))



(defunction (cosi "COS") (a)
  "Cosinus"
  "COS(A)

Résultat: Le Cosinus de l'angle A exprimé en radian."
  (let ((a (deref *vm* a))) (cos      (un-nombre a))))



(defunction atg  (a)
  "Arc tangente"
  "ATG(A)

Résultat: L'arc tangente de A, exprimé en radian."
  (let ((a (deref *vm* a))) (atan     (un-nombre a))))



(defunction rac  (a)
  "Racine carrée"
  "RAC(A)

Résultat: La racine carrée de A.

L'argument A doit être un nombre positif ou nul, sinon une erreur est détectée."
  (let* ((a (deref *vm* a)) (result (sqrt (un-nombre a)))) (un-nombre result)))


(defunction lgn  (a)
  "Logarithme népérien"
  "LGN(A)

Résultat: Le logarithme népérien de A.

L'argument A doit être un nombre strictement positif, sinon une erreur est détectée."
  (let* ((a (deref *vm* a)) (result (log  (un-nombre a)))) (un-nombre result)))


(defun lcg (x)
  ;; http://en.wikipedia.org/wiki/Linear_congruential_generator
  (let ((a 1103515245)
        (c 12345))
    (logand (+ (* a x) c) #xffffffff)))

(defunction ale  (a)
  "Valeur pseudo-aléatoire"
  "ALE(A)

Résultat: Une valeur pseudo-aléatoire comprise entre 0 et 1 (bornes exclues).

Si A=0 alors le résultat est vraiment aléatoire (différent à chaque appel).

Si 0<A<1 alors le résultat dépent de A, ce qui permet de calculer des
séries pseudo-aléatoire reproduisibles."
  ;; (ale 0) --> true random
  ;; (ale n) --> pseudo-random from n
  (let ((a (un-nombre (deref *vm* a))))
    (if (zerop a)
        (random 1.0)
        (coerce (/ (lcg (truncate (* #x100000000 a))) #x100000000) 'nombre))))



(defunction tem ()
  "Temps en seconde écoulé depuis minuit."
  "TEM(A)

Résultat: Le nombre de secondes écoulées depuis le début du jour (minuit)."
  (multiple-value-bind (s m h) (get-decoded-time)
    (+ (* (+ (* 60 h) m) 60) s)))


#+lse-extensions
(defunction att ()
  "Signal d'attention utilisateur"
  "ATT()

Cette fonction retourne normalement 0.

Lorsque l'utilisateur tape [CTRL-A], ATT() retourne transitoirement 1."
  (if (task-signal *task*)
      (progn
        (setf (task-signal *task*) nil)
        1)
      0))



#+(and lse-extensions (or))
(defunction dis (a)
  "DIS(A)

Résultat: Une chaine contenant le contenu du secteur numéro A du disque."
  (declare (ignore a))  (error 'pas-implemente :what "DIS"))




#+lse-extensions
(defunction etl (a b)
   "ET logique bit-à-bit"
   "ETL(A,B)

Résultat: le ET logique bit-à-bit entre les bits de A et ceux de B."
   (un-nombre (logand (truncate (un-nombre (deref *vm* a)))
                      (truncate (un-nombre (deref *vm* b))))))

;; (unless (fboundp 'etl)
;;    (format t "ETL not bound~% *features* = ~S~%" *features*)
;;    (format t "~S" #+lse-extensions 'lse-extensions #-lse-extensions '(not lse-extensions))
;;    (format t "~S" (macroexpand '(defunction etl (a b)
;;                                  "ET logique bit-à-bit"
;;                                  "ETL(A,B)
;; 
;; Résultat: le ET logique bit-à-bit entre les bits de A et ceux de B."
;;                                  (un-nombre (logand (truncate (un-nombre (deref *vm* a)))
;;                                              (truncate (un-nombre (deref *vm* b))))))
;;                                ))
;;    (finish-output)
;;    #+ccl (ccl:quit))



#+lse-extensions
(defunction oul (a b)
  "OU logique bit-à-bit"
  "OUL(A,B)

Résultat le OU logique bit-à-bit entre les bits de A et ceux de B."
  (un-nombre (logior (truncate (un-nombre (deref *vm* a)))
                     (truncate (un-nombre (deref *vm* b))))))


#+lse-extensions
(defunction oux (a b)
  "OU exclusif bit-à-bit"
  "OUX(A,B)

Résultat le OU exclusif bit-à-bit entre les bits de A et ceux de B."
  (un-nombre (logxor (truncate (un-nombre (deref *vm* a)))
                     (truncate (un-nombre (deref *vm* b))))))




#+debugging
(defunction (lisp-eval "LISP") (expr &optional (print 0) (noerr 0))
  "Evaluation d'une expression LISP"
  "LISP(EXPR,PRINT,NOERR)

Cette fonction évalue l'expression Common Lisp EXPR.

1* FONCTION LISP(EXPR,PRINT,NOERR)

2* PRINT=0 => LISP() N'AFFICHE RIEN;

3* PRINT=1 => LISP() AFFICHE LES RESULTATS DE L'EXPRESSION LISP.

4 PRINT_1

5* NOERR=0 => SI UNE ERREUR EST DETECTEE ELLE EST RAPPORTEE NORMALEMENT.

6* NOERR=1 => SI UNE ERREUR EST DETECTEE, LISP() RETOURNE 0.0.

7 NOERR_1

8 A_LISP('(TRUNCATE 10 3)',PRINT,NOERR)

9 TERMINER
"
  (let ((expr  (deref *vm* expr))
        (print (deref *vm* print))
        (noerr (deref *vm* noerr))
        (*package* (find-package #-(and) "COMMON-LISP-USER"
                                 "COM.INFORMATIMAGO.LSE")))
    (handler-case
        (let* ((results)
               (output (with-output-to-string (out)
                         (let ((*standard-output* out)
                               (*error-output* out)
                               (*trace-output* out))
                           (setf results (multiple-value-list (eval (read-from-string expr))))))))
          (io-format *task* "~&~A~&" output)
          (when (plusp print)
            (io-format *task* "~&--> ~{~S~^~%    ~}~%" results))
          (typecase (first results)
            ((or integer nombre chaine) (first results))
            (t (prin1-to-string (first results)))))
      
      (error (err)
        (if (plusp noerr)
            0.0 ; TODO: we should return '' when a string is expected.
            (error err))))))



(defunction (concatenation "!") (a b)
  "Concaténation de deux chaînes"
  "A!B

Résultat: La concatenation des chaînes A et B."
  (la-chaine (concatenate 'string (la-chaine (deref *vm* a)) (la-chaine (deref *vm* b)))))


(defunction lgr (a)
  "Longueur d'une chaîne"
  "LGR(A)

Résultat: la longueur de la chaîne A."
  (un-nombre (length (la-chaine (deref *vm* a)))))



(defunction pos (ch de sc)
  "Position d'une sous-chaîne"
  "POS(CH,DE,SC)

Résultat: la position de la première occurence de la sous-chaîne SC dans la
chaîne CH, après la position DE, ou 0 si SC n'est pas une sous-chaîne de CH.

DE doit être un nombre entier supérieur ou égal à 1."
  (let* ((ch (deref *vm* ch))
         (de (deref *vm* de))
         (sc (deref *vm* sc))
         (debut (1- (truncate (un-nombre de)))))
    (when (or (< debut 0) (/= (1+ debut) de))
      (error 'argument-invalide
             :backtrace (or #+ccl (ccl::backtrace-as-list))
             :op "POS"
             :index 2
             :argument de
             :reason "LE DEBUT DOIT ETRE UN ENTIER SUPERIEUR OU EGAL A 1."))
    (if (< (length (la-chaine ch)) (+ debut (length (la-chaine sc))))
        0.0
        (+ 1.0 (or (search sc ch :start2 debut) -1.0)))))
           

(defunction eqn (ch &optional po)
  "Équivalent numérique"
  "EQN(CH) ou EQN(CH,PO)

Résultat: l'équivalent numérique en code ASCII du caractère de la
chaine CH qui se trouve en position PO, ou en première position si PO
est omis."
  (let* ((ch (deref *vm* ch))
         (po (deref *vm* po)))
    (un-nombre (char-code (aref (la-chaine ch)
                                (if po
                                    (truncate (1- (un-nombre po)))
                                    0))))))


(defunction eqc (co)
  "Équivalent caractère"
  "EQC(CO)

Résultat: une chaîne de 1 caractère dont l'équivalent numérique en
code ASCII est CO."
  (let* ((co (truncate (un-nombre (deref *vm* co))))
         (limit #+lse-t1600     256
                #+lse-mitra-15  128
                #-(or lse-t1600 lse-mitra-15)  char-code-limit))
    (if (< -1 co limit)
        (string (code-char co))
        (error 'argument-invalide
               :backtrace (or #+ccl (ccl::backtrace-as-list))
               :op "EQC"
               :index 1
               :argument co
               :reason (format nil "L'ARGUMENT DOIT ETRE LE CODE D'UN CARACTERE (ENTRE 0 et ~D)."
                               (1- limit))))))


(defunction cca (ca)
  "Conversion en caractère"
  "CCA(A)

Résultat: une chaîne de caractère contenant la représentation du
nombre A au format U."
  (let* ((ca (deref *vm* ca))
         (ca (un-nombre ca))
         (value (abs ca)))
    (format nil (cond
                  ((and (<= 1e-3 value) (< value 1e6)) "~A")
                  (t                                   "~,,2,,,,'EE"))
            (if (= ca (truncate ca))
                (truncate ca)
                ca))))


(defun set-va (va value)
  (let ((value (un-nombre value)))  
    (etypecase va
      (lse-variable
       (if (eql (variable-type va) 'nombre)
           (setf (variable-value va) value)
           (lse-error "LA VARIABLE ~A N'EST PAS UN NOMBRE" va))
       value)
      (null value))))


(defunction cnb (ch de &optional va)
  "Conversion en nombre"
  "CNB(CH,DE) ou CNB(CH,DE,VA)

Résultat: un nombre converti de la sous-chaîne de CH commençant à la
position DE. (Les espaces initiaux sont ignorés).

Si VA est présent, ce doit être une variable arithmétique à laquelle
la fonction CNB affecte la position du premier caractère qui n'est pas
utilisé dans la représentation."
  (let* ((ch (la-chaine (deref *vm* ch)))
         (de (deref *vm* de))
         (chlen (length ch))
         (debut (1- (truncate (un-nombre de))))
         (fin 0))
    (when (or (< debut 0) (/= (1+ debut) de) (<= chlen debut))
      (error 'argument-invalide
             :backtrace (or #+ccl (ccl::backtrace-as-list))
             :op "CNB"
             :index 2
             :argument de             
             :reason "L'ARGUMENT DEBUT DOIT ETRE UN NOMBRE ENTIER ENTRE 1 ET LA LONGUEUR DE LA CHAINE."))
    (labels ((skip-spaces (pos)
               (position (character " ") ch :start pos :test (function char/=)))
             (eos! (fin)
               (return-from cnb
                 (values (un-nombre
                          (or (read-from-string ch nil nil :start debut
                                                :end (min fin chlen))
                              0.0))
                         (set-va va (1+ (min fin chlen))))))
             (eos? (pos) (when  (<= chlen fin) (eos! pos)))
             (match? (pos charseq) (position (aref ch pos) charseq))
             (digit? (pos) (digit-char-p (aref ch pos)))
             (skip-digits (pos)
               (or (position-if-not (function digit-char-p) ch :start pos)
                   (1+ chlen))))
      (setf debut (skip-spaces debut))
      (eos? debut)
      (setf fin debut)
      (when (match? fin "+-") (incf fin))
      (unless (digit? fin) (return-from cnb (values 0.0 (+ 1.0 debut))))
      (setf fin (skip-digits fin))
      (eos? fin)
      (when (match? fin ".")
        (incf fin)
        (when (or (eos? fin) (not (match? fin "E0123456789"))) (eos! fin))
        (setf fin (skip-digits fin))
        (eos? fin))
      (when (match? fin "E")
        (incf fin)
        (eos? (- fin 1))
        (when (match? fin "+-")
          (incf fin)
          (eos? (- fin 2)))
        (when (digit? fin)
          (setf fin (skip-digits fin))))
      (eos! fin))))
        


(defunction sch (ch de lo-or-ch &optional va)
  "Sous-chaîne"
  "SCH(CH,DE,LO[,VA]) ou SCH(CH,DE,SC[,VA])

CH est une chaîne d'où SCH va extraire une sous-chaîne.

DE est la position de départ de la sous-chaîne.

LO est la longueur de la sous-chaîne.

SC est une chaîne contenant les caractères d'arrêt de la sous-chaîne.

Résultat: une sous-chaîne de CH, commençant à la position DE, de
longueur LO ou allant jusqu'à la première occurence d'un des
caractères de SC.

Si VA est présent, ce doit être une variable arithmétique à laquelle
la fonction SCH affecte soit la position du premier caractère qui
n'est pas utilisé dans la représentation, soit LGR(CH)+1 si le dernier
caractère de la sous-chaîne est le dernier caractère de la chaîne."
  (let* ((ch       (deref *vm* ch))
         (de       (deref *vm* de))
         (lo-or-ch (deref *vm* lo-or-ch))
         (debut (1- (truncate (un-nombre de))))
         (chlen (length (la-chaine ch))))
    (when (or (/= (1+ debut) de) (< debut 0))
      (error 'argument-invalide
             :backtrace (or #+ccl (ccl::backtrace-as-list))
             :op "SCH"
             :index 2
             :argument de
             :reason "L'ARGUMENT DEBUT DOIT ETRE UN NOMBRE ENTIER SUPERIEUR OU EGAL A 1."))
    (let ((fin  (etypecase lo-or-ch
                  ((or integer nombre)
                   (let ((longueur (truncate lo-or-ch)))
                     (if (or (< longueur 0) (/= longueur lo-or-ch))
                         (error 'argument-invalide
                                :backtrace (or #+ccl (ccl::backtrace-as-list))
                                :op "SCH"
                                :index 3
                                :argument lo-or-ch
                                :reason "L'ARGUMENT LONGEUR DOIT ETRE UN ENTIER SUPERIEUR OU EGAL A 0 (OU BIEN UNE CHAINE).")
                         (+ debut longueur))))
                  (chaine
                   (or (position-if
                        (lambda (ch) (position ch lo-or-ch
                                               :test (function char=)))
                        ch :start debut)
                       chlen)))))
      (setf fin   (min fin   chlen))
      (setf debut (min debut chlen))
      (values (subseq ch debut fin)
              (set-va va (1+ fin))))))


(defunction skp (ch de &optional ev)
  "Saut"
  "SKP(CH,DE) ou SKP(CH,DE,EV)

Résultat: la position dans la chaîne CH, à partir de la position DE,
de la première lettre si EV n'est pas donné, ou du premier caractère
qui n'est pas dans la chaîne EV; ou bien LGR(CH)+1 si aucune lettre,
ou si tous les caractères sont dans la chaine EV."
  (let* ((ch (deref *vm* ch))
         (de (deref *vm* de))
         (ev (deref *vm* ev))
         (debut (1- (truncate (un-nombre de)))))
    (when (or (< debut 0) (/= (1+ debut) de))
      (error 'argument-invalide
             :backtrace (or #+ccl (ccl::backtrace-as-list))
             :op "SKP"
             :index 2
             :argument de
             :reason "L'ARGUMENT DEBUT DOIT ETRE UN ENTIER SUPERIEUR OU EGAL A 1."))
    (+ 1.0 (or (position-if (if (null ev)
                                (function alpha-char-p)
                                (progn (la-chaine ev) (lambda (ch) (not (position ch ev)))))
                            (la-chaine ch) :start debut)
               (length ch)))))


(defunction ptr (ch de &optional ev)
  "Pointeur"
  "PTR(CH,DE) ou PTR(CH,DE,EV)

Résultat: Si EV n'est pas donné : la position dans la chaîne CH, à
partir de la position DE, du premier caractère qui n'est pas une
lettre, ou LGR(CH)+1 si tous les caractères sont des lettres.  Si EV
est donné: la position dans la chaîne CH, à partir de la position DE,
du premier caractère qui EST dans la chaîne EV, ou LGR(CH)+1 si aucun
des caractères de CH ne sont dans la chaine EV."
  (let* ((ch (deref *vm* ch))
         (de (deref *vm* de))
         (ev (deref *vm* ev))
         (debut (1- (truncate (un-nombre de)))))
    (when (or (< debut 0) (/= (1+ debut) de))
      (error 'argument-invalide
             :backtrace (or #+ccl (ccl::backtrace-as-list))
             :op "PTR"
             :index 2
             :argument de
             :reason "L'ARGUMENT DEBUT DOIT ETRE UN ENTIER SUPERIEUR OU EGAL A 1."))
    (+ 1.0 (or (position-if (if ev
                                (progn (la-chaine ev) (lambda (ch) (find ch ev)))
                                (complement (function alpha-char-p)))
                            (la-chaine ch) :start debut)
               (length ch)))))


(defunction grl (ch de &optional va)
  "Groupe de lettres"
  "GRL(CH,DE) ou GRL(CH,DE,VA)

Résultat: la sous-chaîne composée de la première séquence continue de
lettres à partir de la position DE.

Si VA est présent, ce doit être une variable arithmétique à laquelle
la fonction SCH affecte soit la position du premier caractère qui
n'est pas une lettre, soit LGR(CH)+1 si le dernier caractère de la
sous-chaîne est le dernier caractère de la chaîne."
  (let* ((ch (deref *vm* ch))
         (de (deref *vm* de))
         (debut (1- (truncate (un-nombre de))))
         (chlen (length (la-chaine ch))))
    (when (or (< debut 0) (/= (1+ debut) de) (<= chlen debut))
      (error 'argument-invalide
             :backtrace (or #+ccl (ccl::backtrace-as-list))
             :op "GRL"
             :index 2
             :argument de
             :reason "L'ARGUMENT DEBUT DOIT ETRE UN ENTIER SUPERIEUR OU EGAL A 1 ET INFERIEUR OU EGAL A LA LONGUEUR DE LA CHAINE."))
    (let ((debut (position-if (function alpha-char-p) ch :start debut)))
      (if debut
          (let ((fin (position-if-not (function alpha-char-p) ch :start debut)))
            (values (subseq ch debut fin)
                    (set-va va (if fin (1+ fin) (1+ chlen)))))
          (values ""
                  (set-va va (1+ chlen)))))))
           
(defun formate-date (universal-time)
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time universal-time)
    (format nil "~2,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            da mo (mod ye 100) ho mi se)))

(defunction dat ()
  "Chaîne date"
  "DAT()

Résultat: une chaîne au format AA/MM/DD HH:MM:SS représentant la date
et l'heure courante.

Note: Cette fonction n'est pas conforme Y2K, elle retourne 00 pour 2000."
  (formate-date (get-universal-time)))


#+lse-unix
(defunction env (nom)
    "Variable d'environement"
    "ENV(NOM)

Résultat: une chaîne contenant la valeur de la variable d'environement
POSIX du nom indiqué.  Si la variable d'environement n'existe pas, une
chaîne vide est retournée.

Exemple: ENV('USERNAME') --> 'pjb'
"
  (let* ((nom (la-chaine (deref *vm* nom)))
         (val (getenv nom)))
    (or val "")))


(defun test/fonctions (&key silence)
  (flet ((test ()
           (format t "DAT ~A~%" (dat))
           ;; Numeric:
           (princ (dat))(terpri)
           (dotimes (a 21)
             (format t "~3D: " a)
             (dolist (op '((- neg) (truncate ent) (abs abso) (exp expo)
                           (sin sinu) (cos cosi) (atan atg) (sqrt rac)
                           (log lgn)))
               (format t "~A " (second op))
               (assert
                (equal (ignore-errors (un-nombre (funcall (first op) (- a 10.0))))
                       (ignore-errors (funcall (second op) (- a 10.0))))
                () "~S : ~S = ~S /= ~S = ~S"
                (second op)
                '(ignore-errors (un-nombre (funcall (first op) (- a 10.0))))
                 (ignore-errors (un-nombre (funcall (first op) (- a 10.0))))
                 (ignore-errors (funcall (second op) (- a 10.0)))
                 '(ignore-errors (funcall (second op) (- a 10.0)))))
             (format t "~%")
             (dotimes (b 21)
               (format t "     ~3D: " b)
               #+lse-extensions
               (dolist (op `((+ add) (- sub) (* mul) (/ div)
                             (,(lambda (a b)
                                       (expt a (if (and (< a 0) (= b (truncate b)))
                                                   (truncate b) b)))
                               pow)
                             (,(lambda (a b) (logand (truncate a) (truncate b))) etl)
                             (,(lambda (a b) (logior (truncate a) (truncate b))) oul)
                             (,(lambda (a b) (logxor (truncate a) (truncate b))) oux)))
                 (format t "~A ~A ~A" (second op) a b)
                 (assert
                  (equal
                   (ignore-errors (un-nombre (funcall (first op) (- a 10.0) (- b 10.0))))
                   (ignore-errors (funcall (second op) (- a 10.0) (- b 10.0))))
                  () "~S : ~S = ~S /= ~S = ~S"
                  (second op)
                  '(ignore-errors (un-nombre (funcall (first op) (- a 10.0) (- b 10.0))))
                  (ignore-errors (un-nombre (funcall (first op) (- a 10.0) (- b 10.0))))
                  (ignore-errors (funcall (second op) (- a 10.0) (- b 10.0)))
                  '(ignore-errors (funcall (second op) (- a 10.0) (- b 10.0)))))
               (format t "~%")))
           ;; Boolean:
           (format t "NON~%")
           (dolist (b (list (list vrai faux) (list faux vrai) (list 42 nil)))
             (assert (eq (second b) (ignore-errors (non (first b))))
                     (b) "NON ~A = ~A instead of ~A"
                     (first b) (ignore-errors (non (first b))) (second b)))
           (format t "BOOLEEN ")
           (dolist (op `((et ,(lambda (a b) (cond ((and (eq vrai a) (eq vrai b)) vrai)
                                                  ((or (and (eq vrai a) (eq faux b))
                                                       (and (eq faux a) (eq vrai b))
                                                       (and (eq faux a) (eq faux b))) faux)
                                                  (t nil))))
                         (ou ,(lambda (a b) (cond ((and (eq faux a) (eq faux b)) faux)
                                                  ((or (and (eq vrai a) (eq faux b))
                                                       (and (eq faux a) (eq vrai b))
                                                       (and (eq vrai a) (eq vrai b))) vrai)
                                                  (t nil))))
                         (eg ,(lambda (a b) (cond ((or (and (eq vrai a) (eq faux b))
                                                       (and (eq faux a) (eq vrai b))) faux)
                                                  ((or (and (eq faux a) (eq faux b))
                                                       (and (eq vrai a) (eq vrai b))) vrai)
                                                  (t nil))))
                         (ne ,(lambda (a b) (cond ((or (and (eq vrai a) (eq faux b))
                                                       (and (eq faux a) (eq vrai b))) vrai)
                                                  ((or (and (eq faux a) (eq faux b))
                                                       (and (eq vrai a) (eq vrai b))) faux)
                                                  (t nil))))
                         (lt ,(lambda (a b) (cond ((and (eq faux a) (eq vrai b)) vrai)
                                                  ((or (and (eq vrai a) (eq vrai b))
                                                       (and (eq vrai a) (eq faux b))
                                                       (and (eq faux a) (eq faux b))) faux)
                                                  (t nil))))
                         (gt ,(lambda (b a) (cond ((and (eq faux a) (eq vrai b)) vrai)
                                                  ((or (and (eq vrai a) (eq vrai b))
                                                       (and (eq vrai a) (eq faux b))
                                                       (and (eq faux a) (eq faux b))) faux)
                                                  (t nil))))
                         (ge ,(lambda (a b) (cond ((and (eq faux a) (eq vrai b)) faux)
                                                  ((or (and (eq vrai a) (eq vrai b))
                                                       (and (eq vrai a) (eq faux b))
                                                       (and (eq faux a) (eq faux b))) vrai)
                                                  (t nil))))
                         (le ,(lambda (b a) (cond ((and (eq faux a) (eq vrai b)) faux)
                                                  ((or (and (eq vrai a) (eq vrai b))
                                                       (and (eq vrai a) (eq faux b))
                                                       (and (eq faux a) (eq faux b))) vrai)
                                                  (t nil)))))
                    (format t "~%"))
             (format t "~A " (first op))
             (dolist (a (list vrai faux 42))
               (dolist (b (list vrai faux "42"))
                 (assert (eq (ignore-errors (funcall (first op) a b))
                             (funcall (second op) a b))
                         (op a b)
                         "(~S ~S ~S) = ~S /= ~S = (~S ~S ~S)"
                         (first op) a b (ignore-errors (funcall (first op) a b))
                         (funcall (second op) a b) (second op) a b))))
           ;; String:
           (format T "CONCATENATION~%")
           (dolist (test '("" "a" "Fifty Yards"))
             (dotimes (i (length test))
               (let ((a (subseq test 0 i))
                     (b (subseq test i)))
                 (assert (string= test (concatenation a b))))))
           (dolist (item '(42 42.0 '42 vrai nil))
             (assert (and (null (ignore-errors (concatenation "Hello" item)))
                          (null (ignore-errors (concatenation item "Hello")))
                          (null (ignore-errors (lgr item)))
                          (null (ignore-errors (lgr item)))
                          (null (ignore-errors (pos item item "Word")))
                          (null (ignore-errors (pos "Word" item item)))
                          (or (stringp item) (null (ignore-errors (eqn item))))
                          (or (numberp item) (null (ignore-errors (eqc item)))))))
           (format t "LGR~%")
           (dotimes (ln 10)
             (let ((c (make-string ln :initial-element (character "a"))))
               (assert (= ln (lgr c)))))
           (format t "POS~%")
           (mapcar (lambda (deb res)
                     (assert (eql res
                                  (ignore-errors
                                    (pos "Hello World" (un-nombre deb) "World"))))
                     (assert (eql (and res 0.0)
                                  (ignore-errors
                                    (pos "Hello World" (un-nombre deb) "Planet")))))
                   (iota 20 -4)
                   '(nil nil nil nil nil 7.0 7.0 7.0 7.0 7.0 7.0 7.0
                     0.0 0.0 0.0 0.0 0.0 0.0))
           (format t "EQN EQC~%")
           (dotimes (i 10)
             (assert (= (+ i 32) (eqn (eqc (+ i 32.0)))))
             (assert (let ((ch  (format nil "~C" (code-char (+ i 32)))))
                       (= (+ i 32) (eqn ch))))
             (assert (let ((ch  (format nil "~C" (code-char (+ i 32)))))
                       (string= ch (eqc (eqn ch))))))
           (format t "CNB~%")
           (map nil (lambda (i res) 
                      (assert (and (equal res (ignore-errors
                                                (multiple-value-list
                                                 (cnb "123456789" (un-nombre i)))))
                                   (equal res (ignore-errors
                                                (let* ((va  (make-instance 'lse-variable
                                                                :name 'com.informatimago.lse.identifiers::va))
                                                       (res (cnb "123456789" (un-nombre i) va)))
                                                  (list res (variable-value va))))))))
                (iota 11)
                '(nil (#+ccl 1.2345679E8
                       #-ccl 1.2345678E8
                       10.0)
                  (2.3456789E7 10.0) (3456789.0 10.0)
                  (456789.0 10.0) (56789.0 10.0) (6789.0 10.0) (789.0 10.0) (89.0 10.0)
                  (9.0 10.0) NIL))
           (loop
             :for res :in '(NIL (0.0 1.0) (0.0 2.0) (0.0 4.0) (0.0 4.0) (0.0 5.0)
                            (0.0 6.0) (0.0 7.0) (0.0 8.0) (0.0 9.0) (421.0 14.0)
                            (421.0 14.0) (21.0 14.0) (1.0 14.0) (0.0 14.0) nil NIL)
             :for i :to 16 
             :do (assert (equal res (ignore-errors
                                      (multiple-value-list
                                       (cnb "Le nombre 421%" (un-nombre i)))))
                         () "~S /= ~S = ~S"
                         res
                         (ignore-errors
                           (multiple-value-list
                            (cnb "Le nombre 421%" (un-nombre i))))
                         '(ignore-errors
                           (multiple-value-list
                            (cnb "Le nombre 421%" (un-nombre i))))))
           (format t "SCH ~A~%" (dat)) 
           (let ((expected '(NIL  NIL  NIL ("" 1.0)  NIL  NIL  NIL  NIL  ("a" 2.0)
                             ("a"  2.0)  NIL ("" 2.0)  ("" 2.0)  NIL  NIL  NIL  NIL  NIL  NIL
                             NIL NIL  NIL  NIL NIL  NIL  NIL  ("J" 2.0)  ("Ja" 3.0)  ("Jab" 4.0)
                             ("Jabe" 5.0) ("Jaber" 6.0)  ("Jaberw" 7.0)  ("Jaberwo" 8.0)
                             ("Jaberwoc" 9.0) ("Jaberwock" 10.0)  ("Jaberwocky" 11.0)
                             ("Jaberwocky" 11.0)  NIL  ("a" 3.0)  ("ab" 4.0)  ("abe" 5.0)
                             ("aber" 6.0)  ("aberw" 7.0)  ("aberwo" 8.0)  ("aberwoc" 9.0)
                             ("aberwock" 10.0)  ("aberwocky" 11.0) ("aberwocky" 11.0)
                             ("aberwocky" 11.0)  NIL  ("b" 4.0)  ("be" 5.0) ("ber" 6.0)
                             ("berw" 7.0)  ("berwo" 8.0)  ("berwoc" 9.0)  ("berwock" 10.0)
                             ("berwocky" 11.0)  ("berwocky" 11.0)  ("berwocky" 11.0)
                             ("berwocky" 11.0)  NIL  ("e" 5.0)  ("er" 6.0)  ("erw" 7.0)
                             ("erwo" 8.0)  ("erwoc" 9.0)  ("erwock" 10.0)  ("erwocky" 11.0)
                             ("erwocky" 11.0)  ("erwocky" 11.0)  ("erwocky" 11.0)
                             ("erwocky"  11.0)  NIL  ("r" 6.0)  ("rw" 7.0)  ("rwo" 8.0)
                             ("rwoc" 9.0)
                             ("rwock" 10.0)  ("rwocky" 11.0)  ("rwocky" 11.0)  ("rwocky" 11.0)
                             ("rwocky" 11.0)  ("rwocky" 11.0)  ("rwocky" 11.0)  NIL  ("w" 7.0)
                             ("wo" 8.0)  ("woc" 9.0) ("wock" 10.0)  ("wocky" 11.0)
                             ("wocky" 11.0)  ("wocky" 11.0)  ("wocky" 11.0)  ("wocky" 11.0)
                             ("wocky"   11.0)  ("wocky" 11.0)  NIL  ("o" 8.0) ("oc" 9.0)
                             ("ock" 10.0)
                             ("ocky" 11.0)  ("ocky" 11.0)  ("ocky" 11.0) ("ocky" 11.0)
                             ("ocky" 11.0)  ("ocky" 11.0)  ("ocky" 11.0)  ("ocky" 11.0)  NIL
                             ("c" 9.0)  ("ck" 10.0)  ("cky" 11.0)  ("cky" 11.0)  ("cky" 11.0)
                             ("cky" 11.0)  ("cky" 11.0)  ("cky" 11.0)  ("cky" 11.0)
                             ("cky"  11.0)  ("cky" 11.0)  NIL  ("k" 10.0)  ("ky" 11.0)
                             ("ky" 11.0)
                             ("ky" 11.0)  ("ky" 11.0)  ("ky" 11.0)  ("ky" 11.0)  ("ky" 11.0)
                             ("ky" 11.0) ("ky" 11.0)  ("ky" 11.0)  NIL  ("y" 11.0)  ("y" 11.0)
                             ("y" 11.0) ("y" 11.0)  ("y" 11.0)  ("y" 11.0)  ("y" 11.0)
                             ("y"  11.0)  ("y" 11.0) ("y" 11.0)  ("y" 11.0)  NIL  ("" 11.0)
                             (""  11.0)  ("" 11.0)  ("" 11.0)  ("" 11.0)  ("" 11.0)  ("" 11.0)
                             (""  11.0)  ("" 11.0)  ("" 11.0)  ("" 11.0)  NIL))
                 (results '()))
             (dolist (test '("" "a" "Jaberwocky"))
               (dotimes (de (+ 2 (length test)))
                 (dotimes (le (+ 2 (length test)))
                   (let ((res (ignore-errors
                                (multiple-value-list
                                 (sch test (un-nombre de) (un-nombre le)))))
                         (exp (pop expected)))
                     (push res results)
                     (assert (equal exp res) () "~S /= ~S = ~S ; test = ~S ; de = ~S ; le = ~S"
                             exp res '(ignore-errors
                                       (multiple-value-list
                                        (sch test (un-nombre de) (un-nombre le))))
                             test (un-nombre de) (un-nombre le)))))))
           (let ((expected '(NIL  NIL  NIL
                             ("" 1.0)  ("" 1.0)  ("" 1.0)
                             NIL  NIL  NIL                 ; "a" 0.0 
                             ("a" 2.0)  ("a" 2.0) ("" 1.0) ; "a" 1.0
                             (""  2.0)  ("" 2.0)  ("" 2.0) ; "a" 2.0
                             NIL  NIL NIL                  ; "a" 3.0 
                             ("Jaberwocky" 11.0)  ("Jaberwocky" 11.0)  ("J" 2.0) ; 1
                             ("aberwocky" 11.0)   ("aberwocky" 11.0)   ("" 2.0)  ; 2
                             ("berwocky" 11.0)    ("berwocky" 11.0)    ("" 3.0)  ; 3
                             ("erwocky" 11.0)     ("erwocky" 11.0)     ("" 4.0)  ; 4
                             ("rwocky" 11.0)      ("rwocky" 11.0)      ("rw" 7.0)
                             ("wocky" 11.0)       ("wocky" 11.0)       ("w" 7.0)
                             ("ocky" 11.0)        ("ocky" 11.0)        ("" 7.0)
                             ("cky" 11.0)         ("cky" 11.0)         ("" 8.0)
                             ("ky" 11.0)          ("ky" 11.0)          ("k" 10.0)
                             ("y" 11.0)           ("y" 11.0)           ("" 10.0)
                             ("" 11.0)            ("" 11.0)            ("" 11.0)))
                 (results '()))
             (dolist (test '("" "a" "Jaberwocky"))
               (dotimes (de (+ 2 (length test)))
                 (dolist (stop '("" "0123" "abcdeiouy"))
                   (let ((res (ignore-errors
                                (multiple-value-list
                                 (sch test (un-nombre de) stop))))
                         (exp (pop expected)))
                     (push res results)
                     (assert (equal exp res) () "~S /= ~S = ~S ; test = ~S ; de = ~S ; le = ~S"
                             exp res '(ignore-errors
                                       (multiple-value-list
                                        (sch test (un-nombre de) stop)))
                             test (un-nombre de) stop))))))
           (format t "DAT ~A~%" (dat))
           (format t "TEM (Time dependant, might fail on heavily loaded systems)~%")
           (assert (let ((times (loop repeat 10 do (sleep 1) collect (tem))))
                     (every (lambda (x) (= 1 x)) (mapcar (function -) (cdr times) times))))
           (format t "DAT ~A~%" (dat))))
    (let ((*vm* (make-instance 'lse-vm)))
      (if silence
          (let ((*standard-output* (make-broadcast-stream)))
            (test))
          (test)))))


;;;; THE END ;;;;
