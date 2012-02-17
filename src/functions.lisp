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
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2005
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
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
#+t1600-lse (defconstant chaine-maximum 256)
#+t1600-lse (defun   chainep    (x)  (and (stringp x) (<= (length x) chaine-maximum)))
#+t1600-lse (deftype chaine     ()   '(satisfies chainep))

#-t1600-lse (defconstant chaine-maximum array-dimension-limit)
#-t1600-lse (defun   chainep    (x)   (stringp x))
#-t1600-lse (deftype chaine     ()   'string)


(defun   identificateurp  (x)  (and (symbolp x)
                                    (eq (symbol-package x)
                                        (find-package "LSE-USER"))))
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
  (format stream "~A N'EST PAS ENCORE IMPLEMENTE" (pas-implemente-what err)))


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
          (argument-invalide-reasone err)))



(defmacro un-nombre  (a) `(coerce ,a 'nombre))
(defmacro le-booleen (a) `(etypecase ,a (booleen ,a)))
(defmacro le-nombre  (a) `(etypecase ,a (nombre  ,a)))
(defmacro la-chaine  (a) `(etypecase ,a (chaine  ,a)))


(defun ou    (a b) (if (let ((a (eq vrai (le-booleen a)))
                             (b (eq vrai (le-booleen b)))) (or a b))
                       vrai faux))

(defun et    (a b) (if (let ((a (eq vrai (le-booleen a)))
                             (b (eq vrai (le-booleen b)))) (and a b))
                       vrai faux))

(defun non   (a)   (if (eq (le-booleen a) vrai) faux vrai))

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

(defun <===> (a b) (if (eq (le-booleen a) (le-booleen b)) vrai faux))
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
     (defmethod eg ((a ,class) (b ,class)) (,eg a b))
     (defmethod ne ((a ,class) (b ,class)) (,ne a b))
     (defmethod lt ((a ,class) (b ,class)) (,lt a b))
     (defmethod le ((a ,class) (b ,class)) (,le a b))
     (defmethod gt ((a ,class) (b ,class)) (,gt a b))
     (defmethod ge ((a ,class) (b ,class)) (,ge a b))))

;; We need to use classes here:
(defcompare number          =       /=       <       <=       >       >=)
(defcompare string    string= string/= string< string<= string> string>=)
(defcompare symbol    string= string/= string< string<= string> string>=)
(defcompare booleen     <===>   <=/=>    ====>    <=/==   <====    ==/=>)



(defun add  (a b) (+        (le-nombre a) (le-nombre b)))
(defun sub  (a b) (-        (le-nombre a) (le-nombre b)))
(defun mul  (a b) (*        (le-nombre a) (le-nombre b)))
(defun div  (a b) (/        (le-nombre a) (le-nombre b)))
(defun pow  (a b)
  (when (and (< (le-nombre a) 0.0) (/= (truncate (le-nombre b)) b))
    (error 'argument-invalide
           :op "POW"
           :index 2
           :argument b
           :reason "QUAND LE PREMIER ARGUMENT EST NEGATIF, LE SECOND DOIT ETRE ENTIER."))
  (un-nombre (expt a (if (< a 0.0) (truncate b) b))))
(defun ent  (a)   (un-nombre (truncate (le-nombre a))))
(defun neg  (a)   (-        (le-nombre a)))
(defun abso (a)   (abs      (le-nombre a)))
(defun expo (a)   (exp      (le-nombre a)))
(defun sinu (a)   (sin      (le-nombre a)))
(defun cosi (a)   (cos      (le-nombre a)))
(defun atg  (a)   (atan     (le-nombre a)))
(defun rac  (a)   (let ((result (sqrt (le-nombre a)))) (le-nombre result)))
(defun lgn  (a)   (let ((result (log  (le-nombre a)))) (le-nombre result)))

(defun ale  (a)
  ;; (ale 0) --> true random
  ;; (ale n) --> pseudo-random from n
  ;; ==> we should implement a pseudo-random function (or call srand/rand).
  (let ((a (le-nombre a)))
    (when (zerop a)
      (setf (task-random-state *task*) (make-random-state t)))
    (random 1.0)))
                         
(defun tem ()    (multiple-value-bind (s m h) (get-decoded-time)
                   (+ (* (+ (* 60 h) m) 60) s)))

(defun att ()    (error 'pas-implemente :what "ATT"))
(defun dis (a) (declare (ignore a))  (error 'pas-implemente :what "DIS"))
(defun etl (a b) (un-nombre (logand (truncate (le-nombre a))
                                    (truncate (le-nombre b)))))
(defun oul (a b) (un-nombre (logior (truncate (le-nombre a))
                                    (truncate (le-nombre b)))))
(defun oux (a b) (un-nombre (logxor (truncate (le-nombre a))
                                    (truncate (le-nombre b)))))


(defun concatenation (a b) (la-chaine (concatenate 'string
                                        (la-chaine a) (la-chaine b))))

(defun lgr (a) (un-nombre (length (la-chaine a))))

(defun pos (ch de sc)
  (let* ((debut (1- (truncate (le-nombre de)))))
    (when (or (< debut 0) (/= (1+ debut) de))
      (error 'argument-invalide
             :op "POS"
             :index 2
             :argument de
             :reason "LE DEBUT DOIT ETRE UN ENTIER SUPERIEUR OU EGAL A 1."))
    (if (< (length (la-chaine ch)) (+ debut (length (la-chaine sc))))
        0.0
        (+ 1.0 (or (search sc ch :start2 debut) -1.0)))))
           

(defun eqn (ch &optional po)
  (un-nombre (char-code (aref (la-chaine ch)
                              (or (and po (1- (le-nombre po))) 0)))))


(defun eqc (co)
  (string (or (code-char (truncate (le-nombre co)))
              (error 'argument-invalide
                     :op "EQC"
                     :index 1
                     :argument co
                     :reason "L'ARGUMENT DOIT ETRE LE CODE D'UN CARACTERE (ENTRE 0 et 255)."))))


(defun cca (ca)
  ;; TODO: C'est pas tout à fait ça pour les formats.
  (if (= (truncate (le-nombre ca)) ca)
      (format nil "~D" (truncate ca))
      (format nil (if (<= 1e-3 (abs (le-nombre ca)) 1e6) "~F" "~E") ca)))


(defun cnb (ch de)
  (let* ((ch (la-chaine ch))
         (chlen (length ch))
         (debut (1- (truncate (le-nombre de))))
         (fin 0))
    (when (or (< debut 0) (/= (1+ debut) de) (<= chlen debut))
      (error 'argument-invalide
             :op "CNB"
             :index 2
             :argument de             
             :reason "L'ARGUMENT DEBUT DOIT ETRE UN NOMBRE ENTIER ENTRE 1 ET LA LONGUEUR DE LA CHAINE."))
    (labels ((skip-spaces (pos)
               (position (character " ") ch :start pos :test (function char/=)))
             (eos! (fin)
               (return-from cnb
                 (values
                  (un-nombre
                   (or (read-from-string ch nil nil :start debut
                                         :end (min fin chlen)) 0.0))
                  (un-nombre fin))))
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
        

(defun sch (ch de lo-or-ch)
  (let ((debut (1- (truncate (le-nombre de))))
        (chlen (length (la-chaine ch))))
    (when (or (< debut 0) (/= (1+ debut) de))
      (error 'argument-invalide
             :op "SCH"
             :index 2
             :argument de
             :reason "L'ARGUMENT DEBUT DOIT ETRE UN NOMBRE ENTIER SUPERIEUR OU EGAL A 1."))
    (let ((fin  (etypecase lo-or-ch
                  (nombre (let ((longueur (truncate lo-or-ch)))
                            (if (or (<= longueur 0) (/= longueur lo-or-ch))
                                (error 'argument-invalide
                                       :op "SCH"
                                       :index 3
                                       :argument lo-or-ch
                                       :reason "L'ARGUMENT LONGEUR DOIT ETRE UN ENTIER SUPERIEUR OU EGAL A 1 (OU BIEN UNE CHAINE).")
                                (+ debut longueur))))
                  (chaine (or (position-if
                               (lambda (ch) (position ch lo-or-ch
                                                 :test (function char=)))
                               ch :start debut) (1+ chlen))))))
      (setf fin   (min fin   chlen))
      (setf debut (min debut chlen))
      (values (subseq ch debut fin) (un-nombre fin)))))


(defun skp (ch de &optional ev)
  (let ((debut (1- (truncate (le-nombre de)))))
    (when (or (< debut 0) (/= (1+ debut) de))
      (error 'argument-invalide
             :op "SKP"
             :index 2
             :argument de
             :reason "L'ARGUMENT DEBUT DOIT ETRE UN ENTIER SUPERIEUR OU EGAL A 1."))
    (or (find-if (if (null ev)
                     (function alpha-char-p)
                     (progn (la-chaine ev) (lambda (ch) (not (position ch ev)))))
                 (la-chaine ch) :start debut) (+ 1.0 (length ch)))))


(defun ptr (ch de &optional ev)
  (let ((debut (1- (truncate (le-nombre de)))))
    (when (or (< debut 0) (/= (1+ debut) de))
      (error 'argument-invalide
             :op "PTR"
             :index 2
             :argument de
             :reason "L'ARGUMENT DEBUT DOIT ETRE UN ENTIER SUPERIEUR OU EGAL A 1."))
    (or (find-if (if (null ev)
                     (complement (function alpha-char-p))
                     (progn (la-chaine ev) (lambda (ch) (position ch ev))))
                 (la-chaine ch) :start debut) (+ 1.0 (length ch)))))


(defun grl (ch de)
  (let ((debut (1- (truncate (le-nombre de))))
        (chlen (length (la-chaine ch))))
    (when (or (< debut 0) (/= (1+ debut) de) (<= chlen debut))
      (error 'argument-invalide
             :op "GRL"
             :index 2
             :argument de
             :reason "L'ARGUMENT DEBUT DOIT ETRE UN ENTIER SUPERIEUR OU EGAL A 1 ET INFERIEUR OU EGAL A LA LONGUEUR DE LA CHAINE."))
    (let ((debut (position-if (function alpha-char-p) ch :start debut)))
      (if debut
          (let ((fin (position-if-not (function alpha-char-p) ch :start debut)))
            (values (subseq ch debut fin) (or fin (1+ chlen))))
          (values "" (1+ chlen))))))
           
(defun formate-date (universal-time)
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time universal-time)
    (format nil "~2,'0D/~2,'0D/~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            da mo (mod ye 100) ho mi se)))

(defun dat ()
  (formate-date (get-universal-time)))



(defun test-fonctions ()
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
              (ignore-errors (funcall (second op) (- a 10.0))))))
    (format t "~%")
    (dotimes (b 21)
      (format t "     ~3D: " b)
      (dolist (op `((+ add) (- sub) (* mul) (/ div)
                    (,(lambda (a b)
                         (expt a (if (and (< a 0) (= b (truncate b)))
                                     (truncate b) b)))
                      pow)
                    (,(lambda (a b) (logand (truncate a) (truncate b))) etl)
                    (,(lambda (a b) (logior (truncate a) (truncate b))) oul)
                    (,(lambda (a b) (logxor (truncate a) (truncate b))) oux)))
        (format t "~A " (second op) a b)
        (assert
         (equal
          (ignore-errors (un-nombre (funcall (first op) (- a 10.0) (- b 10.0))))
          (ignore-errors (funcall (second op) (- a 10.0) (- b 10.0))))))
      (format t "~%")))
  ;; Boolean:
  (format t "NON~%")
  (dolist (b (list (list vrai faux) (list faux vrai) (list 42 nil)))
    (assert (eql (second b) (ignore-errors (non (first b))))))
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
                    (funcall (second op) a b))))))
  ;; String:
  (format T "CONCATENATION~%")
  (dolist (test '("" "a" "Fifty Yards"))
    (dotimes (i (length test))
      (let ((a (subseq test 0 i))
            (b (subseq test i)))
        (assert (string= test (concatenation a b))))))
  (dolist (item '(42 42.0 '42 vrai nil))
    (assert (and (null (ignore-errors (concatenate "Hello" item)))
                 (null (ignore-errors (concatenate item "Hello")))
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
             (assert (equal res (ignore-errors
                                  (multiple-value-list
                                   (cnb "123456789" (un-nombre i)))))))
       (iota 11)
       '(nil (1.2345679E8 10.0) (2.3456788E7 10.0) (3456789.0 10.0)
         (456789.0 10.0) (56789.0 10.0) (6789.0 10.0) (789.0 10.0) (89.0 10.0)
         (9.0 10.0) NIL))
  (assert (equal (loop for i to 16
                    collect (ignore-errors
                              (multiple-value-list
                               (cnb "Le nombre 421%" (un-nombre i)))))
                 '(NIL (0.0 1.0) (0.0 2.0) (0.0 4.0) (0.0 4.0) (0.0 5.0)
                   (0.0 6.0) (0.0 7.0) (0.0 8.0) (0.0 9.0) (421.0 13.0)
                   (421.0 13.0) (21.0 13.0) (1.0 13.0) (0.0 14.0) NIL NIL)))
  (format t "SCH ~A~%" (dat))
  (assert (equal
           (let ((result '()))
             (dolist (test '("" "a" "Jaberwocky") result)
               (dotimes (de (+ 2 (length test)))
                 (dotimes (le (+ 2 (length test)))
                   (push (ignore-errors
                           (multiple-value-list
                            (sch test (un-nombre de) (un-nombre le))))
                         result)))))
           '(NIL  NIL  NIL  ("" 0.0)  NIL  NIL  NIL  NIL  ("a" 1.0)
             ("a"  1.0)  NIL ("" 1.0)  ("" 1.0)  NIL  NIL  NIL  NIL  NIL  NIL
             NIL NIL  NIL  NIL NIL  NIL  NIL  ("J" 1.0)  ("Ja" 2.0)  ("Jab" 3.0)
             ("Jabe" 4.0) ("Jaber" 5.0)  ("Jaberw" 6.0)  ("Jaberwo" 7.0)
             ("Jaberwoc" 8.0) ("Jaberwock" 9.0)  ("Jaberwocky" 10.0)
             ("Jaberwocky" 10.0)  NIL  ("a" 2.0)  ("ab" 3.0)  ("abe" 4.0)
             ("aber" 5.0)  ("aberw" 6.0)  ("aberwo" 7.0)  ("aberwoc" 8.0)
             ("aberwock" 9.0)  ("aberwocky" 10.0) ("aberwocky" 10.0)
             ("aberwocky" 10.0)  NIL  ("b" 3.0)  ("be" 4.0) ("ber" 5.0)
             ("berw" 6.0)  ("berwo" 7.0)  ("berwoc" 8.0)  ("berwock" 9.0)
             ("berwocky" 10.0)  ("berwocky" 10.0)  ("berwocky" 10.0)
             ("berwocky" 10.0)  NIL  ("e" 4.0)  ("er" 5.0)  ("erw" 6.0)
             ("erwo" 7.0)  ("erwoc" 8.0)  ("erwock" 9.0)  ("erwocky" 10.0)
             ("erwocky" 10.0)  ("erwocky" 10.0)  ("erwocky" 10.0)
             ("erwocky"  10.0)  NIL  ("r" 5.0)  ("rw" 6.0)  ("rwo" 7.0)
             ("rwoc" 8.0)
             ("rwock" 9.0)  ("rwocky" 10.0)  ("rwocky" 10.0)  ("rwocky" 10.0)
             ("rwocky" 10.0)  ("rwocky" 10.0)  ("rwocky" 10.0)  NIL  ("w" 6.0)
             ("wo" 7.0)  ("woc" 8.0) ("wock" 9.0)  ("wocky" 10.0)
             ("wocky" 10.0)  ("wocky" 10.0)  ("wocky" 10.0)  ("wocky" 10.0)
             ("wocky"   10.0)  ("wocky" 10.0)  NIL  ("o" 7.0) ("oc" 8.0)
             ("ock" 9.0)
             ("ocky" 10.0)  ("ocky" 10.0)  ("ocky" 10.0) ("ocky" 10.0)
             ("ocky" 10.0)  ("ocky" 10.0)  ("ocky" 10.0)  ("ocky" 10.0)  NIL
             ("c" 8.0)  ("ck" 9.0)  ("cky" 10.0)  ("cky" 10.0)  ("cky" 10.0)
             ("cky" 10.0)  ("cky" 10.0)  ("cky" 10.0)  ("cky" 10.0)
             ("cky"  10.0)  ("cky" 10.0)  NIL  ("k" 9.0)  ("ky" 10.0)
             ("ky" 10.0)
             ("ky" 10.0)  ("ky" 10.0)  ("ky" 10.0)  ("ky" 10.0)  ("ky" 10.0)
             ("ky" 10.0) ("ky" 10.0)  ("ky" 10.0)  NIL  ("y" 10.0)  ("y" 10.0)
             ("y" 10.0) ("y" 10.0)  ("y" 10.0)  ("y" 10.0)  ("y" 10.0)
             ("y"  10.0)  ("y" 10.0) ("y" 10.0)  ("y" 10.0)  NIL  ("" 10.0)
             (""  10.0)  ("" 10.0)  ("" 10.0)  ("" 10.0)  ("" 10.0)  ("" 10.0)
             (""  10.0)  ("" 10.0)  ("" 10.0)  ("" 10.0)  NIL)))
  (assert (equal
           (let ((result '()))
             (dolist (test '("" "a" "Jaberwocky") result)
               (dotimes (de (+ 2 (length test)))
                 (dolist (stop '("" "0123" "abcdeiouy"))
                   (push (ignore-errors
                           (multiple-value-list
                            (sch test (un-nombre de) stop))) result)))))
           '(NIL  NIL  NIL  ("" 0.0)  ("" 0.0)  ("" 0.0)  NIL  NIL  NIL
             ("a" 1.0)  ("a" 1.0)  ("" 0.0)  ("" 1.0)  ("" 1.0)  ("" 1.0)
             NIL  NIL NIL ("Jaberwocky" 10.0)  ("Jaberwocky" 10.0)  ("J" 1.0)
             ("aberwocky" 10.0)  ("aberwocky" 10.0)  ("" 1.0)  ("berwocky" 10.0)
             ("berwocky" 10.0)  ("" 2.0)  ("erwocky" 10.0)  ("erwocky" 10.0)
             ("" 3.0) ("rwocky" 10.0)  ("rwocky" 10.0)  ("rw" 6.0)
             ("wocky" 10.0) ("wocky" 10.0)  ("w" 6.0)  ("ocky" 10.0)
             ("ocky" 10.0)  ("" 6.0)  ("cky" 10.0) ("cky" 10.0)  ("" 7.0)
             ("ky" 10.0)  ("ky" 10.0) ("k" 9.0) ("y" 10.0)  ("y" 10.0)
             ("" 9.0)  ("" 10.0)  ("" 10.0) ("" 10.0))))
  (format t "DAT ~A~%" (dat))
  (format t "TEM (Time dependant, might fail on heavily loaded systems)~%")
  (assert (let ((times (loop repeat 10 do (sleep 1) collect (tem))))
            (every (lambda (x) (= 1 x)) (mapcar (function -) (cdr times) times))))
  (format t "DAT ~A~%" (dat)))


;;;; THE END ;;;;
