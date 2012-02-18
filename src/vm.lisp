;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               vm.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    LSE Virtual Machine
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-25 <PJB> Created.
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
(enable-byte-code-reader-macro)

;; 10 ALLER EN 20
;; 20 ALLER EN 10+I
;; 
;; #HASH( (10 => #((PUSHI 20)(POP&GO)))
;;        (20 => #((PUSHI 10)(PUSHV I)(ADD)(POP&GO)) )
       



;; Parameters by reference
;; Parameters by value       = Local variables
;; Global Variables
;; Local Variables


;; There are two kinds of variables: global variables, and local
;; variables.  Local variables are allocated in each
;; procedure/function frame.  But in both cases, instructions may
;; refer either ones depending on what is currently running.
;; Scoping rules are therefore quite complex.
;;
;; 10 A_1
;; 20 AFFICHER A
;; 30 SI A>0 ALORS &P SINON RETOUR
;; 40 TERMINER
;; 100 PROCEDURE P () LOCAL A
;; 105 A_0
;; 110 ALLER EN 20
;; 
;; should produce:
;; 
;; 1 0
;; 
;; which shows that AFFICHER A refered first the global variable, and then the LOCAL variable of P.


;; Furthermore, procedures may have parameters by reference or parameters
;; by value.  Parameters by value are basically the same as local
;; variables (only they're initialized from procedure arguments).
;; 
;; However, parameters by references mean that we need a way to reference
;; variables in their scope:

;; 10 Y=1
;; 20 N=1
;; 30 &F(Y)
;; 40 &F(N)
;; 50 TERMINER
;; 100 PROCEDURE &F(X) LOCAL Z
;; 110 Z_X-1
;; 120 &P(X,Y,Z)
;; 130 RETOUR
;; 200 PROCEDURE &P(A,B,C) ; * ALL PARAMETERS BY REFERENCE
;; 210 A_A+1; B_B+1; C_C+1
;; 220 * SHOULD INCREMENT Y OR N DEPENDING ON THE CALL FROM 30 OR 40,
;; 221 * AND Y, AND THE LOCAL VARIABLE Z.
;; 230 RETOUR



(defclass frame ()
  ((variables :initarg :variables :initform '() :accessor frame-variables)))

(defmethod print-object ((frame frame) stream)
  (print-unreadable-object (frame stream :identity t :type t)
    (format stream "with ~D variables: ~(~A~^ ~)"
            (length (frame-variables frame))
            (sort (mapcar (function variable-name) (frame-variables frame))
                  (function string<))))
  frame)

(defmethod find-variable ((frame frame) (ident symbol))
  (values (cdr (assoc ident (frame-variables frame))) frame))

(defmethod add-variable ((frame frame) var)
  (setf (frame-variables frame) (acons (variable-name var) var (frame-variables frame)))
  var)

(defmethod remove-variable ((frame frame) var)
  (setf (frame-variables frame) (remove (variable-name var) (frame-variables)
                                        :key (function car)))
  var)




;; SLOTS:

(defgeneric variable-value (slot))
(defgeneric (setf variable-value) (new-value slot))

(defgeneric variable-type (slot)
  (:documentation "
variable-type may be:
    :undefined
    procedure              -- can be only a reference-parameter.
    nombre
    chaine
    (vecteur dim)
    (tableau dim1 dim2)
"))

(defclass slot ()
  ())


;; NAMED-SLOTS:

(defgeneric variable-name (named-slot))
(defgeneric (setf variable-type) (new-type named-slot))

(defclass named-slot (slot)
  ((name :initarg :name :accessor variable-name)))

(defmethod print-object ((slot named-slot) stream)
  (print-unreadable-object (slot stream :identity t :type t)
    (format stream "~A" (variable-name slot)))
  slot)


;; LSE-VARIABLES;

(defclass lse-variable (named-slot)
  ((type :initarg :type :accessor variable-type :initform 'nombre)
   (value :initarg :value :accessor variable-value :initform :unbound)))


;; REFERENCE-PARAMETERS:

(defclass reference-parameter (named-slot)
  ((reference :initarg :reference :accessor parameter-reference)))

(defmethod variable-type ((par reference-parameter))
  (variable-type (parameter-reference par)))

(defmethod (setf variable-type) (new-type (par reference-parameter))
  (setf (variable-type (parameter-reference par)) new-type))

(defmethod variable-value ((par reference-parameter))
  (variable-value (parameter-reference par)))

(defmethod (setf variable-value) (new-value (par reference-parameter))
  (setf (variable-value (parameter-reference par)) new-value))


;; VECTEUR-REFS:

(defclass vecteur-ref (named-slot) ; for print-object; could just be slot
  ((vecteur :initarg :vecteur :reader reference-vecteur)
   (index :initarg :index :accessor reference-index)))

(defmethod print-object ((slot vecteur-ref) stream)
  (print-unreadable-object (slot stream :identity t :type t)
    (format stream "~A[~A]" (variable-name slot) (reference-index slot)))
  slot)

(defmethod variable-type ((self vecteur-ref))
  'nombre)

(defmethod variable-value ((self vecteur-ref))
  (aref (reference-vecteur self) (reference-index self)))

(defmethod (setf variable-value) (new-value (self vecteur-ref))
  (setf (aref (reference-vecteur self) (reference-index self)) new-value))



;; TABLEAU-REFS:

(defclass tableau-ref (named-slot) ; for print-object; could just be slot
  ((tableau :initarg :tableau :reader reference-tableau)
   (index1 :initarg :index1 :accessor reference-index1)
   (index2 :initarg :index2 :accessor reference-index2)))

(defmethod print-object ((slot tableau-ref) stream)
  (print-unreadable-object (slot stream :identity t :type t)
    (format stream "~A[~A,~A]" (variable-name slot) (reference-index1 slot) (reference-index2 slot)))
  slot)

(defmethod variable-type ((self tableau-ref))
  'nombre)

(defmethod variable-value ((self tableau-ref))
  (aref (reference-tableau self) (reference-index1 self) (reference-index2 self)))

(defmethod (setf variable-value) (new-value (self tableau-ref))
  (setf (aref (reference-tableau self) (reference-index1 self) (reference-index2 self)) new-value))




;;; The LSE Virtual Machine

(defclass lse-vm ()
  ((global-frame      :reader vm-global-frame
                      :initform (make-instance 'frame)
                      :type frame)
   (local-frame-stack :accessor vm-local-frame-stack
                      :initform '()
                      :type list )
   (code-vectors      :reader vm-code-vectors
                      :initform (make-hash-table :test (function eql) :size 256)
                      :documentation "Keys are line numbers, values are lists (line-number code-vector source-line).")
   (stack             :reader vm-stack
                      :initform (make-array '(256)
                                            :element-type t
                                            :adjustable t
                                            :fill-pointer 0)
                      :type vector)
   (pc.line           :accessor vm-pc.line
                      :initform 0
                      :type (integer 0))
   (pc.offset         :accessor vm-pc.offset
                      :initform 0
                      :type (integer 0))
   ;; current code vector:
   (code              :accessor vm-code
                      :initform  #(!next-line)
                      :type vector )

   (paused.pc.line)
   (paused.pc.offset)
   (paused.code)))




(defmethod print-object ((vm lse-vm) stream)
  (print-unreadable-object (vm stream :identity t :type t)
    (format stream "~D global variable~:*~P, ~D local frame~:*~P, ~D line~:*~P, current line ~D"
            (length (frame-variables (vm-global-frame vm)))
            (length (vm-local-frame-stack vm))
            (hash-table-count (vm-code-vectors vm))
            (vm-pc.line vm)))
  vm)


(defmethod vm-terminer ((vm lse-vm))
  (setf (vm-local-frame-stack vm)         '()
        (fill-pointer (vm-stack vm))      0
        (vm-pc.line vm)                   0
        (vm-pc.offset vm)                 0
        (vm-code vm)                      #(!next-line)
        (slot-value vm 'paused.pc.line)   nil
        (slot-value vm 'paused.pc.offset) nil
        (slot-value vm 'paused.code)      nil)
  vm)



#|



EXECUTER A PARTIR DE debut[,fin]

    Fait exécuter le programme courant de l'utilisateur à partir de la
    ligne de numéro 'debut' (si aucune ligne n'est numéroté 'debut',
    une erreur sera détectée).

    L'exécution se poursuivra jusqu'à ce qu'on arrive :

        - soit à la ligne de numéro 'fin' ; cette ligne ne sera pas
          exécutée, la console repassera dans l'état «moniteur» et
          affichera 'fin'.

        - soit à une des instructions LSE: PAUSE ou TERMINER ou à une
          erreur.  La console repassera dans l'état «moniteur» et
          affichera un message indiquant la cause de l'arrêt et le
          numéro de la ligne où il s'est produit.  Eventuellement, en
          cas d'arrêt dans une procédure, le numéro de la ligne où
          l'on avait appelé cette procédure.

        - L'utilisation de la touche ESC arrête également l'exécution.


REPRENDRE A PARTIR DE debut[,fin]

    Permet de reprendre l'exécution sur une ligne différente de celle
    où elle fut interrompue.

    Les paramètres 'debut' et 'fin' ont la même signification que ceux
    de la commande EXECUTER, mais la commande REPRENDRE ne change pas
    l'affectation des identificateurs.  Toutes les valeurs antérieures
    a l'interruption sont conservée.

    L'exécution du programme est toujours reprise au niveau principal
    (même si le programme avait été interrompu dans une procédure).

    
CONTINUER

    Permet de relancer l'exécution d'un programme momentanément
    interrompu par l'instruction PAUSE ou la touche d'interruption ESC.

    L'exécution reprend à l'endroit où elle fut arrêtée.


POURSUIVRE JUSQU'EN fin

    Relance l'exécution comme CONTINUER mais avec arrêt en ligne 'fin'.




T-1600:
TERMINE EN LIGNE ###
PAUSE EN LIGNE ###

Mitra-15:
TERMINE
PAUSE


Arrêt sur point d'arrêt (début de ligne).
  (EXECUTER A PARTIR DE debut[,fin])
  (REPRENDRE A PARTIR DE debut[,fin])
  (POURSUIVRE JUSQU'EN fin)
   Il n'y a qu'un seul point d'arrêt ('fin') au maximum.

Arrêt sur PAUSE

Arrêt sur TERMINER

Arrêt sur ESC


CONTINUER
POURSUIVRE JUSQU'EN fin
  Sur PAUSE ou ESC.
  Continue là où on en était.
  Poursuivre donne un point d'arrêt.


EXECUTER A PARTIR DE debut[,fin]
  Reprend au niveau principal.
  Efface la pile des procédures.
  Efface pas les variables globales.

REPRENDRE A PARTIR DE debut[,fin]
  Reprend au niveau principal.
  Efface la pile des procédures.
  N'efface pas les variables globales.


|#





(defmethod find-variable ((vm lse-vm) ident)
  "
RETURN: The variable; the frame it belongs to,
        or NIL if the variable cannot be found.
"
  (multiple-value-bind (var frame) (find-variable (vm-global-frame vm) ident)
    (if var
        (values var frame)
        (and (vm-local-frame-stack vm)
             (find-variable (vm-local-frame-stack vm))))))


(defmethod add-global-variable ((vm lse-vm) var)
  (add-variable (vm-global-frame vm) var))


(defun stack-push (val stack) (vector-push-extend val stack (length stack)))
(defun stack-pop  (stack)     (vector-pop stack))




(defun extremum (seq lessp)
  (let ((extremum nil))
    (map nil (lambda (item)
               (when (or (null extremum)
                       (funcall lessp extremum item))
                   (setf extremum item)))
         seq)
    extremum))

(defun maximum (seq &optional (lessp '<)) (extremum seq lessp))
(defun minimum (seq &optional (lessp '<)) (extremum seq (complement lessp)))




(defmethod get-program ((vm lse-vm) from to)
  "Returns a list of (lino line-source) sorted by increasing lino."
  (let ((lines '()))
    (maphash (lambda (lino code)
               (when (and (<= from lino) (or (null to) (<= lino to)))
                 (push (list lino (decompile-lse-line lino code)) lines)))
             (vm-code-vectors vm))
    (sort lines '< :key (function first))))


(defmethod erase-program ((vm lse-vm))
  "
DO:     Erase the program in the VM.
RETURN: vm
"
  (let ((codes (vm-code-vectors vm)))
   (maphash (lambda (k v) (remhash k codes)) codes)
   vm))

(defmethod replace-program ((vm lse-vm) program)
  "
DO:      Replace the program in the VM with PROGRAM.
PROGRAM: a list of (lino code-vector line-source).
RETURN: vm
"
  (erase-program vm)
  (let ((codes (vm-code-vectors vm)))
    (dolist (line program vm)
      (setf (gethash (first line) codes) line))))


(defmethod put-line ((vm lse-vm) lino code)
  (setf (gethash lino (vm-code-vectors vm)) code))


(defmethod erase-line-number ((vm lse-vm) lino)
  (remhash lino (vm-code-vectors vm)))


(defmethod maximum-line-number ((vm lse-vm))
  (maximum (HASH-TABLE-KEYS (vm-code-vectors vm))))


(defmethod minimum-line-number ((vm lse-vm))
  (minimum (HASH-TABLE-KEYS (vm-code-vectors vm))))










(defun afficher-nl      (vm rep) (format t "~V,,,VA" rep LF ""))
(defun afficher-cr      (vm rep) (format t "~V,,,VA" rep CR ""))
(defun afficher-space   (vm rep) (format t "~VA" rep ""))
(defun afficher-newline (vm rep) (format t "~V%" rep))

(defun afficher-chaine (vm rep val)
  (check-type rep nombre)
  (check-type val chaine)
  (loop :repeat (round rep) :do (princ val)))

(defun afficher-e (vm rep e d)
  )

(defun afficher-f (vm rep e d)
  )


(defun afficher-u (vm nexpr)
  ()
  )

;; (:LIRE&store        (op-0/1 LIRE&store))
;; (:LIRE&astore1        (op-1/1 LIRE&astore1))
;; (:LIRE&astore2        (op-2/1 LIRE&astore2))


(defun declare-chaine (vm ident)
  (check-type ident identificateur)
  (let ((var (find-variable vm ident)))
    (if var
        (if (member (variable-type var) '(:undefined chaine))
            (setf (variable-type  var) 'chaine
                  (variable-value var) :unbound)
            (lse-error "LA VARIABLE ~A EXISTE DEJA ET N'EST PAS UNE CHAINE" ident))
        (add-global-variable vm (make-instance 'lse-variable
                                      :name ident
                                      :type 'chaine
                                      :value :unbound)))))

(defun declare-tableau1 (vm dim ident)
  (check-type ident identificateur)
  (check-type dim nombre)
  (let ((dim (round dim))
        (var (find-variable vm ident)))
    (flet ((init-value (dim)
             (make-array dim :element-type 'nombre :initial-element 0.0f0)))
      (if var
          (if (or (eql (variable-type var) ':undefined)
                  (and (consp (variable-type var))
                       (member (first (variable-type var)) '(vecteur tableau))))
              (setf (variable-type  var) `(vecteur ,dim)
                    (variable-value var) (init-value dim))
              (lse-error "LA VARIABLE ~A EXISTE DEJA ET N'EST PAS UN TABLEAU" ident))
          (add-global-variable vm (make-instance 'lse-variable
                                        :name  ident
                                        :type  `(vecteur ,dim)
                                        :value (init-value dim)))))))

(defun declare-tableau2 (vm dim1 dim2 ident)
  (check-type ident identificateur)
  (check-type dim1 nombre)
  (check-type dim2 nombre)
  (let ((dim1 (round dim1))
        (dim2 (round dim2))
        (var (find-variable vm ident)))
    (flet ((init-value (dim1 dim2)
             (make-array (list dim1 dim2) :element-type 'nombre :initial-element 0.0f0)))
      (if var
          (if (or (eql (variable-type var) ':undefined)
                  (and (consp (variable-type var))
                       (member (first (variable-type var)) '(vecteur tableau))))
              (setf (variable-type  var) `(tableau ,dim1 ,dim2)
                    (variable-value var) (init-value dim1 dim2))
              (lse-error "LA VARIABLE ~A EXISTE DEJA ET N'EST PAS UN TABLEAU" ident))
          (add-global-variable vm (make-instance 'lse-variable
                                        :name  ident
                                        :type  `(tableau ,dim1 ,dim2)
                                        :value (init-value dim1 dim2)))))))


(defun declare-liberer (vm ident)
  "
NOTE: on ne peut pas liberer un parametre par reference.
"
  (check-type ident identificateur)
  (multiple-value-bind (var frame) (find-variable vm ident)
    (if var
        (etypecase var
          (lse-variable
           (remove-variable frame var))
          (reference-parameter
           (lse-error "LA VARIABLE ~A EST UN PARAMETRE PAR REFERENCE ET NE PEUT PAS ETRE LIBEREE" ident)))
        (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident))))



;; references are either lse-variable or vecteur-ref or tableau-ref.
;; see about procedure in parameters by reference?


(defun pushi (vm val)
  (stack-push val (vm-stack vm)))

(defun push-ref (vm ident)
  (check-type ident identificateur)
  (let ((var (find-variable vm ident)))
    (if var
        (etypecase var
          (lse-variable          (stack-push var (vm-stack vm)))
          (reference-parameter   (stack-push (reference-variable var) (vm-stack vm))))
        (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident))))

(defun push-val (vm ident)
  (check-type ident identificateur)
  (let ((var (find-variable vm ident)))
    (if var
        (let ((val (variable-value var)))
          (if (indefinip val)
              (lse-error "LA VARIABLE ~A N'A PAS ETE INITIALISEE")
              (stack-push val (vm-stack vm))))
        (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident))))


(defun deref (vm object)
  (typecase object
    (identificateur
     (let ((var (find-variable vm object)))
       (if var
           (let ((val (variable-value var)))
             (if (indefinip val)
                 (lse-error "LA VARIABLE ~A N'A PAS ETE INITIALISEE" var)
                 val))
           (lse-error "LA VARIABLE ~A N'EXISTE PAS" object))))
    (lse-variable
     (let ((val (variable-value object)))
       (if (indefinip val)
           (lse-error "LA VARIABLE ~A N'A PAS ETE INITIALISEE" object)
           val)))
    (otherwise
     object)))




(defun POP&ASTORE1 (vm val index ident)
  (check-type ident identificateur)
  (check-type index nombre)
  (check-type val nombre)
  (let ((index (round index))
        (var (find-variable vm ident)))
    (if var
        (if (and (consp (variable-type var))
                 (eql (first (variable-type var)) 'vecteur))
            (setf (aref (variable-value var) index) val)
            (lse-error "LA VARIABLE ~A N'EST PAS UN TABLEAU DE RANG 1" ident))
        (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident))))


(defun POP&ASTORE2 (vm val index1 index2 ident)
  (check-type ident identificateur)
  (check-type index1 nombre)
  (check-type index2 nombre)
  (check-type val nombre)
  (let ((index1 (round index1))
        (index2 (round index2))
        (var (find-variable vm ident)))
    (if var
        (if (and (consp (variable-type var))
                 (eql (first (variable-type var)) 'tableau))
            (setf (aref (variable-value var) index1 index2) val)
            (lse-error "LA VARIABLE ~A N'EST PAS UN TABLEAU DE RANG 2" ident))
        (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident))))


(defun POP&STORE (vm val ident)
  (check-type ident identificateur)
  (check-type val (or nombre chaine))
  (let ((var (find-variable vm ident)))
    (if var
        (case (variable-type var)
          (nombre
           (if (nombrep val)
               (setf (variable-value var) val)
               (lse-error "LA VARIABLE ~A N'EST PAS UNE CHAINE" ident)))
          (chaine
           (if (chainep val)
               (setf (variable-value var) val)
               (lse-error "LA VARIABLE ~A EST UNE CHAINE" ident)))
          (t
           (lse-error "LA VARIABLE ~A N'EST PAS ~A" ident
                      (if (nombrep val) "UN NOMBRE" "UNE CHAINE"))))
        (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident))))


(defun AREF1&PUSH-VAL (vm index ident)
  (check-type ident identificateur)
  (check-type index nombre)
  (let ((index (round index))
        (var (find-variable vm ident)))
    (if var
        (if (and (consp (variable-type var))
                 (eql (first (variable-type var)) 'vecteur))
            (if (<= 1 index (array-dimension (variable-value var) 0))
                (stack-push (aref (variable-value var) index) (vm-stack vm))
                (lse-error "DEPASSEMENT DES BORNES ~A[~A] EST INVALIDE" ident index))
            (lse-error "LA VARIABLE ~A N'EST PAS UN TABLEAU DE RANG 1" ident))
        (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident))))


(defun AREF2&PUSH-VAL (vm index1 index2 ident)
  (check-type ident identificateur)
  (check-type index1 nombre)
  (check-type index2 nombre)
  (let ((index1 (round index1))
        (index2 (round index2))
        (var (find-variable vm ident)))
    (if var
        (if (and (consp (variable-type var))
                 (eql (first (variable-type var)) 'tableau))
            (if (and (<= 1 index1 (array-dimension (variable-value var) 0))
                     (<= 1 index2 (array-dimension (variable-value var) 1)))
                (stack-push (aref (variable-value var) index1 index2) (vm-stack vm))
                (lse-error "DEPASSEMENT DES BORNES ~A[~A,~A] EST INVALIDE" ident index1 index2))
            (lse-error "LA VARIABLE ~A N'EST PAS UN TABLEAU DE RANG 2" ident))
        (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident))))


(defun AREF1&PUSH-REF (vm index ident)
  (check-type ident identificateur)
  (check-type index nombre)
  (let ((index (round index))
        (var (find-variable vm ident)))
    (if var
        (if (and (consp (variable-type var))
                 (eql (first (variable-type var)) 'vecteur))
            (if (<= 1 index (array-dimension (variable-value var) 0))
                (stack-push (make-instance 'vecteur-ref
                          :name ident
                          :vecteur (variable-value var)
                          :index index)
                      (vm-stack vm))
                (lse-error "DEPASSEMENT DES BORNES ~A[~A] EST INVALIDE" ident index))
            (lse-error "LA VARIABLE ~A N'EST PAS UN TABLEAU DE RANG 1" ident))
        (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident))))


(defun AREF2&PUSH-REF (vm index1 index2 ident)
  (check-type ident identificateur)
  (check-type index1 nombre)
  (check-type index2 nombre)
  (let ((index1 (round index1))
        (index2 (round index2))
        (var (find-variable vm ident)))
    (if var
        (if (and (consp (variable-type var))
                 (eql (first (variable-type var)) 'tableau))
            (if (and (<= 1 index1 (array-dimension (variable-value var) 0))
                     (<= 1 index2 (array-dimension (variable-value var) 1)))
                (stack-push (make-instance 'tableau-ref
                       :name ident
                       :tableau (variable-value var)
                       :index1 index1
                       :index2 index2)
                   (vm-stack vm))
                (lse-error "DEPASSEMENT DES BORNES ~A[~A,~A] EST INVALIDE" ident index1 index2))
            (lse-error "LA VARIABLE ~A N'EST PAS UN TABLEAU DE RANG 2" ident))
        (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident))))


(defun balways (vm offset)
  (incf (vm-pc.offset vm) offset))

(defun btrue (vm val offset)
  (if (booleenp val)
      (when (eq vrai val)
        (incf (vm-pc.offset vm) offset))
      (lse-error "LE TEST N'EST PAS UN BOOLEEN MAIS ~A" val)))

(defun bfalse (vm val offset)
  (if (booleenp val)
      (when (eq faux val)
        (incf (vm-pc.offset vm) offset))
      (lse-error "LE TEST N'EST PAS UN BOOLEEN MAIS ~A" val)))

(defun bnever (vm offset)
  (declare (ignore vm offset)))


;; (:faire-jusqu-a    (op-4/1 faire-jusqu-a))
;; (:faire-tant-que   (op-3/1 faire-tant-que))
;; (:tant-que         (op-1 tant-que))
;; 
(defun pause (vm)
  ;; TODO: Not correct.
  (setf (slot-value vm 'paused.pc.line)   (vm-pc.line vm)
        (slot-value vm 'paused.pc.offset) (vm-pc.offset vm)
        (slot-value vm 'paused.code)      (vm-code vm))
  (throw 'run-step-done nil))

(defun terminer (vm)
  ;; TODO: Not correct.
  (throw 'run-step-done nil))



;; (:next-line        (op-0 next-line))
;; (:goto             (op-1 goto))
;; (:call             (op-0/2 call))
;; (:retour           (op-0 retour))
;; (:retour-en        (op-1 retour-en))
;; (:result           (op-1 result))
;; 
;; (:garer            (op-2/1 garer))
;; (:charger                   (op-2 charger))
;; (:supprimer-enregistrement  (op-2 supprimer-enregistrement))
;; (:supprimer-fichier         (op-1 supprimer-fichier))
;; (:executer                  (op-2 executer))



                  

(defun comment (vm comment)
  (declare (ignore vm comment))
  (values))


(defvar *vm* nil "Current LSE-VM.")


(defun run-step (vm)
  (catch 'run-step-done
    (handler-case
        (let ((stack (vm-stack vm))
              (code  (vm-code  vm))
              (*vm*  vm))
          (flet ((spush  (val) (vector-push-extend val stack))
                 (spop   ()    (vector-pop stack))
                 (pfetch ()    (prog1 (aref code (vm-pc.offset vm))
                                 (incf (vm-pc.offset vm)))))
            (declare (inline spush spop pfetch))
            (macrolet ((op-1*  (op) `(spush (,op (deref vm (spop)))))
                       (op-2*  (op) `(spush (let ((b (spop))) (,op (deref vm (spop)) (deref vm b)))))
                       (op-0   (op) `(,op vm))
                       (op-1   (op) `(spush (,op vm (spop))))
                       (op-2   (op) `(spush (let ((b (spop))) (,op vm (spop) b))))
                       (op-3   (op) `(spush (let ((c (spop)) (b (spop))) (,op vm (spop) b c))))
                       (op-0/1 (op) `(,op vm (pfetch)))
                       (op-1/1 (op) `(spush (,op vm (spop) (pfetch))))
                       (op-2/1 (op) `(spush (let ((b (spop))) (,op vm (spop) b (pfetch)))))
                       (op-3/1 (op) `(spush (let ((c (spop)) (b (spop))) (,op vm (spop) b c (pfetch)))))
                       (op-4/1 (op) `(spush (let ((d (spop)) (c (spop)) (b (spop))) (,op vm (spop) b c d (pfetch)))))
                       (op-0/2 (op) `(,op vm (pfetch) (pfetch))))
              (let ((cop (pfetch)))
                (format t "Executing COP ~A~%" cop)
                (case cop

                  (!dup    (let ((a (spop))) (spush a) (spush a)))
                  
                  (!non    (op-1* non))
                  (!et     (op-2* et))
                  (!ou     (op-2* ou))

                  (!eg     (op-2* eg))
                  (!ne     (op-2* ne))
                  (!le     (op-2* le))
                  (!lt     (op-2* lt))
                  (!ge     (op-2* ge))
                  (!gt     (op-2* gt))

                  (!concat (op-2* concatenation))

                  (!neg    (op-1* neg))
                  (!add    (op-2* add))
                  (!sub    (op-2* sub))
                  (!mul    (op-2* mul))
                  (!div    (op-2* div))
                  (!pow    (op-2* pow))

                  (!afficher-e       (op-3 afficher-e))
                  (!afficher-f       (op-3 afficher-f))
                  (!afficher-cr      (op-1 afficher-cr))
                  (!afficher-nl      (op-1 afficher-nl))
                  (!afficher-space   (op-1 afficher-space))
                  (!afficher-newline (op-1 afficher-newline))
                  (!afficher-chaine  (op-2 afficher-chaine))
                  (!afficher-u       (op-1 afficher-u))
                  

                  (!LIRE&STORE       (op-0/1 LIRE&STORE))
                  (!LIRE&ASTORE1     (op-1/1 LIRE&ASTORE1))
                  (!LIRE&ASTORE2     (op-2/1 LIRE&ASTORE2))

                  (!next-line        (op-0 next-line))
                  (!retour           (op-0 retour))
                  (!retour-en        (op-1 retour-en))
                  (!result           (op-1 result))
                  (!goto             (op-1 goto))


                  (!tant-que                  (op-1 tant-que))
                  (!charger                   (op-2 charger))
                  (!supprimer-enregistrement  (op-2 supprimer-enregistrement))
                  (!supprimer-fichier         (op-1 supprimer-fichier))
                  (!executer                  (op-2 executer))
                  (!pause          (op-0 pause))
                  (!terminer       (op-0 terminer))
                  
                  (!AREF1&PUSH-REF (op-1/1 AREF1&PUSH-REF))
                  (!AREF1&PUSH-VAL (op-1/1 AREF1&PUSH-VAL))
                  (!AREF2&PUSH-REF (op-2/1 AREF2&PUSH-REF))
                  (!AREF2&PUSH-VAL (op-2/1 AREF2&PUSH-VAL))

                  (!POP&ASTORE1    (op-2/1 POP&ASTORE1))
                  (!POP&ASTORE2    (op-3/1 POP&ASTORE2))
                  (!POP&STORE      (op-1/1 POP&STORE))

                  (!push-ref       (op-0/1 push-ref))
                  (!push-val       (op-0/1 push-val))
                  (!pushi          (op-0/1 pushi))

                  (!chaine         (op-0/1 declare-chaine))
                  (!tableau1       (op-1/1 declare-tableau1))
                  (!tableau2       (op-2/1 declare-tableau2))
                  (!liberer        (op-0/1 declare-liberer))

                  
                  (!balways        (op-0/1 balways))
                  (!btrue          (op-1/1 btrue))
                  (!bfalse         (op-1/1 bfalse))
                  (!bnever         (op-0/1 bnever))

                  (!faire-jusqu-a  (op-4/1 faire-jusqu-a))

                  (!call           (op-0/2 call))
                  (!faire-tant-que (op-3/1 faire-tant-que))
                  (!garer          (op-2/1 garer))

                  (!comment        (op-0/1 comment))
                  (otherwise
                   (error 'lse-error
                          :format-control "INTERNE MACHINE VIRTUELLE: CODE OPERATION INCONNU ~S"
                          :format-arguments (list cop))))))))
      
      (error (err)
        (pause vm)
        ;; (io-format *task* "ERREUR: ~A~%" err)
        (error err)))
    t))


#||

(:liste-instruction inst . rest) --> inst (:list-instruction rest)
------------------------------------------------------------------------

gestion des erreurs:
- handler-case at each vm instruction.
  When an error occurs, report it and --> :pause


------------------------------------------------------------------------

(:Ligne-Programme numero instr...)
==> compile instr... and store vector in lino.

(:ligne-programme numero (:decl-procedure  decl-procedure))
==> compile decl-procedure; store vector in lino; store proc.ident in proctable.

(:liberer ident...) --> {:liberer ident}...
(:chaine  ident...) --> {:chaine ident}...
(:tableau adecl...)          --> adecl...
(:adecl ident expr)          --> expr :tableau1 ident
(:adecl ident expr.1 expr.2) --> expr.1 expr.2 :tableau2 ident


references:
In expressions:
(:aref ident expr)          --> expr :aref1&push-val ident
(:aref ident expr.1 expr.2) --> expr.1 expr.2 :aref2&push-val ident
(:vref ident)               --> :push-val ident
Needs to be preprocessed to:
(:aval ident expr)          --> expr :aref1&push-val ident
(:aval ident expr.1 expr.2) --> expr.1 expr.2 :aref2&push-val ident
(:vval ident)               --> :push-val ident

In user function calls:
(:aref ident expr)          --> expr :aref1&push-ref ident
(:aref ident expr.1 expr.2) --> expr.1 expr.2 :aref2&push-ref ident
(:vref ident)               --> :push-ref ident

For predefined functions, arguments are :push-val'ed and the optional
va references are :pop&store'd

(:fonction ident nil)     --> :call ident 0
(:fonction ident ref...) --> ref... :call ident N
(:appel    ident ref...) --> ref... :call ident N
==> store N in the new stack frame for the return.

(:procedure ident nil       nil)
(:procedure ident (fpid...) nil)
(:procedure ident nil       locid...)
(:procedure ident (fpid...) locid...)
--> :trap-proc-reached *
==> fpid inter locid == arguments par valeur    ==> copier
==> fpid diff  locid == arguments par reference
==> locid diff fpid  == variable locales --> table variable locale pour la proc.

(:resultat expression)  --> expression :result
(:retour)               --> :return
(:retour-en expression) --> expression :return&go

    :result             OR  :return&go      OR   :return
    -------------------     ---------------     ------------
    result    <-- :pop  OR  goto <-- :pop   OR   nothing

        return-pc <-- sf.return-pc
        next-sf   <-- sf.next-sf
        argcnt    <-- sf.argcnt
        sf        <-- next-sf
        :pop-sf
        :pop-n argcnt
                      
    :push  result              --                 --
    pc <-- return-pc       OR  pc <-- goto     or pc <-- return-pc



(:affectation ident expression)
   --> expression :pop&store ident
(:affectation (:aref ident expr) expression)
   --> expression expr :pop&astore1 ident
(:affectation (:aref ident expr.1 expr.2) expression)
   --> expression expr.1 expr.2 :pop&astore2 ident


(:lire liste-reference)
  --> :lire&sore ident
  --> expr :lire&astore1 ident
  --> expr.1 expr.2 :lire&astore2 ident


(:afficher nil expr...)      --> :pushi n expr... :afficher-u
(:afficher (form...) expr...)
(:afficher (form...))

(:rep-1)          --> :pushi 1
(:rep tok-numero) --> :pushi tok-numero
(:rep-var)        --> expression

(:spec-chaine rep  tok-litchaine) --> rep :pushi tok-litchaine :afficher-lit
(:spec-slash rep) --> rep :afficher-newline
(:spec-space rep) --> rep :afficher-space
(:spec-cr    rep) --> rep :afficher-cr
(:spec-nl    rep) --> rep :afficher-nl
(:spec-u     rep) --> rep expression... :afficher-u
(:spec-f     rep width precision)
   --> rep :pushi width :pushi precision expression... :affichier-f
(:spec-e     rep width precision)
   --> rep :pushi width :pushi precision expression... :affichier-e

(:aller-en  expression) --> expression :goto
(:si test then)       --> test :bfalse offset.t then  
(:si test then else)  --> test :bfalse offset.t then  :balways offset.e else 
;; same as :xi

(:terminer)  --> :terminer
(:pause)     --> :pause

(:faire-jusqu-a  lino ident init pas jusqua)
--> lino init pas jusqua :faire-jusqu-a ident

(:faire-tant-que lino ident init pas test)
--> lino init pas :faire-tant-que ident test

==> create a faire bloc. When we reach the end of each line, we must
    check for loop blocks available for this line. (kind of come from...).
    10 FAIRE 20 POUR I_1 JUSQUA 5
    15 AFFICHER I
    25 AFFICHER 'TERMINE';TERMINER


(:garer var enr fic) --> enr fic :garer var

;; TOOD: this creates the variables var and varstat too (TABLEAU, CHAINE or real)
(:charger  var enr fic)         --> enr fic :charger :pop&store var :pop
(:charger  var enr fic varstat) --> enr fic :charger :pop&store var :pop&store varstat

(:supprimer fic)          --> fic :supprimer-fic
(:supprimer fic enr)      --> fic enr :supprimer-enristrement
(:executer  fic)          --> fic (pushi 1) :executer
(:executer  fic lino)     --> fic lino      :executer

||#

;;;; THE END ;;;;
