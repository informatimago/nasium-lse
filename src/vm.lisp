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
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;****************************************************************************

(in-package "COM.INFORMATIMAGO.LSE")
(enable-byte-code-reader-macro)

;; 10 ALLER EN 20
;; 20 ALLER EN 10+I
;; 
;; #HASH( (10 => #((PUSHI 20)(POP&GO)))
;;        (20 => #((PUSHI 10)(PUSHV I)(ADD)(POP&GO)) )
       



;;; The LSE Virtual Machine

(defclass lse-vm ()
  ((state             :accessor vm-state
                      :initform :idle
                      :type (member :idle :running))
   (global-frame      :reader vm-global-frame
                      :initform (make-instance 'frame)
                      :type frame)
   (local-frame-stack :accessor vm-local-frame-stack
                      :initform '()
                      :type list )
   (stack             :reader vm-stack
                      :initform (make-array '(256)
                                            :element-type t
                                            :adjustable t
                                            :fill-pointer 0)
                      :type vector)
   (procedures        :reader vm-procedures
                      :initform (make-hash-table))
   (code-vectors      :reader vm-code-vectors
                      :initform (make-hash-table :test (function eql) :size 256)
                      :documentation "Keys are line numbers, values are lists (line-number code-vector source-line).")
   (pas-a-pas         :accessor vm-pas-a-pas
                      :initform nil
                      :type boolean)
   (trap-line         :accessor vm-trap-line
                      :initform nil
                      :type (or null (integer 0))
                      :documentation "The line number where execution will be paused (from EX, RE and PO commands).")
   (pc.line           :accessor vm-pc.line :reader vm-current-line
                      :initform 0
                      :type (integer 0))
   (pc.offset         :accessor vm-pc.offset
                      :initform 0
                      :type (integer 0))
   (code              :accessor vm-code 
                      :initform  #(!next-line)
                      :type vector
                      :documentation "The current code vector.")
   ;; When the VM is in pause, we keep the pc.line, pc.offset and code
   ;; in those slots, so that we can use the noram pc.line, pc.offset
   ;; and code slots to execute expressions in the "Machine de bureau"
   ;; mode.
   (paused.pc.line    :initform nil)
   (paused.pc.offset  :initform nil)
   (paused.code       :initform nil)))



(defmethod print-object ((vm lse-vm) stream)
  (print-unreadable-object (vm stream :identity t :type t)
    (format stream "~D global variable~:*~P, ~D local frame~:*~P, ~D line~:*~P, current line ~D"
            (length (frame-variables (vm-global-frame vm)))
            (length (vm-local-frame-stack vm))
            (hash-table-count (vm-code-vectors vm))
            (vm-pc.line vm)))
  vm)









(defmethod find-variable ((vm lse-vm) ident)
  "
RETURN: The variable; the frame it belongs to,
        or NIL if the variable cannot be found.
"
  (loop :for frame :in (vm-local-frame-stack vm)
        :do (multiple-value-bind (var frame) (find-variable frame ident)
              (when var
                (return-from find-variable (values var frame))))
        :finally (return (find-variable (vm-global-frame vm) ident))))


(defmethod add-global-variable ((vm lse-vm) var)
  (add-variable (vm-global-frame vm) var))


(defun stack-push (val stack) (vector-push-extend val stack (length stack)))
(defun stack-pop  (stack)     (vector-pop stack))





(defmethod get-program ((vm lse-vm) from to)
  "Returns a list of (lino line-source) sorted by increasing lino."
  (let ((lines '()))
    (maphash (lambda (lino code)
               (when (and (<= from lino) (or (null to) (<= lino to)))
                 (push code lines)))
             (vm-code-vectors vm))
    (sort lines '< :key (function code-line))))


(defmethod erase-program ((vm lse-vm))
  "
DO:     Erase the program in the VM.
RETURN: vm
"
  (let ((codes (vm-code-vectors vm)))
    (maphash (lambda (k v) (declare (ignore v)) (remhash k codes))
             codes))
  (let ((procedures (vm-procedures vm)))
    (maphash (lambda (k v) (declare (ignore v)) (remhash k procedures))
             procedures))
  (do-symbols (s "COM.INFORMATIMAGO.LSE.IDENTIFIERS")
    (unintern s))
  (setf (vm-state     vm) :idle
        (vm-pc.line   vm) 0
        (vm-pc.offset vm) 0
        (vm-code      vm) #(!next-line)
        (slot-value vm 'paused.pc.line)   nil
        (slot-value vm 'paused.pc.offset) nil
        (slot-value vm 'paused.code)      nil
        (fill-pointer (vm-stack vm))      0)
  vm)

(defmethod replace-program ((vm lse-vm) program)
  "
DO:      Replace the program in the VM with PROGRAM.
PROGRAM: a list of (lino code-vector line-source).
RETURN: vm
"
  (erase-program vm)
  (dolist (code program vm)
    (put-line vm (code-line code) code)))


(defmethod put-line ((vm lse-vm) lino code)
  (setf (gethash lino (vm-code-vectors vm)) code)
  (when (code-procedure code)
    (setf (gethash (procedure-name (code-procedure code)) (vm-procedures vm))
          (code-procedure code))))


(defmethod erase-line-number ((vm lse-vm) lino)
  (let ((code (gethash lino (vm-code-vectors vm))))
    (when (and code (code-procedure code))
      (remhash (procedure-name (code-procedure code)) (vm-procedures vm))))
  (remhash lino (vm-code-vectors vm)))



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


(defmethod maximum-line-number ((vm lse-vm))
  (maximum (HASH-TABLE-KEYS (vm-code-vectors vm))))


(defmethod minimum-line-number ((vm lse-vm))
  (minimum (HASH-TABLE-KEYS (vm-code-vectors vm))))



(defun line-length-maximum ()
  #+LSE-T1600 80
  #+LSE-MITRA-15 80
  #-(or LSE-T1600 LSE-MITRA-15) 65535)

(defun line-number-maximum ()
  #+LSE-T1600 255
  #+LSE-MITRA-15 250
  #-(or LSE-T1600 LSE-MITRA-15) 65535)

(defun line-number-valid-p (linum)
  (and (integerp linum) (<= 1 linum (line-number-maximum))))



;; EXECUTER A PARTIR DE debut[,fin]
;; 
;;     Fait exécuter le programme courant de l'utilisateur à partir de la
;;     ligne de numéro 'debut' (si aucune ligne n'est numéroté 'debut',
;;     une erreur sera détectée).
;; 
;;     L'exécution se poursuivra jusqu'à ce qu'on arrive :
;; 
;;         - soit à la ligne de numéro 'fin' ; cette ligne ne sera pas
;;           exécutée, la console repassera dans l'état «moniteur» et
;;           affichera 'fin'.
;; 
;;         - soit à une des instructions LSE: PAUSE ou TERMINER ou à une
;;           erreur.  La console repassera dans l'état «moniteur» et
;;           affichera un message indiquant la cause de l'arrêt et le
;;           numéro de la ligne où il s'est produit.  Eventuellement, en
;;           cas d'arrêt dans une procédure, le numéro de la ligne où
;;           l'on avait appelé cette procédure.
;; 
;;         - L'utilisation de la touche ESC arrête également l'exécution.
;; 
;; 
;; REPRENDRE A PARTIR DE debut[,fin]
;; 
;;     Permet de reprendre l'exécution sur une ligne différente de celle
;;     où elle fut interrompue.
;; 
;;     Les paramètres 'debut' et 'fin' ont la même signification que ceux
;;     de la commande EXECUTER, mais la commande REPRENDRE ne change pas
;;     l'affectation des identificateurs.  Toutes les valeurs antérieures
;;     a l'interruption sont conservée.
;; 
;;     L'exécution du programme est toujours reprise au niveau principal
;;     (même si le programme avait été interrompu dans une procédure).
;; 
;;     
;; CONTINUER
;; 
;;     Permet de relancer l'exécution d'un programme momentanément
;;     interrompu par l'instruction PAUSE ou la touche d'interruption ESC.
;; 
;;     L'exécution reprend à l'endroit où elle fut arrêtée.
;; 
;; 
;; POURSUIVRE JUSQU'EN fin
;; 
;;     Relance l'exécution comme CONTINUER mais avec arrêt en ligne 'fin'.
;; 
;; 
;; 
;; 
;; T-1600:
;; TERMINE EN LIGNE ###
;; PAUSE EN LIGNE ###
;; 
;; Mitra-15:
;; TERMINE
;; PAUSE
;; 
;; 
;; Arrêt sur point d'arrêt (début de ligne).
;;   (EXECUTER A PARTIR DE debut[,fin])
;;   (REPRENDRE A PARTIR DE debut[,fin])
;;   (POURSUIVRE JUSQU'EN fin)
;;    Il n'y a qu'un seul point d'arrêt ('fin') au maximum.
;; 
;; Arrêt sur PAUSE
;; 
;; Arrêt sur TERMINER
;; 
;; Arrêt sur ESC
;;   Implémenté avec la condition USER-INTERRUPT signalée par YIELD-SIGNALS.
;; 
;; CONTINUER
;; POURSUIVRE JUSQU'EN fin
;;   Sur PAUSE ou ESC.
;;   Continue là où on en était.
;;   Poursuivre donne un point d'arrêt.
;; 
;; 
;; EXECUTER A PARTIR DE debut[,fin]
;;   Reprend au niveau principal.
;;   Efface la pile des procédures.
;;   Efface pas les variables globales.
;; 
;; REPRENDRE A PARTIR DE debut[,fin]
;;   Reprend au niveau principal.
;;   Efface la pile des procédures.
;;   N'efface pas les variables globales.



(defmethod vm-pausedp ((vm lse-vm))
  (slot-value vm 'paused.pc.line))


(defmethod vm-pause ((vm lse-vm))
  (when (eql (vm-state vm) :running)
    (setf (vm-state vm) :idle
          (slot-value vm 'paused.pc.line)   (vm-pc.line vm)  
          (slot-value vm 'paused.pc.offset) (vm-pc.offset vm)
          (slot-value vm 'paused.code)      (vm-code vm)))
  vm)

(defmethod vm-unpause ((vm lse-vm))
  (when (vm-pausedp vm)
    (setf (vm-state vm) :running
          (vm-pc.line   vm) (slot-value vm 'paused.pc.line)  
          (vm-pc.offset vm) (slot-value vm 'paused.pc.offset)
          (vm-code      vm) (slot-value vm 'paused.code)
          (slot-value vm 'paused.pc.line)   nil
          (slot-value vm 'paused.pc.offset) nil
          (slot-value vm 'paused.code)      nil))
  vm)

(defmethod vm-line-exist-p ((vm lse-vm) lino)
  (gethash lino (vm-code-vectors vm)))

(defmethod vm-goto ((vm lse-vm) lino)
  (let ((line (gethash lino (vm-code-vectors vm))))
    (if line
        (let ((frame (or (first (vm-local-frame-stack vm))
                         (vm-global-frame vm))))
          (setf (vm-state     vm) :running
                (vm-pc.line   vm) lino
                (vm-pc.offset vm) 0
                (vm-code      vm) (code-vector line)
                (slot-value vm 'paused.pc.line)   nil
                (slot-value vm 'paused.pc.offset) nil
                (slot-value vm 'paused.code)      nil
                (fill-pointer (vm-stack vm)) (frame-stack-pointer frame))
          (when (or (vm-pas-a-pas vm)
                    (eql lino (vm-trap-line vm)))
            ;; TODO: Not a pause message, but an EXECUTION JUSQU'A message.
            ;; Furthermore if we are in a procedure, we must print the
            ;; line number of the procedure call.
            ;; To get the PAUSE message
            (pause vm)))
        (error-bad-line lino)))
  vm)

(defmethod vm-reset-variables ((vm lse-vm))
  (setf (slot-value vm 'global-frame) (make-instance 'frame))
  (vm-reset-stacks vm))

(defmethod vm-reset-stacks ((vm lse-vm))
  (setf (vm-local-frame-stack vm)         '()
        (fill-pointer (vm-stack vm))      0)
  (setf
   (vm-pc.line vm)                   0
   (vm-pc.offset vm)                 0
   (vm-code vm)                      #(!next-line)
   (slot-value vm 'paused.pc.line)   nil
   (slot-value vm 'paused.pc.offset) nil
   (slot-value vm 'paused.code)      nil)
  vm)

(defmethod vm-terminer ((vm lse-vm))
  (setf (vm-state vm) :idle)
  vm)

(defmethod vm-run ((vm lse-vm))
  (loop
    :while (eql (vm-state vm) :running)
    :do (run-step vm))
  vm)





(defun afficher-nl      (vm rep)
  (declare (ignore vm))
  (io-line-feed *task* rep))

(defun afficher-cr      (vm rep)
  (declare (ignore vm rep))
  (io-carriage-return *task*))

(defun afficher-space   (vm rep)
  (declare (ignore vm))
  (io-format *task* "~VA" rep ""))

(defun afficher-newline (vm rep)
  (declare (ignore vm))
  (io-new-line *task* rep))

(defun afficher-chaine (vm rep val)
  (declare (ignore vm))
  (check-type rep (or (integer 1) nombre))
  (check-type val chaine)
  (io-format *task* "~A" 
             (with-output-to-string (out)
               (loop
                 :repeat (round rep)
                 :do (princ val out)))))



;; NOTE: AFFICHER[Fn.m,En.m]VECT,TABL are specific to T1600.
;;       We should have some #+LSE-T1600 arround here…

(defun afficher-with-format (vm ctrl value)
  (etypecase value
    ((or integer nombre chaine)
     (if (functionp ctrl)
         (funcall ctrl vm value)
         (io-format *task* ctrl value)))
    (vector
     (loop
       :for i :below (length value)
       :initially (afficher-newline vm 1)
       :do (if (functionp ctrl)
               (progn (funcall ctrl vm (aref value i))
                      (io-format *task* " "))
               (io-format *task* "~? " ctrl (list (aref value i))))))
    (array
     (loop
       :for i :below (array-dimension value 0)
       :do (loop
             :for j :below (array-dimension value 1)
             :initially (afficher-newline vm 1)
             :do  (if (functionp ctrl)
                      (progn (funcall ctrl vm (aref value i j))
                             (io-format *task* " "))
                      (io-format *task* "~? " ctrl (list (aref value i j)))))))))


(defun afficher-e (vm val e d)
  (let* ((val (deref vm val))
         (w (+ e 1 d 4))
         (ctrl (format nil "~~~A,~A,2,,,,'EE" w d)))
    (afficher-with-format vm ctrl val)))


(defun afficher-f (vm val e d)
  (let* ((val (deref vm val))
         (w (+ e 1 d))
         (ctrl (format nil "~~~A,~AF" w d)))
    (afficher-with-format vm ctrl val)))


(defun afficher-u (vm value)
  (let ((value (deref vm value)))
    (etypecase value
      ((or integer nombre)
       (io-format *task* (if (or (zerop value)
                                 (and (<= 1e-3 (abs value)) (< (abs value) 1e6)))
                             "~A "          
                             "~,,2,,,,'EE ")
                  (let ((tvalue (truncate value)))
                    (if (= tvalue value)
                        tvalue
                        value))))
      (chaine
       (io-format *task*  "~A" value))
      ((or vector array)
       (afficher-with-format vm (function afficher-u) value)))))



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
  (check-type dim (or integer nombre))
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
  (check-type dim1 (or integer nombre))
  (check-type dim2 (or integer nombre))
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
          (reference-parameter   (stack-push (referenced-variable var) (vm-stack vm))))
        ;; (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident)
        (stack-push (add-global-variable vm (make-instance 'lse-variable :name ident))
                    (vm-stack vm)))))

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
    ((or lse-variable vecteur-ref tableau-ref)
     (let ((val (variable-value object)))
       (if (indefinip val)
           (typecase object
             (lse-variable (lse-error "LA VARIABLE ~A N'A PAS ETE INITIALISEE"
                                      object))
             (vecteur-ref  (lse-error "~A[~A] N'A PAS ETE INITIALISEE"
                                      (variable-name object)
                                      (reference-index object)))
             (tableau-ref  (lse-error "~A[~A,~A] N'A PAS ETE INITIALISEE"
                                      (variable-name object)
                                      (reference-index1 object)
                                      (reference-index2 object))))
           val)))
    (otherwise
     object)))



(defun POP&STORE (vm val ident)
  (check-type ident identificateur)
  (let ((val (deref vm val)))
    (check-type val (or integer nombre chaine))
    (let ((var (find-variable vm ident)))
      (if var
          (case (variable-type var)
            (nombre
             (if (chainep val)
                 (lse-error "LA VARIABLE ~A N'EST PAS UNE CHAINE" ident)
                 (setf (variable-value var) (un-nombre val))))
            (chaine
             (if (chainep val)
                 (setf (variable-value var) val)
                 (lse-error "LA VARIABLE ~A EST UNE CHAINE" ident)))
            (t
             (lse-error "LA VARIABLE ~A N'EST PAS ~A" ident
                        (if (nombrep val) "UN NOMBRE" "UNE CHAINE"))))
          (typecase val
            ((or integer nombre)
             (add-global-variable vm (make-instance 'lse-variable
                                         :name  ident
                                         :value (un-nombre val))))
            (otherwise
             (lse-error "LA VARIABLE ~A N'EST PAS DECLAREE COMME CHAINE." ident)))))))



(defun POP&ASTORE1 (vm val index ident)
  (check-type ident identificateur)
  (check-type index (or integer nombre))
  (let ((val (deref vm val)))
   (check-type val (or integer nombre))
   (let ((index (round index))
         (var (find-variable vm ident)))
     (if var
         (if (and (consp (variable-type var))
                  (eql (first (variable-type var)) 'vecteur))
             (setf (aref (variable-value var) (1- index)) (un-nombre val))
             (lse-error "LA VARIABLE ~A N'EST PAS UN TABLEAU DE RANG 1" ident))
         (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident)))))


(defun POP&ASTORE2 (vm val index1 index2 ident)
  (check-type ident identificateur)
  (check-type index1 (or integer nombre))
  (check-type index2 (or integer nombre))
  (let ((val (deref vm val)))
   (check-type val (or integer nombre))
   (let ((index1 (round index1))
         (index2 (round index2))
         (var (find-variable vm ident)))
     (if var
         (if (and (consp (variable-type var))
                  (eql (first (variable-type var)) 'tableau))
             (setf (aref (variable-value var) (1- index1) (1- index2)) (un-nombre val))
             (lse-error "LA VARIABLE ~A N'EST PAS UN TABLEAU DE RANG 2" ident))
         (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident)))))



(defun beep (vm)
  (declare (ignore vm))
  (io-bell *task*))


;; We need lire&store operators since we need to know the type of
;; variable to know what to read (string, number, vector or array).


(defun lire&store (vm ident)
  (check-type ident identificateur)
  (let ((var (find-variable vm ident)))
    (if var
        (case (variable-type var)
          ((nombre)
           (setf (variable-value var) (un-nombre (io-read-number *task*))))
          ((chaine)
           (setf (variable-value var) (io-read-string *task*)))
          (t
           (loop
             :for i :below (array-total-size (variable-value var))
             :for val = (io-read-number *task*)
             :do (setf (row-major-aref (variable-value var) i) (un-nombre val)))))
        (let ((val (io-read-number *task*)))
          (add-global-variable vm (make-instance 'lse-variable
                                      :name  ident
                                      :value (un-nombre val)))))))


(defun lire&astore1 (vm index ident)
  (check-type ident identificateur)
  (check-type index (or integer nombre))
  (let ((index (round index))
        (var (find-variable vm ident)))
    (if var
        (if (and (consp (variable-type var))
                 (eql (first (variable-type var)) 'vecteur))
            (if (<= 1 index (array-dimension (variable-value var) 0))
                (let ((val (io-read-number *task*)))
                  (setf (aref (variable-value var) (1- index)) (un-nombre val)))
                (lse-error "DEPASSEMENT DES BORNES ~A[~A] EST INVALIDE" ident index))
            (lse-error "LA VARIABLE ~A N'EST PAS UN TABLEAU DE RANG 1" ident))
        (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident))))


(defun lire&astore2 (vm index1 index2 ident)
  (check-type ident identificateur)
  (check-type index1 (or integer nombre))
  (check-type index2 (or integer nombre))
  (let ((index1 (round index1))
        (index2 (round index2))
        (var (find-variable vm ident)))
    (if var
        (if (and (consp (variable-type var))
                 (eql (first (variable-type var)) 'tableau))
            (if (and (<= 1 index1 (array-dimension (variable-value var) 0))
                     (<= 1 index2 (array-dimension (variable-value var) 1)))
                (let ((val (io-read-number *task*)))
                  (setf (aref (variable-value var) (1- index1) (1- index2)) (un-nombre val)))
                (lse-error "DEPASSEMENT DES BORNES ~A[~A,~A] EST INVALIDE" ident index1 index2))
            (lse-error "LA VARIABLE ~A N'EST PAS UN TABLEAU DE RANG 2" ident))
        (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident))))



(defun AREF1&PUSH-VAL (vm index ident)
  (check-type ident identificateur)
  (check-type index (or integer nombre))
  (let ((index (round index))
        (var (find-variable vm ident)))
    (if var
        (if (and (consp (variable-type var))
                 (eql (first (variable-type var)) 'vecteur))
            (if (<= 1 index (array-dimension (variable-value var) 0))
                (stack-push (aref (variable-value var) (1- index)) (vm-stack vm))
                (lse-error "DEPASSEMENT DES BORNES ~A[~A] EST INVALIDE" ident index))
            (lse-error "LA VARIABLE ~A N'EST PAS UN TABLEAU DE RANG 1" ident))
        (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident))))


(defun AREF2&PUSH-VAL (vm index1 index2 ident)
  (check-type ident identificateur)
  (check-type index1 (or integer nombre))
  (check-type index2 (or integer nombre))
  (let ((index1 (round index1))
        (index2 (round index2))
        (var (find-variable vm ident)))
    (if var
        (if (and (consp (variable-type var))
                 (eql (first (variable-type var)) 'tableau))
            (if (and (<= 1 index1 (array-dimension (variable-value var) 0))
                     (<= 1 index2 (array-dimension (variable-value var) 1)))
                (stack-push (aref (variable-value var) (1- index1) (1- index2)) (vm-stack vm))
                (lse-error "DEPASSEMENT DES BORNES ~A[~A,~A] EST INVALIDE" ident index1 index2))
            (lse-error "LA VARIABLE ~A N'EST PAS UN TABLEAU DE RANG 2" ident))
        (lse-error "LA VARIABLE ~A N'EXISTE PAS" ident))))


(defun AREF1&PUSH-REF (vm index ident)
  (check-type ident identificateur)
  (check-type index (or integer nombre))
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
  (check-type index1 (or integer nombre))
  (check-type index2 (or integer nombre))
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
  (cond
    ((booleen-p val)
     (when (eq vrai val)
       (incf (vm-pc.offset vm) offset)))
    ((eq t val)
     (incf (vm-pc.offset vm) offset))
    ((null val))
    (t
     (lse-error "LE TEST N'EST PAS UN BOOLEEN MAIS ~A" val))))

(defun bfalse (vm val offset)
  (cond
    ((booleen-p val)
     (when (eq faux val)
       (incf (vm-pc.offset vm) offset)))
    ((eq t val))
    ((null val)
     (incf (vm-pc.offset vm) offset))
    (t    
     (lse-error "LE TEST N'EST PAS UN BOOLEEN MAIS ~A" val))))

(defun bnever (vm offset)
  (declare (ignore vm offset)))





;; For the FAIRE instructions, the line number is inclusive, and the loop
;; goes back from the end of that line.  This is processed in the
;; NEXT-LINE function.  Also, in VM-GOTO, we must check when we exit a
;; loop.  Therefore we implement FAIRE loops by stacking loop-frames for
;; embedded loops.  The loop stack is in the frames (global or local
;; frames) so they're automatically unwound when we return from a
;; procedure.
;; Note: the end-line-number can be the same for several embedded loops.


(defclass loop-frame ()
  ((start-line-number :initarg :start-line-number
                      :reader loop-start-line-number)
   (start-offset      :initarg :start-offset
                      :reader loop-offset
                      :documentation "The code-vector offset for the start of the loop.")
   (end-line-number   :initarg :end-line-number
                      :reader loop-end-line-number)
   (step              :initarg :step
                      :reader loop-step)
   (variable          :initarg :variable
                      :reader loop-variable)))

(defmethod print-object ((self loop-frame) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~D FAIRE ~D POUR ~A PAS ~S TANT QUE ..."
            (loop-start-line-number self)
            (loop-end-line-number self)
            (loop-variable self)
            (loop-step self)))
  self)



(defclass loop-jusqua (loop-frame)
  ((limit             :initarg :limit
                      :reader loop-limit)))

(defmethod print-object ((self loop-jusqua) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream "~D FAIRE ~D POUR ~A PAS ~S JUSQUA ~S"
            (loop-start-line-number self)
            (loop-end-line-number self)
            (loop-variable self)
            (loop-step self)
            (loop-limit self)))
  self)


(defun faire (vm loop-class lino init pas limit ident)
  (let ((line (gethash lino (vm-code-vectors vm)))
        (var (find-variable vm ident)))
    (if line
        (progn
          (let ((init (if limit
                          (- init pas)
                          init)))
            (if var
                (if (equal (variable-type var) 'nombre)
                    (setf (variable-value var) init)
                    (lse-error "LA VARIABLE DE BOUCLE FAIRE ~A EXISTE, MAIS N'EST PAS UNE VARIABLE ARITHMETIQUE" ident))
                (setf var (add-global-variable vm (make-instance 'lse-variable
                                                      :name  ident
                                                      :value init)))))
          (let ((stack (loop-stack vm)))
            (when (and stack
                       (or (<= lino (loop-start-line-number (first stack)))
                           (< (loop-end-line-number (first stack)) lino)))
              (error 'lse-error
                     :backtrace (or #+ccl (ccl::backtrace-as-list))
                     :line-number (vm-pc.line vm)
                     :format-control "BOUCLE FAIRE ~D POUR ~A ENCHEVETREE AVEC LA BOUCHE FAIRE ~D POUR ~A DE LA LIGNE ~D"
                     :format-arguments (list lino var (loop-end-line-number (first stack))
                                             (loop-variable (first stack))
                                             (loop-start-line-number (first stack))))))
          (let ((loop (apply (function make-instance) loop-class
                             :start-line-number (vm-pc.line vm)
                             :start-offset (vm-pc.offset vm)
                             :end-line-number lino
                             :step pas
                             :variable var
                             (when limit (list :limit limit)))))
            (push loop (loop-stack vm))
            (when limit (test-end-of-loop vm loop))))
        (error-bad-line lino))))


(defun faire-jusqu-a (vm lino init pas limit ident)
  (faire vm 'loop-jusqua lino init pas limit ident))


(defmethod test-end-of-loop (vm (loop loop-jusqua))
  "Returns whether the loop goes on (and we've already jumped to the start of the loop)."
  (with-slots (variable step limit start-line-number start-offset end-line-number) loop
    (incf (variable-value variable) step)
    (if (if (minusp step)
                (> limit (variable-value variable))
                (< limit (variable-value variable)))
        (progn
          ;; end of loop, we exit it.
          ;; #+debugging (io-format *task* "~&END OF LOOP ~S~&" loop)
          (pop (loop-stack vm))
          nil)
        ;; loop over:
        (setf (vm-pc.line vm) start-line-number
              (vm-pc.offset vm) start-offset
              (vm-code vm) (code-vector (gethash start-line-number (vm-code-vectors vm)))))))




(defclass loop-tant-que (loop-frame)
  ())


(defun faire-tant-que (vm lino init pas ident)
  (faire vm 'loop-tant-que lino init pas nil ident))


(defun tant-que (vm test)
  (let ((stack (loop-stack vm)))
    (if stack
        (with-slots (end-line-number) (first stack)
          (unless (eql (le-booleen test) vrai)
            ;; end of loop, we exit it.
            (pop (loop-stack vm))
            (vm-goto vm (following-line vm end-line-number))))
        (lse-error "INTERNE: CODE OPERATION TANT-QUE SANS BOUCLE FAIRE ACTIVE."))))


(defmethod test-end-of-loop (vm (loop loop-tant-que))
  (with-slots (variable step start-line-number start-offset) loop
    (incf (variable-value variable) step)
    ;; Always loop over, there's a tant-que instruction at the start-offset.
    (setf (vm-pc.line vm) start-line-number
          (vm-pc.offset vm) start-offset
          (vm-code vm) (code-vector (gethash start-line-number (vm-code-vectors vm))))))


(defun following-line (vm lino)
  (let ((max  (maximum-line-number vm)))
    (loop
      :while (and (<= lino max)
                  (not (vm-line-exist-p vm lino)))
      :do (incf lino))
    (if (<= lino max)
        lino
        (error 'lse-error
               :backtrace (or #+ccl (ccl::backtrace-as-list))
               :line-number (vm-pc.line vm)
               :format-control "FIN DU PROGRAMME ATTEINTE; IL MANQUE UNE INSTRUCTION TERMINER"
               :format-arguments (list (vm-pc.line vm))))))


(defun next-line (vm)
  (let ((stack (loop-stack vm)))
    ;; #+debugging (io-format *task* "~&~S~%" stack)
    (if (and stack
             (= (loop-end-line-number (first stack)) (vm-pc.line vm)))
        (if (test-end-of-loop vm (first stack))
            ;; we have jumped to the start of the loop
            nil
            ;; try again, we have finished the current loop, and might
            ;; have another embedded loop ending on the same line.
            (next-line vm))
        (vm-goto vm (following-line vm (1+ (vm-pc.line vm)))))))




(defun pause (vm)
  (io-format *task*
             #-LSE-MITRA-15 "~%PAUSE EN LIGNE ~3,'0D~%"
             #+LSE-MITRA-15 "~%PAUSE~%" (vm-pc.line vm))
  (vm-pause vm)
  (throw 'run-step-done nil))

(defun terminer (vm)
  (io-format *task*
             #-LSE-MITRA-15 "~%TERMINE EN LIGNE ~3,'0D~%"
             #+LSE-MITRA-15 "~%TERMINE~%" (vm-pc.line vm))
  (vm-terminer vm)
  (throw 'run-step-done nil))

(defun stop (vm)
  (vm-terminer vm)
  (throw 'run-step-done nil))


(defun goto (vm lino)
  (let ((line (gethash lino (vm-code-vectors vm)))
        (stack (loop-stack vm)))
    (cond
      ((null line)
       (error-bad-line lino))
      (stack
       (if (or (< (loop-start-line-number (first stack)) lino)
               (<= lino (loop-end-line-number (first stack))))
           (vm-goto vm lino)
           (progn
             ;; When we GO TO a line outside of the current loop, we unwind it.
             (pop (loop-stack vm))
             ;; and try again (there may be several embedded loops)
             (goto vm lino))))
      (t
       (vm-goto vm lino)))))



#+lse-extensions
(defunction xit (ea)
  "Sortie de boucles"
  "XIT(N)

Cette procédure permet de sortir d'un ou de plusieurs niveaux de boucles.

Si N=0 alors cet procédure ne fait rien.

Si N=1 alors on sort de la boucle courante.

Si N>1 alors on sort de N boucle imbriquées.

Si N est supérieur au nombre de boucles imbriquées, on sort de toutes les boucles.

Voir: FAIREJUSQUA, FAIRETANTQUE"
  (declare (ignore ea))
  (error 'pas-implemente :what 'xit))




(defparameter *primitive-functions*
  (hashtable :elements '((id::ent (ent 1 1))
                         (id::neg (neg 1 1))
                         (id::abs (abso 1 1))
                         (id::exp (expo 1 1))
                         (id::sin (sinu 1 1))
                         (id::cos (cosi 1 1))
                         (id::atg (atg 1 1))
                         (id::rac (rac 1 1))
                         (id::lgn (lgn 1 1))
                         (id::ale (ale 1 1))
                         (id::tem (tem 0 0))
                         #+lse-extensions (id::xit (xit 1 1))
                         #+lse-extensions (id::att (att 0 0))
                         #+lse-extensions #-(and) (id::dis  (dis 1 1))
                         #+lse-extensions (id::etl (etl 2 2))
                         #+lse-extensions (id::oul (oul 2 2))
                         #+lse-extensions (id::oxl (oxl 2 2))
                         (id::lgr (lgr 1 1))
                         (id::pos (pos 3 3))
                         (id::eqn (eqn 1 2))
                         (id::eqc (eqc 1 1))
                         (id::cca (cca 1 1))
                         (id::cnb (cnb 2 3))
                         (id::sch (sch 3 4))
                         (id::skp (skp 2 3))
                         (id::ptr (ptr 2 3))
                         (id::grl (grl 2 3))
                         (id::dat (dat 0 0))
                         #+lse-unix (id::env (env 1 1))
                         #+debugging (id::lisp (lisp-eval 1 3)))))





(defun call (vm call-type procident nargs)
  (check-type procident identificateur)
  (check-type nargs (integer 0))
  (let ((entry (gethash procident *primitive-functions*)))
    (if entry
        ;; primitive functions
        (destructuring-bind (fun minargs maxargs) entry
          (if (<= minargs nargs maxargs)
              (let ((stack  (vm-stack vm)))
                (stack-push (apply fun (loop
                                         :with args = '()
                                         :repeat nargs
                                         :do (push (stack-pop stack) args)
                                         :finally (return args)))
                            stack))
              (lse-error (if (< nargs minargs)
                             "NOMBRE D'ARGUMENTS POUR ~:@(~A~) INSUFFISANT (MINIMUM ~D, DONNE~:@(~P~) ~:*~D)"
                             "NOMBRE D'ARGUMENTS POUR ~:@(~A~) TROP GRAND (MAXIMUM ~D, DONNE~:@(~P~) ~:*~D)")
                         procident
                         (if (< nargs minargs)
                             minargs
                             maxargs)
                         nargs)))
        ;; &procident
        (let ((procedure (gethash procident (vm-procedures vm))))
          (unless procedure
              (lse-error "IL N'Y A PAS DE PROCEDURE NOMMEE ~A" procident))
          (if (/= nargs (length (procedure-parameters procedure)))
              (lse-error "NOMBRE D'ARGUMENTS (~A) DIFFERENT DU NOMBRE DE PARAMETRES (~A) POUR LA PROCEDURE ~A"
                         nargs (length (procedure-parameters procedure))procident)
              (let ((frame (make-instance 'local-frame
                                          :procedure-name (procedure-name procedure)
                                          :call-type call-type
                                          :return.line (vm-pc.line vm)
                                          :return.offset (vm-pc.offset vm))))
                (loop
                  :for n :from nargs :by -1
                  :for (passage parameter) :in (procedure-parameters procedure)
                  :for argument = (stack-pop (vm-stack vm))
                  :do (ecase passage
                        (:par-valeur
                         (let ((argument (deref *vm* argument)))
                           (add-variable frame
                                         (make-instance 'lse-variable
                                                        :name parameter
                                                        :type (etypecase argument
                                                                ((or integer nombre) 'nombre)
                                                                (chaine              'chaine)
                                                                ((or vecteur tableau)
                                                                 (lse-error "ARGUMENT NO. ~D DE TYPE TABLEAU PASSE PAR VALEUR AU PARAMETRE ~A DE LA PROCEDURE ~A"
                                                                            n parameter procident)))
                                                        :value argument))))
                        (:par-reference
                         (let ((reference (typecase argument
                                            (identificateur
                                             (let ((var (find-variable vm argument)))
                                               (if var
                                                   var
                                                   (add-global-variable vm
                                                                        (make-instance 'lse-variable
                                                                                       :name argument
                                                                                       :type 'nombre)))))
                                            ((or lse-variable vecteur-ref tableau-ref)
                                             argument)
                                            (otherwise
                                             (lse-error
                                              "L'ARGUMENT NO. ~D PASSE PAR REFERENCE AU PARAMETRE ~A DE LA PROCEDURE ~A DOIT ETRE UNE REFERENCE A UNE VARIABLE OU UN TABLEAU."
                                              n parameter procident)))))
                           (add-variable frame
                                         (make-instance 'reference-parameter
                                                        :name parameter
                                                        :reference reference))))))
                (setf (slot-value frame 'stack-pointer) (fill-pointer (vm-stack vm)))
                (push frame (vm-local-frame-stack vm))
                (loop
                  :for local :in (procedure-local-variables procedure)
                  :do (add-variable frame (make-instance 'lse-variable :name local)))
                (let ((line (gethash (procedure-line procedure) (vm-code-vectors vm))))
                  (unless line
                    (lse-error "INTERNE: LE VECTEUR DE CODE POUR LA PROCEDURE ~A EST ABSENT." procident))
                  (setf (vm-state     vm) :running
                        (vm-pc.line   vm) (procedure-line procedure)
                        (vm-pc.offset vm) (procedure-offset procedure)
                        (vm-code      vm) (code-vector line))
                  (when (or (vm-pas-a-pas vm)
                            (eql (code-line line) (vm-trap-line vm)))
                    ;; to get the PAUSE message
                    (pause vm)))))))))

(defun callf (vm procident nargs) (call vm :function  procident nargs))
(defun callp (vm procident nargs) (call vm :procedure procident nargs))



(defun retour (vm)
  (let ((frame (first (vm-local-frame-stack vm))))
    (cond
      ((null frame)
       (lse-error "IL N'Y A PAS D'APPEL DE PROCEDURE EN COURS, RETOUR IMPOSSIBLE"))
      ((eql :function (frame-call-type frame))
       (lse-error "RETOUR DANS LA PROCEDURE ~A APPELEE COMME FONCTION."
                  (frame-procedure-name frame)))
      (t
       (pop (vm-local-frame-stack vm))
       (setf (vm-pc.line   vm) (frame-return.line   frame)
             (vm-pc.offset vm) (frame-return.offset frame)
             (vm-code      vm) (code-vector (gethash (vm-pc.line vm) (vm-code-vectors vm)))
             (fill-pointer (vm-stack vm)) (frame-stack-pointer frame))))))

(defun retour-en (vm lino)
  (let ((frame (first (vm-local-frame-stack vm))))
    (cond
      ((null frame)
       (lse-error "IL N'Y A PAS D'APPEL DE PROCEDURE EN COURS, RETOUR EN IMPOSSIBLE"))
      (t
       ;; When call-type is :function we have an exceptionnal return,
       ;; but it's the same processing.  The stack unwinding is done
       ;; in vm-goto, from the saved frame-stack-pointer.
       (let ((line (gethash lino (vm-code-vectors vm))))
         (pop (vm-local-frame-stack vm))
         (if line
             (setf (vm-pc.line   vm) lino
                   (vm-pc.offset vm) 0
                   (vm-code      vm) (code-vector line)
                   (fill-pointer (vm-stack vm)) (frame-stack-pointer frame))
             (error-bad-line lino)))))))

(defun result (vm result)
  (let ((frame (first (vm-local-frame-stack vm))))
    (cond
      ((null frame)
       (lse-error "IL N'Y A PAS D'APPEL DE PROCEDURE EN COURS, RESULTAT IMPOSSIBLE"))
      ((eql :function (frame-call-type frame))
       (pop (vm-local-frame-stack vm))
       (setf (vm-pc.line   vm) (frame-return.line   frame)
             (vm-pc.offset vm) (frame-return.offset frame)
             (vm-code      vm) (code-vector (gethash (vm-pc.line vm) (vm-code-vectors vm)))
             (fill-pointer (vm-stack vm)) (frame-stack-pointer frame))
       (stack-push result (vm-stack vm)))
      (t
       (lse-error "RESULTAT DANS LA PROCEDURE ~A APPELEE COMME SOUS-PROGRAMME."
                  (frame-procedure-name frame))))))





(defun validate-file-name (name)
  (cond
    ((not (stringp name))
     (lse-error "UN ~[NOMBRE~;TABLEAU~;OBJECT DE TYPE INDETERMINE~] NE PEUT ETRE UTILISE COMME NOM DE FICHIER."
                (typecase name
                  ((or integer nombre) 0)
                  (array               1)
                  (t                   2))))
    ((zerop (length name))
     (lse-error "UNE CHAINE VIDE NE PEUT ETRE UTILISE COMME NOM DE FICHIER."))
    (t
     (flet ((validate (name)
              (unless (every (function alphanumericp) name)
                (lse-error "UN NOM DE FICHIER NE PEUT CONTENIR QUE DES CARACTERES ALPHANUMERIQUES."))
              name))
       (if (char= #\# (aref name 0))
           (if (= 1 (length name))
               (lse-error "UNE CHAINE '#' NE PEUT ETRE UTILISE COMME NOM DE FICHIER PERMANENT.")
               (values (validate (subseq name 1)) :data))
           (values (validate name) :temporary))))))


(defun charger (vm enr fic ident status-ident)
  (let ((statvar (and status-ident (find-variable vm status-ident))))
    (when (and status-ident (not statvar))
      (setf statvar (add-global-variable vm (make-instance 'lse-variable
                                                :name status-ident
                                                :type 'nombre
                                                :value 0))))
    (multiple-value-bind (ficname fictype) (validate-file-name (deref vm fic))
      (let ((file (task-open-file *task* ficname fictype
                                  :if-does-not-exist (if status-ident
                                                         nil
                                                         :error)))
            (enr  (truncate (deref vm enr))))
        (if file
            (multiple-value-bind (data status) (read-record file enr)
              (if (minusp status)
                  (if status-ident
                      (setf (variable-value statvar) status)
                      (error 'lse-file-error
                             :backtrace (or #+ccl (ccl::backtrace-as-list))
                             :pathname ficname
                             :file file
                             :record-number enr
                             :format-control "ENREGISTREMENT ~A DANS '~A' INEXISTANT"
                             :format-arguments (list enr ficname)))
                  (let ((var (or (find-variable vm ident)
                                 (add-global-variable vm (make-instance 'lse-variable
                                                             :name ident)))))
                    (setf (variable-type var) (case status
                                                (0 'nombre)
                                                (1 `(vecteur ,(array-dimension data 0)))
                                                (2 `(tableau ,(array-dimension data 0)  ,(array-dimension data 1)))
                                                (3 'chaine))
                          (variable-value var) data
                          (variable-value statvar) status))))
            ;; No file:
            (if status-ident
                (setf (variable-value statvar) -2)
                (error 'lse-file-error
                       :backtrace (or #+ccl (ccl::backtrace-as-list))
                       :pathname (catalog-pathname ficname fictype)
                       :format-control "FICHIER '~A' INEXISTANT OU INACCESSIBLE"
                       :format-arguments (list ficname))))))))


(defun garer (vm enr fic ident)
  (multiple-value-bind (ficname fictype) (validate-file-name (deref vm fic))
    (let ((file (task-open-file *task* ficname fictype :if-does-not-exist :create))
          (enr  (truncate (deref vm enr)))
          (var  (find-variable vm ident)))
      (when (or (null var)
                (eql (variable-value var) :unbound))
        (lse-error "VARIABLE ~:@(A~) NON DEFINIE" ident))
      (write-record file enr
                    (let ((val (variable-value var)))
                      (typecase val
                        (integer (un-nombre val))
                        (t       val)))))))


(defun supprimer-enregistrement (vm fic enr)
  (multiple-value-bind (ficname fictype) (validate-file-name  (deref vm fic))
    (let ((file (task-open-file *task* ficname fictype :if-does-not-exist :error))
          (enr  (truncate (deref vm enr))))
      (delete-record file enr))))


(defun supprimer-fichier (vm fic)
  (let ((ficname (deref vm fic)))
    (multiple-value-bind (ficname fictype) (validate-file-name ficname)
      (task-delete-file *task* ficname fictype))))


(defun executer (vm fic lino)
  (let* ((ficname  (validate-file-name (deref vm fic)))
         (ficpath  (catalog-pathname ficname :program))
         (lino     (truncate (deref vm lino))))
    (vm-terminer vm)
    (replace-program vm (compile-lse-file ficpath ficname))
    (setf (vm-trap-line vm) nil)
    (vm-reset-variables vm)
    (vm-goto vm lino)))

                  

(defun comment (vm comment)
  (declare (ignore vm comment))
  (values))


(defvar *vm* nil "Current LSE-VM.")

(defvar *debug-vm* nil)
;; (setf *debug-vm* '(:cop))
;; (setf *debug-vm* '(:error))
;; (setf *debug-vm* nil)

(defun pret (task)
  (io-format task "    ~C~%PRET~%" #\Return)
  (io-finish-output task))


(defun run-step (vm)
  (catch 'run-step-done
    (handler-case
        (flet ((run ()
                 (let ((stack (vm-stack vm))
                       (code  (vm-code  vm))
                       (*vm*  vm))
                   (handler-case
                       (terminal-yield (task-terminal *task*))
                     (user-interrupt (condition)
                       (if (eql +sigquit+ (user-interrupt-signal condition))
                           (setf (task-signal *task*) t)
                           (signal condition))))
                   (flet ((spush  (val) (vector-push-extend val stack))
                          (spop   ()    (vector-pop stack))
                          (pfetch ()    (prog1 (aref code (vm-pc.offset vm))
                                          (incf (vm-pc.offset vm)))))
                     (declare (inline spush spop pfetch))
                     (macrolet ((op-1*  (op) `(spush (,op (deref vm (spop)))))
                                (op-2*  (op) `(spush (let ((b (spop))) (,op (deref vm (spop)) (deref vm b)))))
                                (op-0   (op) `(,op vm))
                                (op-1   (op) `(,op vm (spop)))
                                (op-2   (op) `(let ((b (spop))) (,op vm (spop) b)))
                                (op-3   (op) `(let ((c (spop)) (b (spop))) (,op vm (spop) b c)))
                                (op-0/1 (op) `(,op vm (pfetch)))
                                (op-1/1 (op) `(,op vm (spop) (pfetch)))
                                (op-2/1 (op) `(let ((b (spop))) (,op vm (spop) b (pfetch))))
                                (op-3/1 (op) `(let ((c (spop)) (b (spop))) (,op vm (spop) b c (pfetch))))
                                (op-4/1 (op) `(let ((d (spop)) (c (spop)) (b (spop))) (,op vm (spop) b c d (pfetch))))
                                (op-0/2 (op) `(,op vm (pfetch) (pfetch)))
                                (op-2/2 (op) `(let ((b (spop))) (,op vm (spop) b (pfetch) (pfetch)))))
                       #+debugging
                       (when (and (listp *debug-vm*) (member :cop *debug-vm*))
                         (let ((*standard-output* *trace-output*))
                           (disassemble-lse (vm-code vm)
                                            :start (vm-pc.offset vm)
                                            :one-instruction t :line (vm-pc.line vm))
                           (force-output)))
                       (let ((cop (pfetch)))
                         (case cop
                           
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
                           
                           (!beep             (op-0 beep))
                           (!LIRE&STORE       (op-0/1 LIRE&STORE))
                           (!LIRE&ASTORE1     (op-1/1 LIRE&ASTORE1))
                           (!LIRE&ASTORE2     (op-2/1 LIRE&ASTORE2))

                           (!next-line        (op-0 next-line))
                           (!retour           (op-0 retour))
                           (!retour-en        (op-1 retour-en))
                           (!result           (op-1 result))
                           (!goto             (op-1 goto))

                           (!garer                     (op-2/1 garer))
                           (!charger                   (op-2/2 charger))
                           (!supprimer-enregistrement  (op-2 supprimer-enregistrement))
                           (!supprimer-fichier         (op-1 supprimer-fichier))
                           (!executer                  (op-2 executer))
                           
                           (!pause          (op-0 pause))
                           (!terminer       (op-0 terminer))
                           (!stop           (op-0 stop))
                           
                           (!AREF1&PUSH-REF (op-1/1 AREF1&PUSH-REF))
                           (!AREF1&PUSH-VAL (op-1/1 AREF1&PUSH-VAL))
                           (!AREF2&PUSH-REF (op-2/1 AREF2&PUSH-REF))
                           (!AREF2&PUSH-VAL (op-2/1 AREF2&PUSH-VAL))

                           (!dup            (let ((a (spop))) (spush a) (spush a)))
                           (!pop            (spop))                           
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
                           (!faire-tant-que (op-3/1 faire-tant-que))
                           (!tant-que       (op-1 tant-que))

                           (!callf          (op-0/2 callf))
                           (!callp          (op-0/2 callp))
                           (!procedure      (lse-error "PROCEDURE N'EST PAS EXECUTABLE."))

                           (!comment        (op-0/1 comment))
                           (otherwise
                            (lse-error "INTERNE MACHINE VIRTUELLE: CODE OPERATION INCONNU ~S" cop)))))))))
          
          #-debugging (run)
          #+debugging (if (or (eq t *debug-vm*) (member :error *debug-vm*))
                          (handler-bind ((error #'invoke-debugger)) (run))
                          (run)))

      (error (err)
        (vm-pause vm) ; no message
        (error err))
      #+debugging
      (user-interrupt (condition)
        #-debugging (declare (ignore condition))
        #+debugging (progn (format *error-output* "~%Condition: ~A~%" condition)
                           (force-output *error-output*))
        (vm-pause vm) ; no message
        (io-standard-redirection *task*)
        (setf (task-silence *task*) nil)
        (pret *task*)))
    t))





#||

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


||#

;;;; THE END ;;;;
