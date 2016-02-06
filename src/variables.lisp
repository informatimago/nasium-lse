;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               variables.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines LSE variables.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-23 <PJB> Extracted from vm.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
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


;; Therefore we define frames to hold variables.  There's a global frame,
;; for global variables, and a stack of local frames for procedure local
;; variables.  The local frames also have the return line and offset for
;; the procedure.



;; FRAMES:

(defclass frame ()
  ((variables      :initarg :variables :initform '() :accessor frame-variables)
   (loop-stack     :initform '() :accessor frame-loop-stack)
   (stack-pointer  :initarg :stack :initform 0 :reader frame-stack-pointer
                   :documentation "The stack pointer upon entry in the procedure.
It's needed to unwind the stack when the procedure exits exceptionnaly.")))

(defun loop-stack (vm)
  (frame-loop-stack (or (first (vm-local-frame-stack vm))
                        (vm-global-frame vm))))

(defun (setf loop-stack) (new-value vm)
  (setf (frame-loop-stack (or (first (vm-local-frame-stack vm))
                              (vm-global-frame vm)))
        new-value))



(defclass local-frame (frame)
  ((procedure-name :initarg :procedure-name :reader frame-procedure-name)
   (call-type      :initarg :call-type :initform :procedure :reader frame-call-type
                   :type (member :procedure :function))
   (return.line    :initarg :return.line :initform 0 :reader frame-return.line
                   :documentation "The return line.")
   (return.offset  :initarg :return.offset :initform 0 :reader frame-return.offset
                   :documentation "The offset into the code vector of the return line.")))


(defmethod print-object ((frame frame) stream)
  (print-unreadable-object (frame stream :identity t :type t)
    (format stream "with ~D variables: ~(~A~^ ~)"
            (length (frame-variables frame))
            (sort (mapcar (function car) (frame-variables frame))
                  (function string<))))
  frame)

(defmethod find-variable ((frame frame) (ident symbol))
  (values (cdr (assoc ident (frame-variables frame))) frame))

(defmethod add-variable ((frame frame) var)
  (setf (frame-variables frame) (acons (variable-name var) var (frame-variables frame)))
  var)

(defmethod remove-variable ((frame frame) var)
  (setf (frame-variables frame) (remove (variable-name var) (frame-variables frame)
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
  (if *print-escape*
      (print-unreadable-object (slot stream :identity t :type t)
        (format stream "~A" (variable-name slot)))
      (format stream "~:@(~A~)" (variable-name slot)))
  slot)


;; LSE-VARIABLES:

(defclass lse-variable (named-slot)
  ((type  :initarg :type  :accessor variable-type  :initform :undefined)
   (value :initarg :value :accessor variable-value :initform :unbound)))

(defmethod print-object ((variable lse-variable) stream)
  (if *print-escape*
      (print-unreadable-object (variable stream :identity t :type t)
        (format stream "~A = ~S of type ~S"
                (variable-name variable)
                (variable-value variable)
                (variable-type variable)))
      (format stream "~:@(~A~)" (variable-name variable)))
  variable)

;; REFERENCE-PARAMETERS:

(defclass reference-parameter (named-slot)
  ((reference :initarg :reference :accessor referenced-variable)))

(defmethod variable-type ((par reference-parameter))
  (variable-type (referenced-variable par)))

(defmethod (setf variable-type) (new-type (par reference-parameter))
  (setf (variable-type (referenced-variable par)) new-type))

(defmethod variable-value ((par reference-parameter))
  (variable-value (referenced-variable par)))

(defmethod (setf variable-value) (new-value (par reference-parameter))
  (setf (variable-value (referenced-variable par)) new-value))


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
  (aref (reference-vecteur self)
        (1- (reference-index self))))

(defmethod (setf variable-value) (new-value (self vecteur-ref))
  (setf (aref (reference-vecteur self)
              (1- (reference-index self))) new-value))



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
  (aref (reference-tableau self)
        (1- (reference-index1 self))
        (1- (reference-index2 self))))

(defmethod (setf variable-value) (new-value (self tableau-ref))
  (setf (aref (reference-tableau self)
              (1- (reference-index1 self))
              (1- (reference-index2 self))) new-value))




;;;; THE END ;;;;
