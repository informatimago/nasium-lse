;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               byte-code.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the byte codes of the L.S.E Virtual Machine.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-15 <PJB> Extracted from compiler.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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
;;;;**************************************************************************


(in-package "COM.INFORMATIMAGO.LSE.BYTE-CODE")
;;; The BYTE-CODE package doesn't use :COMMON-LISP.


;;; So we must qualify CL symbols.

;; We define a stack machine byte code.  Each opcode may take 0, 1 or
;; 2 parameters form the stack and push back results on the stack.
;; opcode may also have zero or more literal parameters following the
;; opcode in the byte-code vector.


(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  
  (cl:defparameter *branches* '(
                                BALWAYS ;       BALWAYS offset
                                BTRUE   ; test  BTRUE   offset
                                BFALSE  ; test  BFALSE  offset
                                BNEVER  ;       BNEVER  offset
                                ))
  
  (cl:defparameter *0* '(
                         DUP ; arg DUP

                         NON ; arg NON 
                         OU  ; arg1 arg2 OU
                         ET  ; arg1 arg2 ET

                         EG  ; arg1 arg2 EG
                         NE  ; arg1 arg2 NE
                         LE  ; arg1 arg2 LE
                         LT  ; arg1 arg2 LT
                         GE  ; arg1 arg2 GE
                         GT  ; arg1 arg2 GT

                         CONCAT ; arg1 arg2 CONCAT

                         NEG ; arg NEG
                         ADD ; arg1 arg2 ADD
                         SUB ; arg1 arg2 SUB
                         MUL ; arg1 arg2 MUL
                         DIV ; arg1 arg2 DIV
                         POW ; arg1 arg2 POW


                         AFFICHER-E        ; rep e f AFFICHER-E
                         AFFICHER-F        ; rep e f AFFICHER-F
                         AFFICHER-CR       ; rep AFFICHER-CR
                         AFFICHER-CHAINE   ; rep chaine AFFICHER-CHAINE
                         AFFICHER-NEWLINE  ; rep AFFICHER-NEWLINE
                         AFFICHER-NL       ; rep AFFICHER-NL
                         AFFICHER-SPACE    ; rep AFFICHER-SPACE
                         AFFICHER-U        ; nexpr AFFICHER-U

                         NEXT-LINE  ; NEXT-LINE
                         RETOUR     ; RETOUR
                         RETOUR-EN  ; line RETOUR-EN
                         RESULT     ; arg RESULT
                         GOTO       ; line GOTO

                         TANT-QUE                 ; test TANT-QUE
                         CHARGER                  ; enr fic CHARGER
                         SUPPRIMER-ENREGISTREMENT ; enr fic SUPPRIMER-ENREGISTREMENT
                         SUPPRIMER-FICHIER        ;     fic SUPPRIMER-FICHIER
                         EXECUTER  ; fic lin EXECUTED
                         PAUSE     ; PAUSE
                         TERMINER  ; TERMINER
                         STOP ; STOP, to end interactive expressions.
                         beep ; BEEP, to beep before LIRE.
                         PROCEDURE
                         ))

  (cl:defparameter *1* '(
                         AREF1&PUSH-REF ; idx AREF1&PUSH-REF identifier
                         AREF1&PUSH-VAL ; idx AREF1&PUSH-VAL identifier
                         AREF2&PUSH-REF ; idx1 idx2 AREF2&PUSH-REF identifier
                         AREF2&PUSH-VAL ; idx1 idx2 AREF2&PUSH-VAL identifier

                         POP&ASTORE1    ; val idx POP&ASTORE1 identifier
                         POP&ASTORE2    ; val idx1 idx2 POP&ASTORE2 identifier
                         POP&STORE      ; val POP&STORE identifier

                         PUSH-REF       ; PUSH-REF identifier
                         PUSH-VAL       ; PUSH-REF identifier
                         PUSHI          ; PUSHI immediate-value

                         LIRE&STORE        ; LIRE&STORE ident
                         LIRE&ASTORE1      ; index LIRE&ASTORE1 ident
                         LIRE&ASTORE2      ; idx1 idx2 LIRE&ASTORE1 ident

                         CHAINE         ; CHAINE identifier
                         TABLEAU1       ; dim TABLEAU1 identifier
                         TABLEAU2       ; dim1 dim2 TABLEAU2 identifier
                         LIBERER        ; LIBERER identifier

                         BALWAYS    ;      BALWAYS offset
                         BTRUE      ; test BTRUE   offset
                         BFALSE     ; test BFALSE  offset
                         BNEVER     ;      BNEVER  offset

                         FAIRE-JUSQU-A   ; lino init pas jusqua FAIRE-JUSQU-A ident
                         FAIRE-TANT-QUE  ; lino init pas FAIRE-TANT-QUE ident
                                        ; test TANT-QUE
                         GARER           ; enr fic GARER identificateur
                         comment ; COMMENT comment
                         ))

  (cl:defparameter *2* '(CALL            ; CALL identificateur-procedure nombre-d-argument
                         ))

  (cl:defparameter *cops* (cl:append *0* *1* *2*)))


(cl:defmacro defcops (cl:&key numeric)
  `(cl:progn ,@(cl:loop
                :for i :from 0
                :for s :in *cops*
                :collect `(cl:defparameter ,s ,(cl:if numeric i `,s)))))

(cl:eval-when (:compile-toplevel :load-toplevel :execute)
  (defcops :numeric t))



;;----------------------------------------------------------------------
(cl:in-package "COM.INFORMATIMAGO.LSE")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun byte-code-reader (stream ch)
    (declare (ignore ch))
    (let ((sym (read stream))
          (bc-package (find-package "COM.INFORMATIMAGO.LSE.BYTE-CODE")))
      (multiple-value-bind (bc status) (find-symbol (symbol-name sym) bc-package)
        (if status
            (symbol-value bc)
            (error "There is no bytecode named ~A" sym))))))

(defmacro enable-byte-code-reader-macro ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (set-macro-character #\! 'byte-code-reader t)))


;;;; THE END ;;;;
