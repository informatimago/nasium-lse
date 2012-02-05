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

;; 10 ALLER EN 20
;; 20 ALLER EN 10+I
;; 
;; #HASH( (10 => #((PUSHI 20)(POP&GO)))
;;        (20 => #((PUSHI 10)(PUSHV I)(ADD)(POP&GO)) )
       




(defstruct vm
  (cv        (make-hash-table :test (function eql) :size 256))
  (gv        (make-hash-table :test (function eql) :size 256))
  (stack     (make-array '(256)
                     :element-type t
                     :adjustable t
                     :fill-pointer 0))
  (pc.line   0)
  (pc.offset 0)
  (fp        0)
  (sp        0)
  ;; cache:
  (code      #() :type vector))


(defun rem-line (vm lino)      (remhash lino (vm-cv vm)))
(defun put-line (vm lino code) (setf (gethash lino (vm-cv vm)) code))


(defun run-step (vm)
  (catch 'done
    (handler-case
        (let ((stack (vm-stack vm))
              (code  (vm-code  vm)))
          (flet ((spush  (val) (vector-push-extend val stack))
                 (spop   ()    (vector-pop stack))
                 (pfetch ()    (prog1 (aref code (vm-pc.offset vm))
                                 (incf (vm-pc.offset vm)))))
            (declare (inline spush spop pfetch))
            (macrolet ((op-0   (op) `(,op))
                       (op-1   (op) `(spush (,op (spop))))
                       (op-2   (op) `(spush (let ((b (spop))) (,op (spop) b))))
                       (op-3   (op) `(spush (let ((c (spop)) (b (spop))) (,op (spop) b c))))
                       (op-0/1 (op) `(,op (pfetch)))
                       (op-1/1 (op) `(spush (,op (spop) (pfetch))))
                       (op-2/1 (op) `(spush (let ((b (spop))) (,op (spop) b (pfetch)))))
                       (op-3/1 (op) `(spush (let ((c (spop)) (b (spop))) (,op (spop) b c (pfetch)))))
                       (op-4/1 (op) `(spush (let ((d (spop)) (c (spop)) (b (spop))) (,op (spop) b c d (pfetch)))))
                       (op-0/2 (op) `(,op (pfetch) (pfetch))))
              (let ((cop (pfetch)))

                (case cop

                  (:dup    (let ((a (spop))) (spush a) (spush a)))
                  
                  (:non    (op-1 non))
                  (:et     (op-2 et))
                  (:ou     (op-2 ou))

                  (:eg     (op-2 eg))
                  (:ne     (op-2 ne))
                  (:le     (op-2 le))
                  (:lt     (op-2 lt))
                  (:ge     (op-2 ge))
                  (:gt     (op-2 gt))

                  (:concat (op-2 concatenation))

                  (:neg    (op-1 neg))
                  (:add    (op-2 add))
                  (:sub    (op-2 sub))
                  (:mul    (op-2 mul))
                  (:div    (op-2 div))
                  (:pow    (op-2 pow))

                  (:afficher-e       (op-3 afficher-e))
                  (:afficher-f       (op-3 afficher-f))
                  (:afficher-cr      (op-1 afficher-cr))
                  (:afficher-newline (op-1 afficher-newline))
                  (:afficher-nl      (op-1 afficher-nl))
                  (:afficher-space   (op-1 afficher-space))
                  (:afficher-u       (op-1 afficher-u))
                  

                  (:LIRE&PUSH        (op-0* LIRE&PUSH))

                  (:next-line        (op-0* next-line))
                  (:retour           (op-0* retour))
                  (:retour-en        (op-1* retour-en))
                  (:result           (op-1* result))
                  (:goto             (op-1* goto))


                  (:tant-que                  (op-1* tant-que))
                  (:charger                   (op-2 charger))
                  (:supprimer-enregistrement  (op-2 supprimer-enregistrement))
                  (:supprimer-fichier         (op-1 supprimer-fichier))
                  (:executer                  (op-2* executer))
                  (:pause          (op-0* pause))
                  (:terminer       (op-0* terminer))
                  
                  (:AREF1&PUSH-REF (op-1/1* AREF1&PUSH-REF))
                  (:AREF1&PUSH-VAL (op-1/1* AREF1&PUSH-VAL))
                  (:AREF2&PUSH-REF (op-2/1* AREF2&PUSH-REF))
                  (:AREF2&PUSH-VAL (op-2/1* AREF2&PUSH-VAL))

                  (:POP&ASTORE1    (op-2/1* POP&ASTORE1))
                  (:POP&ASTORE2    (op-3/1* POP&ASTORE2))
                  (:POP&STORE      (op-1/1* POP&STORE))

                  (:push-ref       (op-0/1* push-ref))
                  (:push-val       (op-0/1* push-val))
                  (:pushi          (op-0/1* pushi))

                  (:chaine         (op-0/1* chaine))
                  (:tableau1       (op-1/1* tableau1))
                  (:tableau2       (op-2/1* tableau2))
                  (:liberer        (op-0/1* liberer))

                  
                  (:balways        (op-0/1* balways))
                  (:btrue          (op-1/1* btrue))
                  (:bfalse         (op-1/1* bfalse))
                  (:bnever         (op-0/1* bnever))

                  (:faire-jusqu-a  (op-4/1* faire-jusqu-a))

                  (:call           (op-0/2* call))
                  (:faire-tant-que (op-3/1* faire-tant-que))
                  (:garer          (op-2/1 garer))


                  ;; (:liberer   (liberer      (prog1 (aref code pc.offset) (incf pc.offset))))
                  ;; (:chaine    (decl-chaine  (prog1 (aref code pc.offset) (incf pc.offset))))
                  ;; (:tableau1  (decl-tableau (prog1 (aref code pc.offset) (incf pc.offset)) 
                  ;;                           (spop)))
                  ;; (:tableau2  (let ((b (spop)))
                  ;;               (decl-tableau (prog1 (aref code pc.offset) (incf pc.offset))
                  ;;                             (spop) b)))
                  ;; (:pause    -->complete-pause)
                  ;; (:terminer -->complete-terminer)
                  
                  (otherwise (error "COP INCONNU: ~S" cop)))))))

      
      (error (err)
        (report err)
        (pause)))))


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
  --> {:lire ident}...
  --> :lire&push expr :pop&astore1 ident
  --> :lire&push expr.1 expr.2 :pop&astore2 ident


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
