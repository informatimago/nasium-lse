;;;;****************************************************************************
;;;;FILE:               compiler.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    The LSE line compiler.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-25 <PJB> Created.
;;;;BUGS
;;;;    LIRE must take into account the type of the variable.
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

;;----------------------------------------------------------------------
(cl:in-package "COM.INFORMATIMAGO.LSE.BYTE-CODE")
;;----------------------------------------------------------------------

;; We define a stack machine byte code.  Each opcode may take 0, 1 or
;; 2 parameter form the stack and push back results on the stack.
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


                         AFFICHER-E        ; arg1 arg2 arg3 AFFICHER-E
                         AFFICHER-F        ; arg1 arg2 arg3 AFFICHER-F
                         AFFICHER-CR       ; arg AFFICHER-CR
                         AFFICHER-NEWLINE  ; arg AFFICHER-NEWLINE
                         AFFICHER-NL       ; arg AFFICHER-NL
                         AFFICHER-SPACE    ; arg AFFICHER-SPACE
                         AFFICHER-U        ; arg AFFICHER-U
                         LIRE&PUSH         ; LIRE&PUSH

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
;;----------------------------------------------------------------------


(defun cons-position (cons list &key (start 0) (end nil))
  "
RETURN: The index in list of the cons cell CONS.
POST:   (and (cons-position c l) (eq c (nthcdr (cons-position c l) l)))
"
  (when (minusp start) (setf start 0))
  (when (and end (minusp end)) (setf end 0))
  (let ((current (nthcdr start list)))
    (loop (cond ((eq current cons)
                 (return-from cons-position start))
                ((or (and end (<= end start))
                     (null (cdr current)))
                   (return-from cons-position nil))
                (t
                 (incf start)
                 (setf current (cdr current)))))))



(defun gen (&rest items)
  (cond
    ((null (cdr items))
     (car items))
    ((or (numberp (car items))
         (stringp (car items))
         (and (symbolp (car items))
              (or (eq (symbol-package (car items)) (find-package "BC"))
                  (eq (symbol-package (car items)) (find-package "ID")))))
     (cons (car items) (apply (function gen) (cdr items))))
    (t
      (generate-statement (car items) (apply (function gen) (cdr items))))))



(defun generate-statement (stat next)
  (etypecase stat
    ((or integer nombre symbol)
     (cons stat next))
    (token
     (ecase (token-kind stat)
       (tok-litchaine  (gen bc::pushi (chaine-valeur stat) next))
       (tok-nombre     (gen bc::pushi (nombre-valeur stat) next))
       (tok-numero     (gen bc::pushi (numero-valeur stat) next))))
    (cons
     (ecase (first stat)

       ((:neg :non)
        (gen (second stat) (case (first stat)
                             (:neg bc::neg)
                             (:non bc::non)) next))
       ((:moins :plus :fois :divise :puissance :concat
                :lt :le :gt :ge :eg :ne)
        (gen (second stat) (third stat)
             (case (first stat)
               (:moins bc::sub)
               (:plus  bc::add)
               (:fois  bc::mul)
               (:divise bc::div)
               (:puissance bc::pow)
               (:concat bc::concat)
               (:lt bc::lt)
               (:le bc::le)
               (:gt bc::gt)
               (:ge bc::ge)
               (:eg bc::eg)
               (:ne bc::ne)) next))
       (:xi
        (let* ((else (gen (fourth stat) next))
               (offset.e (cons-position next else))
               (then (gen (third stat) bc::balways  offset.e else))
               (offset.t (cons-position else then)))
          (gen (second stat) bc::bfalse offset.t then)))
       ((:ou :et)
        (loop
           :with suite = next
           :for item :in (reverse (cddr stat))
           :do (setf suite (gen item (case (first stat)
                                      (:ou bc::ou)
                                      (:et bc::et)) suite))
           :finally (return (gen (second stat) (case (first stat)
                                                (:ou bc::ou)
                                                (:et bc::et)) suite))))

       (:commentaire next)
       ((:liberer :chaine)
        (loop
           :with suite = next
           :for item :in (reverse (rest stat))
           :do (setf suite (gen (case (first stat)
                                  (:liberer bc::liberer)
                                  (:chaine  bc::chaine))
                                (identificateur-nom item) suite))
           :finally (return suite)))
       (:tableau
        (loop
           :for item :in (reverse (rest stat))
           :do (setf next (generate-statement item next))
           :finally (return next)))
       (:adecl
        (if (null (cdddr stat))
            (gen (third stat) bc::tableau1
                 (identificateur-nom (second stat)) next)
            (gen (third stat) (fourth stat) bc::tableau2
                 (identificateur-nom (second stat)) next)))
       
       ((:aval :aref)
        (if (null (cdddr stat))
            (gen (third stat)
                 (case (first stat)
                   (:aval bc::aref1&push-val)
                   (:aref bc::aref1&push-ref))
                 (identificateur-nom (second stat)) next)
            (gen (third stat) (fourth stat)
                 (case (first stat)
                   (:aval bc::aref2&push-val)
                   (:aref bc::aref2&push-ref))
                 (identificateur-nom (second stat)) next)))
       ((:vval :vref)
        (gen (case (first stat)
               (:vval bc::push-val)
               (:vref bc::push-ref)) (identificateur-nom (second stat)) next))

       ((:fonction :appel)
        (loop
           :with suite = (gen bc::call (identificateur-nom (second stat))
                              (length (cddr stat)) next)
           :for item :in (reverse (cddr stat))
           :do (setf suite (gen item suite))
           :finally (return suite)))
       #||
       (:procedure ident nil       nil)
       (:procedure ident (fpid...) nil)
       (:procedure ident nil       locid...)
       (:procedure ident (fpid...) locid...)
       --> :trap-proc-reached *
       ==> fpid inter locid == arguments par valeur    ==> copier
       ==> fpid diff  locid == arguments par reference
       ==> locid diff fpid  == variable locales --> table variable locale pour la proc.
       ||#

       (:resultat   (gen (second stat) bc::result next))
       (:retour-en  (gen (second stat) bc::retour-en next))
       (:retour     (gen bc::retour next))

       ;; :result             OR  :retour-en      OR   :retour
       ;; -------------------     ---------------     ------------
       ;; result    <-- :pop  OR  goto <-- :pop   OR   nothing
       ;; 
       ;; return-pc <-- sf.return-pc
       ;; next-sf   <-- sf.next-sf
       ;; argcnt    <-- sf.argcnt
       ;; sf        <-- next-sf
       ;; :pop-sf
       ;; :pop-n argcnt
       ;;                
       ;; :push  result              --                 --
       ;; pc <-- return-pc       OR  pc <-- goto     or pc <-- return-pc

       (:affectation
        ;; (:vref ident) expression) --> expression :pop&store ident
        ;; (:aref ident expr) expression) --> expression expr :pop&astore1 ident
        ;; (:aref ident expr.1 expr.2) expression) --> expression expr.1 expr.2 :pop&astore2 ident
        (setf next (cons (identificateur-nom (second (second stat))) next))
        (case (length (second stat))
          (2  (gen (third stat)
                   bc::pop&store    next))
          (3  (gen (third stat) (third (second stat))
                   bc::pop&astore1  next))
          (4  (gen (third stat) (third (second stat))
                   (fourth (second stat))
                   bc::pop&astore2  next))))

       (:lire
        ;; (:vref ident) --> :lire&push :pop&store ident
        ;; (:aref ident expr) --> :lire&push expr :pop&astore1 ident
        ;; (:aref ident expr.1 expr.2) --> :lire&push expr.1 expr.2 :pop&astore2 ident
        (loop
           :for item :in (reverse (rest stat))
           :do (setf next 
                    (case (length item)
                      (2 (gen bc::lire&push bc::pop&store
                              (identificateur-nom (second item)) next))
                      (3  (gen bc::lire&push
                               (third item)  bc::pop&astore1
                               (identificateur-nom (second item)) next))
                      (4  (gen bc::lire&push
                               (third item) (fourth item)  bc::pop&astore2
                               (identificateur-nom (second item)) next))))
           :finally (return next)))

       (:rep-1     (gen bc::pushi 1 next))
       (:rep       (gen bc::pushi (numero-valeur (second stat)) next))
       (:rep-var   (gen bc::push-val 'id::$index bc::dup bc::pushi 1 bc::add
                        bc::pop&store 'id::$index bc::aref2&push-val 'id::$vals
                        next))
       (:afficher
        ;; (:afficher nil expr...)      --> :pushi n expr... :afficher-u
        ;; (:afficher (form...) expr...)
        ;; (:afficher (form...))
        (if (second stat)
            (loop
               :for format :in (reverse (second stat))
               :do (setf next
                        (case (first format)
                          (:spec-chaine
                           (gen (second format)
                                bc::pushi (third format)
                                bc::afficher-u next))
                          ((:spec-slash :spec-space :spec-cr :spec-nl :spec-u)
                           (gen (second format)
                                (case (first format)
                                  (:spec-slash bc::afficher-newline)
                                  (:spec-space bc::afficher-space)
                                  (:spec-cr    bc::afficher-cr)
                                  (:spec-nl    bc::afficher-nl)
                                  (:spec-u     bc::afficher-u))
                                next))
                          ((:spec-f :spec-e)
                           (gen (second format)
                                (third format)
                                (fourth format)
                                (case (first format)
                                  (:spec-f bc::afficher-f)
                                  (:spec-e bc::afficher-e))
                                next)))))
            ;; (5) AFFICHER[{n}U]expr...
            (setf next (gen bc::pushi (length (cddr stat)) bc::afficher-u next)))
        ;; (4) set $index to 1
        (setf next (gen bc::pushi 1 bc::pop&store 'id::$index next))
        ;; (3) pop the N expression and store them in $vals[$index]
        (loop
           :for i :from 1 :to (length (cddr stat)) 
           :do (setf next (gen  bc::pushi i bc::pop&astore1 'id::$vals next)))
        ;; (2) push the N expressions (first pushed first)
        (loop
           :for expr :in (reverse (cddr stat))
           :do (setf next (generate-statement expr next)))
        ;; (1) declare the tableau $vals[N]
        (gen  bc::pushi (length (cddr stat)) BC::TABLEAU1 'ID::$VALS next))

       ;; rep :affiche-u -- 
       ;; 
       ;; @loop: dup pushi 0 eg btrue @end
       ;; dup pushi 1 add swap pushi 1 swap
       ;; aref1&push $vals :afficher-u branch @loop
       ;; @end: pop



       (:aller-en (gen (second stat) bc::goto next))
       (:si
        ;;(:si test then)       --> test :bfalse offset.t then  
        ;;(:si test then else)  --> test :bfalse offset.t then  :balways offset.e else
        (if (cdddr stat)
            ;; same as :xi
            (let* ((else (gen (fourth stat) next))
                   (offset.e (cons-position next else))
                   (then (gen (third stat) bc::balways  offset.e else))
                   (offset.t (cons-position else then)))
              (gen (second stat) bc::bfalse offset.t then))
            (let* ((then (gen (third stat) next))
                   (offset.t (cons-position next then)))
              (gen (second stat) bc::bfalse offset.t then))))

       (:terminer (gen bc::terminer next))
       (:pause    (gen bc::pause    next))

       (:faire-jusqu-a ;; lino ident init pas jusqua)
        ;; --> lino init pas jusqua :faire-jusqu-a ident
        (gen (second stat) (fourth stat) (fifth stat) (sixth stat)
             bc::faire-jusqu-a (third stat) next))

       (:faire-tant-que ;; lino ident init pas test)
        ;; --> lino init pas :faire-tant-que ident test
        (gen (second stat) (fourth stat) (fifth stat) 
             bc::faire-tant-que (third stat)
             (sixth stat) bc::tant-que next))

       ;; ==> create a faire bloc. When we reach the end of each line, we must
       ;;     check for loop blocks available for this line. (kind of come from...).


       (:garer ;; var enr fic) --> enr fic :garer var
        (gen (third stat) (fourth stat) bc::garer
             (identificateur-nom (second stat))))

       (:charger
        ;; TOOD: this creates the variables var and varstat too (TABLEAU, CHAINE or real)
        ;;(:charger  var enr fic)         --> enr fic :charger :pop&store var :pop
        ;;(:charger  var enr fic varstat) --> enr fic :charger :pop&store var :pop&store varstat
        (if (cddddr stat)
            (gen (third stat) (fourth stat) bc::charger bc::pop&store
                 (second stat) bc::pop&store (fifth stat) next)
            (gen (third stat) (fourth stat) bc::charger bc::pop&store
                 (second stat) next)))
       
       (:supprimer
        (if (cddr stat)
            (gen (second stat) (third stat)  bc::supprimer-enregistrement next)
            (gen (second stat) bc::supprimer-fichier next)))
       
       (:executer
        (if (cddr stat)
            (gen (second stat) (third stat)  bc::executer next)
            (gen (second stat) bc::pushi 1   bc::executer next)))

       (:debut
        (loop
           :for i :in (reverse (cdr stat))
           :do (setf next (generate-statement i next))
           :finally (return next)))))))


(defun generate-slist (slist)    ; generate bytes for a statement list
  (if slist
      (generate-statement (car slist)
                          (generate-slist (cdr slist)))
      (list BC::NEXT-LINE)))
                          



;; lire --> vref
;; si --> vval/aval
;; adecl --> vref/aref

(defun compile-slist (slist)            ; compile statement list
  (let ((bytes (generate-slist slist)))
    (make-array (length bytes) :initial-contents bytes)))


(defun compile-line (source-line)
  (let* ((scanner (task-scanner *task*))
         (next-token (get-next-token-function scanner)))
    (with-input-from-string (source source-line)
      (setf (source scanner) source)
      (advance-line scanner)
      (let ((parse-tree (lr-parse next-token
                                  (lambda (msg) (error "ERREUR: ~A" msg))
                                  (find-grammar "LSE"))))
        (terpri) (print parse-tree)
        (cond
          ((null parse-tree))           ; nothing to do
          ((and (typep parse-tree 'token)
                (eq (token-kind parse-tree) 'tok-erreur))
           (format *error-output*
             "~&ERREUR: ~S~%" (token-text parse-tree)))
          ((atom parse-tree)
           (format *error-output*
             "~&ERREUR: JE TROUVE UN ATOME: ~S~%" parse-tree))
          ((eq (car parse-tree) :liste-instructions)
           (compile-slist (cdr parse-tree)))
          ((eq (car parse-tree) :ligne-programme)
           (compile-slist (cddr parse-tree)))
          (t
           (format *error-output*
             "~&ERREUR: JE TROUVE UNE LISTE INVALIDE: ~S~%" parse-tree)))))))
            

(defparameter *cop-info*
  (let ((table (make-hash-table)))
    (dolist (c bc::*0*) (setf (gethash (symbol-value c) table) (cons c 0)))
    (dolist (c bc::*1*) (setf (gethash (symbol-value c) table) (cons c 1)))
    (dolist (c bc::*2*) (setf (gethash (symbol-value c) table) (cons c 2)))
    table)
  "Maps the code operation to a (cons symbol number-of-parameters).")



(defun disassemble-lse (byte-code)
  (flet ((nbop (cop) (cdr (gethash cop *cop-info* '(0 . 0))))
         (disa (cop) (car (gethash cop *cop-info* (cons cop 0)))))
    (let ((pc 0)
          (listing '())
          (labels '()))
      (loop :while (< pc (length byte-code)) :do
         (let ((cop (list (disa (aref byte-code pc))))
               (noa       (nbop (aref byte-code pc))))
           (incf pc)
           (dotimes (n noa)
             (setf cop (nconc cop (list (aref byte-code pc))))
             (incf pc))
           (push cop listing)
           (when (member (first cop) bc::*branches*)
             (push (+ pc (second cop)) labels))))
      (setf listing (nreverse listing))
      (loop
         :initially (format t "~&")
         :with pc = 0
         :for line :in listing
         :do (progn (if (member pc labels)
                        (format t "@~6:A " pc)
                        (format t "        "))
                    (format t "~32:A"
                            (with-output-to-string (out)
                              (princ "(" out)
                              (let ((first t))
                                (dolist (item line)
                                  (if first
                                      (setf first nil)
                                      (princ " " out))
                                  (if (symbolp item)
                                      (princ item out)
                                      (prin1 item out))))
                              (princ ")" out)))
                    (if (member (first line) bc::*branches*)
                        (format t " ; @~A~%"  (+ pc (length line) (second line)))
                        (format t "~%"))
                    (incf pc (length line)))))))

;;(progn (terpri)(disassemble-lse bc))

;;(:Ligne-Programme numero instr...)
;;(:liste-instructions instr...)

;; (:Ligne-Programme numero instr...)
;; ==> compile instr... and store vector in lino.
;; 
;; (:ligne-programme numero (:decl-procedure  decl-procedure))
;; ==> compile decl-procedure; store vector in lino; store proc.ident in proctable.


(defun test-parse-file (path)
  (with-open-file (src path)
    (terpri)
    (let* ((scanner    (make-instance 'scanner :source src))
           (next-token (get-next-token-function scanner)))
      (loop :do
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


(defun test-compile-file (path)
  (with-open-file (src path)
    (loop
       :for line = (read-line src nil nil)
       :for comp = (and line (compile-line line))
       :while line
       :do (print comp)
       :do (print (disassemble-lse comp))
       :finally (terpri) (return (values)))))


;; (test-parse-file #P "~/src/pjb/lse-cl/SYNTERR.LSE")
;; (test-parse-file #P "~/src/pjb/lse/BOURG/BOUR.LSE")

;; (test-compile-file #P "~/src/pjb/lse/BOURG/BOUR.LSE")
;; (test-compile-file #P "~/src/pjb/lse-cl/TESTCOMP.LSE")



;;;; THE END ;;;;
