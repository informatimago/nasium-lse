;;;; -*- mode:lisp; coding:utf-8 -*-
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
       (tok-chaine         (gen bc::pushi (chaine-valeur stat) next))
       (tok-nombre         (gen bc::pushi (nombre-valeur stat) next))
       (tok-numero         (gen bc::pushi (numero-valeur stat) next))
       (tok-identificateur (gen (identificateur-nom stat) next))
       (tok-commentaire    (gen bc::comment (token-text stat) next))))
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

       (:commentaire
        (gen (second stat) next))
       
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
               (:vref bc::push-ref))
             (identificateur-nom (second stat)) next))

       ((:fonction :appel)
        (loop
           :with suite = (gen bc::call (identificateur-nom (second stat))
                              (length (cddr stat)) next)
           :for item :in (reverse (cddr stat))
           :do (setf suite (gen item suite))
           :finally (return suite)))

       
       ;; (:decl-procedure ident nil       nil)
       ;; (:decl-procedure ident (fpid...) nil)
       ;; (:decl-procedure ident nil       (locid...))
       ;; (:decl-procedure ident (fpid...) (locid...))
       ;; --> :trap-proc-reached *
       ;; ==> fpid inter locid == arguments par valeur  ==> copier
       ;; ==> fpid diff  locid == arguments par reference
       ;; ==> locid diff fpid  == variable locales --> table variable locale pour la proc.

       ;; Parameters by reference
       ;; Parameters by value       = Local variables
       ;; Global Variables
       ;; Local Variables

       
       (:decl-procedure
        ;; Les procedures ne sont pas executable, bc::procedure generates an error.
        
        (gen bc::procedure next))
       
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
        ;; (:vref ident) --> :lire&store ident
        ;; (:aref ident expr) -->  expr :lire&astore1 ident
        ;; (:aref ident expr.1 expr.2) -->  expr.1 expr.2 :lire&astore2 ident
        (loop
           :for item :in (reverse (rest stat))
           :do (setf next 
                     (case (length item)
                       (2  (gen bc::lire&store
                                (identificateur-nom (second item)) next))
                       (3  (gen (third item) bc::lire&astore1
                                (identificateur-nom (second item)) next))
                       (4  (gen (third item) (fourth item) bc::lire&astore2
                                (identificateur-nom (second item)) next))))
           :finally (return next)))

       (:rep-1     (gen bc::pushi 1 next))
       (:rep       (gen bc::pushi (numero-valeur (second stat)) next))
       (:rep-var   (gen bc::push-val 'id::$index bc::dup bc::pushi 1 bc::add
                        bc::pop&store 'id::$index bc::aref1&push-val 'id::$vals
                        next))


       ;; AFFICHER expr…
       ;; --> (:afficher nil expr…)
       ;;
       ;; AFFICHER [spec…] expr…
       ;; --> (:afficher (spec…) expr…)                 

       ;; First, the expr… are evaluated and stored in a TABLEAU $VALS[number of expr…]
       ;; Then $INDEX is set to 1,
       ;; and finally the :afficher-* codops are generated.
       ;; Each :afficher-* operation should use $VALS[$INDEX] and up, incrementing $INDEX.
       
       ;; ['litchaine']
       ;; --> spec ::= (:spec-chaine (:rep-1) litchaine)    ==> (rep litchaine :afficher-u)
       ;; [42'litchaine']
       ;; --> spec ::= (:spec-chaine (:rep 42) litchaine)   ==> (rep litchaine :afficher-u)
       ;; [*'litchaine']
       ;; --> spec ::= (:spec-chaine (:rep-var) litchaine)  ==> (rep litchaine :afficher-u)

       ;; [/]
       ;; --> spec ::= (:spec-slash (:rep-1))      ==> (rep :afficher-newline)
       ;; [42/]
       ;; --> spec ::= (:spec-slash (:rep 42))     ==> (rep :afficher-newline)
       ;; [*/]
       ;; --> spec ::= (:spec-slash (:rep-var))    ==> (rep :afficher-newline)

       ;; X --> :spec-space    ==> (rep :afficher-space)
       ;; C --> :spec-cr       ==> (rep :afficher-cr)
       ;; L --> :spec-nl       ==> (rep :afficher-nl)

       ;; For the following specifiers, only fixed repeatitions are allowed: :rep-1 and (:rep n)
       ;; U --> :spec-u        ==> (rep :afficher-u)
       ;; Fe.d --> (:spec-f rep e d) ==> (rep e d :afficher-f)
       ;; Ee.d --> (:spec-d rep e d) ==> (rep e d :afficher-e)
       
       ;; This rules means that the number of expressions processed is
       ;; known at compilation time.
       
       ;; The format specifiers and the expressions are processed in
       ;; parallel, threfore the following program:
       ;;
       ;; 10 AFFICHER['(',U,')']&P(42)
       ;; 11 TERMINER
       ;; 100 PROCEDURE &P(X)
       ;; 101 AFFICHER['[',U,']']X
       ;; 102 RESULTAT X
       ;;
       ;; should print:
       ;;
       ;; ([42]42)

       (:afficher
        ;; (:afficher nil expr...)      --> :pushi n expr... :afficher-u
        ;; (:afficher (form...) expr...)
        ;; (:afficher (form...))

        ;; TODO: the $INDEX and $VALS variables should be "local"
        ;; (gen  BC::LIBERER 'ID::$VALS next)
        (let ((result '())
              (exprs (cddr stat)))
          (labels ((collect (code)
                     (setf result (nconc result (copy-list code))))
                   (spec-simple (spec format)
                     (if (atom spec)
                         (error "SPECIFICATEUR DE FORMAT INVALIDE ~S" format)
                         (case (first spec)
                           (:rep-1   (collect (gen bc::pushi 1 nil)))
                           (:rep     (collect (gen bc::pushi (numero-valeur (second spec)) nil)))
                           (:rep-var (when (null exprs)
                                       (error "IL MANQUE AU MOINS UNE EXPRESSION POUR LE SPECIFICATEUR DE FORMAT ~S" format))
                                     (collect (generate-statement (pop exprs) nil)))
                           (otherwise
                            (error "SPECIFICATEUR DE FORMAT INVALIDE ~S" format)))))
                   (spec-expr (spec op format)
                     (if (atom spec)
                         (error "SPECIFICATEUR DE FORMAT INVALIDE ~S" format)
                         (case (first spec)
                           (:rep-1 (when (null exprs)
                                     (error "IL MANQUE AU MOINS UNE EXPRESSION POUR LE SPECIFICATEUR DE FORMAT ~S" format))
                                   (collect (generate-statement (pop exprs) nil))
                                   (collect op))
                           (:rep   (loop
                                     :repeat (numero-valeur (second spec))
                                     :do (progn
                                           (when (null exprs)
                                             (error "IL MANQUE AU MOINS UNE EXPRESSION POUR LE SPECIFICATEUR DE FORMAT ~S" format))
                                           (collect (generate-statement (pop exprs) nil))
                                           (collect op))))
                           (otherwise
                            (error "SPECIFICATEUR DE FORMAT INVALIDE ~S" format))))))
            (if (second stat)
                (loop
                  :for format :in (second stat)
                  :do (ecase (first format)
                        (:spec-chaine
                         (spec-simple (second format) format)
                         (collect (gen bc::pushi (third format) bc::afficher-chaine nil)))
                        ((:spec-slash :spec-space :spec-cr :spec-nl)
                         (spec-simple (second format) format)
                         (collect (gen (case (first format)
                                         (:spec-slash bc::afficher-newline)
                                         (:spec-space bc::afficher-space)
                                         (:spec-cr    bc::afficher-cr)
                                         (:spec-nl    bc::afficher-nl))
                                       nil)))
                        ((:spec-f :spec-e)
                         (spec-expr (second format) (gen (third format)
                                                         (fourth format)
                                                         (case (first format)
                                                           (:spec-f bc::afficher-f)
                                                           (:spec-e bc::afficher-e))
                                                         nil) format))
                        (:spec-u
                         (spec-expr (second format) (gen bc::afficher-u nil) format)))
                  :finally (loop
                             :for expr :in exprs
                             :do (collect (gen (generate-statement expr nil) bc::afficher-u nil))))
                ;; (5) afficher[{n}u]expr...
                (loop
                  :for expr :in (cddr stat)
                  :do (progn
                        (collect (generate-statement expr nil))
                        (collect (gen bc::afficher-u nil))))))
          (nconc result next)))

       
       ;; (setf next (gen bc::pushi (length (cddr stat)) bc::pop&store 'id::$valscnt next))
       ;; (setf next (gen bc::pushi 1 bc::pop&store 'id::$index next))
       ;; (4) set $index to 1
       ;; to avoid too much stack usage, we evalute each expression
       ;; and store it into $vals in turn.
       ;; (3) pop one expression and store them in $vals[$index]
       ;; (2) push one expression 
       ;; (loop
       ;;    :for expr :in (reverse (cddr stat))
       ;;    :for i :from (length (cddr stat)) :downto 1 
       ;;    :do (setf next (gen  bc::pushi i bc::pop&astore1 'id::$vals next))
       ;;    :do (setf next (generate-statement expr next)))
       ;; (1) declare the tableau $vals[n]
       ;; (gen  bc::pushi (length (cddr stat)) bc::tableau1 'id::$vals next)


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
        ;; TODO: this creates the variables var and varstat too (TABLEAU, CHAINE or real)
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



(defmacro dolist/separator ((var list separator) &body body)
  (let ((vsep (gensym))
        (esep (gensym)))
    `(loop
       :with ,esep = ,separator
       :for ,var :in ,list
       :for ,vsep = "" :then ,esep
       :do (princ ,vsep) (locally ,@body))))


(defun unparse-tree (stat)
  (macrolet ((with-parens (item)
               (let ((vitem (gensym)))
                 `(let ((,vitem ,item))
                    (if (atom ,vitem)
                        (unparse-tree ,vitem)
                        (progn
                          (princ "(")
                          (unparse-tree ,vitem)
                          (princ ")")))))))
    (etypecase stat
      ((or integer nombre symbol)
       (princ stat))
      (token
       (ecase (token-kind stat)
         (tok-chaine         (princ (token-text stat)))
         (tok-identificateur (princ (identificateur-nom stat)))
         (tok-nombre         (princ (nombre-valeur stat)))
         (tok-numero         (princ (numero-valeur stat)))
         (tok-commentaire    (princ (token-text stat)))))
      (cons
       (ecase (first stat)

         ((:neg) (princ "-")    (with-parens (second stat)))

         ((:non) (princ "NON ") (with-parens (second stat)))

         ((:moins :plus :fois :divise :puissance :concat
                  :lt :le :gt :ge :eg :ne)
          (with-parens (second stat))
          (princ (case (first stat)
                   (:moins "-")
                   (:plus  "+")
                   (:fois  "*")
                   (:divise "/")
                   (:puissance "^")
                   (:concat "!")
                   (:lt "<")
                   (:le "<=")
                   (:gt ">")
                   (:ge ">=")
                   (:eg "=")
                   (:ne "#")))
          (with-parens (third stat)))

         (:xi
          (princ "SI ")
          (unparse-tree (second stat))
          (princ " ALORS ")
          (unparse-tree (third stat))
          (princ " SINON ")
          (unparse-tree (fourth stat)))

         ((:ou :et)
          (dolist/separator (item (rest stat) (case (first stat)
                                                (:ou " OU ")
                                                (:et " ET ")))
            (unparse-tree item)))

         ((:commentaire :terminer :pause :aller-en :retour :retour-en :resultat
                        :liberer :chaine :tableau
                        :lire :garer :charger :supprimer :executer)
          (princ (case (first stat)
                   (:commentaire "")
                   (:terminer   "TERMINER")
                   (:pause      "PAUSE")
                   (:aller-en   "ALLER EN ")
                   (:retour     "RETOUR")
                   (:resultat   "RESULTAT ")
                   (:retour-en  "RETOUR EN ")
                   (:liberer    "LIBERER ")
                   (:chaine     "CHAINE ")
                   (:tableau    "TABLEAU ")
                   (:lire       "LIRE ")
                   (:garer      "GARER ")
                   (:charger    "CHARGER ")
                   (:supprimer  "SUPPRIMER ")
                   (:executer   "EXECUTER ")))
          (dolist/separator (item (rest stat) ",")
            (unparse-tree item)))

         ((:adecl :aval :aref)
          (princ (identificateur-nom (second stat)))
          (princ "[")
          (unparse-tree (third stat))
          (when (cdddr stat)
            (princ ",")
            (unparse-tree (fourth stat)))
          (princ "]"))

         ((:vval :vref)
          (princ (identificateur-nom (second stat))))

         ((:fonction :appel)
          (princ (identificateur-nom (second stat)))
          (princ "(")
          (dolist/separator (item (cddr stat) ",")
            (unparse-tree item))
          (princ ")"))
         
         (:decl-procedure
          (princ "PROCEDURE ")
          (princ (identificateur-nom (second stat)))
          (princ "(")
          (dolist/separator (item (third stat) ",")
            (unparse-tree item))
          (princ ")")
          (when (fourth stat)
            (princ " LOCAL ")
            (dolist/separator (item (fourth stat) ",")
              (unparse-tree item))))
         

         (:affectation
          (unparse-tree (second stat))
          (princ "_")
          (unparse-tree (third stat)))


         (:afficher
          (if (second stat)
              (progn (princ "AFFICHER[")
                     (dolist/separator (item (second stat) ",")
                       (ecase (first (second item))
                         (:rep-1     )
                         (:rep       (princ (numero-valeur (second (second item)))))
                         (:rep-var   (princ "*")))
                       (ecase (first item)
                         (:spec-chaine
                          (unparse-tree (third item)))
                         ((:spec-slash :spec-space :spec-cr :spec-nl)
                          (princ (case (first item)
                                   (:spec-slash "/")
                                   (:spec-space "X")
                                   (:spec-cr    "C")
                                   (:spec-nl    "L"))))
                         ((:spec-f :spec-e)
                          (princ (case (first item)
                                   (:spec-f "F")
                                   (:spec-e "E")))
                          (princ (third item))
                          (princ ".")
                          (princ (fourth item)))
                         (:spec-u
                          (princ "U"))))
                     (princ "]"))
              (princ "AFFICHER "))
          (dolist/separator (item (cddr stat) ",")
            (unparse-tree item)))


         (:si
          (princ "SI ")
          (unparse-tree (second stat))
          (princ " ALORS ")
          (unparse-tree (third stat))
          (when (cdddr stat)
            (princ " SINON ")
            (unparse-tree (fourth stat))))

         ((:faire-jusqu-a :faire-tant-que) ;; lino ident init pas jusqua|tantque)
          (princ "FAIRE ")
          (unparse-tree (second stat))
          (princ " POUR ")
          (unparse-tree (third stat))
          (princ "_")
          (unparse-tree (fourth stat))
          (unless (= 1 (fifth stat))
            (princ " PAS ")
            (unparse-tree (fifth stat)))
          (princ (case (first stat)
                   (:faire-jusqu-a  " JUSQUA ")
                   (:faire-tant-que " TANT QUE ")))
          (unparse-tree (sixth stat)))

         
         (:debut
          (loop
            :for item :in (cddr stat)
            :initially (princ "BEGIN ") (unparse-tree (second stat))
            :do (princ ";") (unparse-tree item)
            :finally (princ " END"))))))))


(defun unparse-slist (parse-tree)
  (with-output-to-string (*standard-output*)
    (with-standard-io-syntax
     (cond
       ((null parse-tree))              ; nothing to do
       ((atom parse-tree)
        (format *error-output* "~&ERREUR: JE TROUVE UN ATOME: ~S~%" parse-tree))
       ((eq (car parse-tree) :liste-instructions)
        (dolist/separator (item (cdr parse-tree) ";")
          (unparse-tree item)))
       ((eq (car parse-tree) :ligne-programme)
        (princ (numero-valeur (cadr parse-tree)))
        (unless (and (listp (caddr parse-tree))
                     (eql ':commentaire (caaddr parse-tree)))
          (princ " "))
        (dolist/separator (item (cddr parse-tree) ";")
          (unparse-tree item)))
       (t
        (format *error-output* "~&ERREUR: JE TROUVE UNE LISTE INVALIDE: ~S~%" parse-tree))))))


(defun generate-slist (slist final)    ; generate bytes for a statement list
  (if slist
      (generate-statement (car slist) (generate-slist (cdr slist) final))
      (list final)))

(defun compile-slist (slist final)            ; compile statement list
  (let ((bytes (generate-slist slist final)))
    (make-array (length bytes) :initial-contents bytes)))


(defun compile-lse-line-parse-tree (parse-tree)
  ;; (let ((*print-pretty* nil)) (terpri) (princ "Parse tree: ") (prin1 parse-tree) (terpri))
  (cond
    ((null parse-tree) (list 0 #()))           ; nothing to do
    ((atom parse-tree)
     (format *error-output* "~&ERREUR: JE TROUVE UN ATOME: ~S~%" parse-tree))
    ((eq (car parse-tree) :liste-instructions)
     (list 0 (compile-slist (cdr parse-tree) BC::TERMINER)))
    ((eq (car parse-tree) :ligne-programme)
     (list (numero-valeur (cadr parse-tree)) (compile-slist (cddr parse-tree) BC::NEXT-LINE)))
    (t
     (format *error-output* "~&ERREUR: JE TROUVE UNE LISTE INVALIDE: ~S~%" parse-tree))))



;; lire --> vref
;; si --> vval/aval
;; adecl --> vref/aref


;; zebu:
#-(and) 
(defun parse-lse (scanner)
  (handler-case
      (lr-parse (progn
                  (advance-line)
                  (lambda (parser-data)
                    (scan-next-token scanner parser-data)))
                (lambda (msg) (error "ERREUR: ~A" msg))
                (find-grammar "LSE"))
    (error (err)
      (format t "Parsing error ~A" err)
      (signal err))))



(defun compile-lse-line (source-line)
  (let* ((*scanner*  (make-instance 'lse-scanner :source source-line))
         (parse-tree (parse-lse *scanner*)))
    (append (compile-lse-line-parse-tree parse-tree)
            (list (unparse-slist parse-tree)))))


(defun compile-lse-stream (stream)
  (let ((*scanner* (make-instance 'lse-scanner :source stream)))
   (loop
     :until (typep (scanner-current-token *scanner*) 'tok-eof)
     :for source-line = (scanner-buffer *scanner*)
     :for parse-tree = (parse-lse *scanner*)
     :when parse-tree
     :collect (append (compile-lse-line-parse-tree parse-tree)
                      (list (unparse-slist parse-tree))))))


(defun compile-lse-file (source &optional name)
  (with-open-file (stream source
                          :direction :input
                          :if-does-not-exist nil)
    (if stream
        (compile-lse-stream stream)
        (error 'lse-file-error
               :pathname source
               :format-control "PAS DE PROGRAMME NOMME '~A' ACCESSIBLE"
               :format-arguments (list name)))))



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
                    (incf pc (length line))))))
  (values))


(defun decompile-lse-line (lino code)
  (declare (ignore lino))
  (third code))

;; (defun decompile-lse-line (lino byte-code)
;;   (with-output-to-string (source)
;;     (princ lino source) (princ " " source)
;;     (flet ((nbop (cop) (cdr (gethash cop *cop-info* '(0 . 0))))
;;            (disa (cop) (car (gethash cop *cop-info* (cons cop 0)))))
;;       (let ((pc 0)
;;             (listing '())
;;             (labels '()))
;;         (loop :while (< pc (length byte-code)) :do
;;           (let ((cop (list (disa (aref byte-code pc))))
;;                 (noa       (nbop (aref byte-code pc))))
;;             (incf pc)
;;             (dotimes (n noa)
;;               (setf cop (nconc cop (list (aref byte-code pc))))
;;               (incf pc))
;;             (push cop listing)
;;             (when (member (first cop) bc::*branches*)
;;               (push (+ pc (second cop)) labels))))
;;         (setf listing (nreverse listing))
;;         (princ listing source)
;;         #-(and)
;;         (loop
;;           :initially (format t "~&")
;;           :with pc = 0
;;           :for line :in listing
;;           :do (progn (if (member pc labels)
;;                          (format t "@~6:A " pc)
;;                          (format t "        "))
;;                      (format t "~32:A"
;;                              (with-output-to-string (out)
;;                                (princ "(" out)
;;                                (let ((first t))
;;                                  (dolist (item line)
;;                                    (if first
;;                                        (setf first nil)
;;                                        (princ " " out))
;;                                    (if (symbolp item)
;;                                        (princ item out)
;;                                        (prin1 item out))))
;;                                (princ ")" out)))
;;                      (if (member (first line) bc::*branches*)
;;                          (format t " ; @~A~%"  (+ pc (length line) (second line)))
;;                          (format t "~%"))
;;                      (incf pc (length line))))))
;;     #-(and)
;;     (loop
;; 
;;       (case
;; 
;;           
;;           (cl:defparameter *branches* '(
;;                                         BALWAYS ;       BALWAYS offset
;;                                         BTRUE   ; test  BTRUE   offset
;;                                         BFALSE  ; test  BFALSE  offset
;;                                         BNEVER  ;       BNEVER  offset
;;                                         ))
;;         
;;         (cl:defparameter *0* '(
;;                                DUP ; arg DUP
;; 
;;                                NON ; arg NON 
;;                                OU  ; arg1 arg2 OU
;;                                ET  ; arg1 arg2 ET
;; 
;;                                EG  ; arg1 arg2 EG
;;                                NE  ; arg1 arg2 NE
;;                                LE  ; arg1 arg2 LE
;;                                LT  ; arg1 arg2 LT
;;                                GE  ; arg1 arg2 GE
;;                                GT  ; arg1 arg2 GT
;; 
;;                                CONCAT ; arg1 arg2 CONCAT
;; 
;;                                NEG ; arg NEG
;;                                ADD ; arg1 arg2 ADD
;;                                SUB ; arg1 arg2 SUB
;;                                MUL ; arg1 arg2 MUL
;;                                DIV ; arg1 arg2 DIV
;;                                POW ; arg1 arg2 POW
;; 
;; 
;;                                AFFICHER-E        ; rep e f AFFICHER-E
;;                                AFFICHER-F        ; rep e f AFFICHER-F
;;                                AFFICHER-CR       ; rep AFFICHER-CR
;;                                AFFICHER-CHAINE   ; rep chaine AFFICHER-CHAINE
;;                                AFFICHER-NEWLINE  ; rep AFFICHER-NEWLINE
;;                                AFFICHER-NL       ; rep AFFICHER-NL
;;                                AFFICHER-SPACE    ; rep AFFICHER-SPACE
;;                                AFFICHER-U        ; nexpr AFFICHER-U
;; 
;;                                NEXT-LINE  ; NEXT-LINE
;;                                RETOUR     ; RETOUR
;;                                RETOUR-EN  ; line RETOUR-EN
;;                                RESULT     ; arg RESULT
;;                                GOTO       ; line GOTO
;; 
;;                                TANT-QUE                 ; test TANT-QUE
;;                                CHARGER                  ; enr fic CHARGER
;;                                SUPPRIMER-ENREGISTREMENT ; enr fic SUPPRIMER-ENREGISTREMENT
;;                                SUPPRIMER-FICHIER        ;     fic SUPPRIMER-FICHIER
;;                                EXECUTER  ; fic lin EXECUTED
;;                                PAUSE     ; PAUSE
;;                                TERMINER  ; TERMINER
;; 
;;                                PROCEDURE
;;                                comment ; COMMENT comment
;;                                ))
;; 
;;         (cl:defparameter *1* '(
;;                                AREF1&PUSH-REF ; idx AREF1&PUSH-REF identifier
;;                                AREF1&PUSH-VAL ; idx AREF1&PUSH-VAL identifier
;;                                AREF2&PUSH-REF ; idx1 idx2 AREF2&PUSH-REF identifier
;;                                AREF2&PUSH-VAL ; idx1 idx2 AREF2&PUSH-VAL identifier
;; 
;;                                POP&ASTORE1    ; val idx POP&ASTORE1 identifier
;;                                POP&ASTORE2    ; val idx1 idx2 POP&ASTORE2 identifier
;;                                POP&STORE      ; val POP&STORE identifier
;; 
;;                                PUSH-REF       ; PUSH-REF identifier
;;                                PUSH-VAL       ; PUSH-REF identifier
;;                                PUSHI          ; PUSHI immediate-value
;; 
;;                                LIRE&STORE        ; LIRE&STORE ident
;;                                LIRE&ASTORE1      ; index LIRE&ASTORE1 ident
;;                                LIRE&ASTORE2      ; idx1 idx2 LIRE&ASTORE1 ident
;; 
;;                                CHAINE         ; CHAINE identifier
;;                                TABLEAU1       ; dim TABLEAU1 identifier
;;                                TABLEAU2       ; dim1 dim2 TABLEAU2 identifier
;;                                LIBERER        ; LIBERER identifier
;; 
;;                                BALWAYS    ;      BALWAYS offset
;;                                BTRUE      ; test BTRUE   offset
;;                                BFALSE     ; test BFALSE  offset
;;                                BNEVER     ;      BNEVER  offset
;; 
;;                                FAIRE-JUSQU-A   ; lino init pas jusqua FAIRE-JUSQU-A ident
;;                                FAIRE-TANT-QUE  ; lino init pas FAIRE-TANT-QUE ident
;;                                         ; test TANT-QUE
;;                                GARER           ; enr fic GARER identificateur
;;                                ))
;; 
;;         (cl:defparameter *2* '(CALL            ; CALL identificateur-procedure nombre-d-argument
;;                                ))))))
;; 
;; (destructuring-bind (lino . code)
;;     (compile-lse-line "10 si a=b alors afficher['egal ',u]a sinon afficher['different ',2u]a,b")
;;   (decompile-lse-line lino code))
;; "10 ((PUSH-REF A) (PUSH-REF B) (EG) (BFALSE 11) (PUSHI 1) (PUSHI 48) (egal ) (AFFICHER-CHAINE) (PUSH-REF A) (AFFICHER-U) (BALWAYS 12) (PUSHI 1) (PUSHI 48) (different ) (AFFICHER-CHAINE) (PUSH-REF A) (AFFICHER-U) (PUSH-REF B) (AFFICHER-U) (NEXT-LINE))"


;;(progn (terpri)(disassemble-lse bc))

;;(:Ligne-Programme numero instr...)
;;(:liste-instructions instr...)

;; (:Ligne-Programme numero instr...)
;; ==> compile instr... and store vector in lino.
;; 
;; (:ligne-programme numero (:decl-procedure  decl-procedure))
;; ==> compile decl-procedure; store vector in lino; store proc.ident in proctable.


(defun test/parse-stream (src)
  (let ((*scanner* (make-instance 'lse-scanner :source src)))
   (loop
     :until (typep (scanner-current-token *scanner*) 'tok-eof)
     :collect  (parse-lse *scanner*))))


(defun test/parse-file (path)
  (with-open-file (src path)
    (test/parse-stream src)))

(defun test/parse-string (source)
  (with-input-from-string (src source)
    (test/parse-stream src)))


(defun test/compile-lse-stream (src)
  (loop
    :for line = (read-line src nil nil)
    :do (terpri) (princ ";; |  ") (write-string line)
    :while line
    :do (let ((comp (compile-lse-line line)))
          (print comp)
          (print (first comp))
          (print (disassemble-lse (second comp))))
    :finally (terpri) (finish-output))
  (values))

(defun test/compile-lse-file (path)
  (with-open-file (src path)
    (test/compile-lse-stream src))
  (values))

(defun test/compile-lse-string (source)
  (with-input-from-string (src source)
    (test/compile-lse-stream src))
  (values))


;; (test/parse-file #P"~/src/pjb/lse-cl/SYNTERR.LSE")
;; (test/parse-file #P"~/src/pjb/lse/BOURG/BOUR.LSE")
;; (test/parse-string "18*")


;; (compile-lse-file #P "~/src/pjb/lse/BOURG/BOUR.LSE")
;; (compile-lse-file #P "~/src/pjb/lse-cl/TESTCOMP.LSE")

;; (test/compile-lse-file #P "~/src/pjb/lse-cl/TESTCOMP.LSE")



;; Local Variables:
;; (cl-indent 'dolist/separator 1)
;; End:

;;;; THE END ;;;;
