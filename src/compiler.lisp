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
;;;;    2012-02-19 <PJB> Implemented the expression unparser.
;;;;    2005-08-25 <PJB> Created.
;;;;BUGS
;;;;    LIRE must take into account the type of the variable.
;;;;    NON est une extension; voir comment le rendre optionel.
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2005
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


;;----------------------------------------------------------------------
(cl:in-package "COM.INFORMATIMAGO.LSE")
;;----------------------------------------------------------------------
(enable-byte-code-reader-macro)


;; The compiler module provides three features:
;; 
;; 1- unparse the syntactic tree produced by the parser into a LSE source
;;    text.  This is used by the DECODE command.  We could as well use
;;    the original source text, but this allows us to produce a nicely
;;    formated (ie. without extraneous spaces or parentheses) code.
;; 
;;    UNPARSE-SLIST
;; 
;; 2- compiles the LSE source code to byte-code vectors.
;;    The scanner and parser are provided by lse-scanner.lisp
;;    and lse-parser.lisp. 
;; 
;;    GENERATE-EXPRESSION
;;    GENERATE-STATEMENT
;; 
;;    COMPILE-LSE-LINE-PARSE-TREE
;;    COMPILE-LSE-LINE
;;    COMPILE-LSE-STREAM
;;    COMPILE-LSE-FILE
;; 
;;    DECOMPILE-LSE-LINE
;; 
;;    We don't decompile the byte-code, so that we may implement
;;    "optimizations".  Already,  the code generated for AFFICHER would
;;    produce sometimes a different source if decompiled.
;; 
;; 3- disassemble the byte-code vectors.  This is used for debugging and
;;    pedagogical purposes.
;; 
;;    DISASSEMBLE-LSE
;;
;;
;; Note: for now, the byte-code vectors also contain identifiers
;;       (symbols) and strings.  Since identifiers are limited to
;;       [A-Z][A-Z0-9]{0,4}, (with possibly a '&'  prefix for
;;       procedure identifiers),  they could be encoded as a 27-bit
;;       integer easily serialized as 4 bytes.  Strings could be
;;       serialized as a 2-byte length followed by UTF-8 codes.   But
;;       since the virtual machine is written in lisp, we really don't
;;       need to do that. (We could also use symbols instead of byte
;;       codes, see byte-code.lisp).
;;



;;;---------------------------------------------------------------------
;;; 1- Unparser
;;;---------------------------------------------------------------------

(defmacro dolist/separator ((var list separator) &body body)
  (let ((vsep (gensym))
        (esep (gensym)))
    `(loop
       :with ,esep = ,separator
       :for ,var :in ,list
       :for ,vsep = "" :then ,esep
       :do (princ ,vsep) (locally ,@body))))



;; (:plus (:fois (:vref a) (:vref a))
;;        (:fois (:vref b) (:vref b)))
;; a*a+b*b

;; (:fois (:plus (:vref a) (:vref a))
;;        (:plus (:vref b) (:vref b)))
;; (a+a)*(b+b)

;; Two signs cannot be adjacent: a*-b must be written a*(-b)

;; for (op1 (op2 … …) …)
;;
;; if (<= (operator-priority op1) (operator-priority op2))
;; then no need for parentheses  … op2 … op1 … == (… op2 …) op1 …
;;
;; if (> (operator-priority op1) (operator-priority op2))
;; then we need parentheses  (… op2 …) op1 …
;;                 otherwise  … op2 … op1 … == … op2 (… op1 …)


(defparameter *priority-levels*
  '((:ou)
    (:et)
    (:non)
    (:eg :ne :lt :le :gt :ge)
    (:moins :plus :concat)
    (:neg)
    (:divise :fois)
    (:puissance)
    (:xi :fonction :vval :vref :aval :aref)))

(defun operator-priority (operator)
  (position operator *priority-levels*
            :test (function member)))




;; a+b+c
;; (:plus a (:plus b c))

;; a-b-c
;; (:moins (:moins a b) c)

;; a-(b-c)
;; (:moins a (:moins b c))

;; a/b/c
;; (:divise (:divise a b) c)

;; a/(b/c)
;; (:divise a (:divise b c))


;; (disjonction (conjonction { :OU conjonction }))
;; (conjonction (condition   { :ET condition }))
;; (condition   (:NON condition)
;;              (expression (:EG :NE :LT :LE :GT :GE) expression))
;; (expression  (:neg expression)
;;              (terme   { (:moins :plus :concat) terme))
;; (terme       (facteur { (:fois :divise) facteur))
;; (facteur     (simple  { :puissance simple }))
;; (simple      (:xi disjonction expression expression)
;;              (:fonction procident (liste-arguments))
;;              (:VVAL procident)
;;              ( \( disjonction \) )
;;              (:vref identifier)
;;              (:vval identifier)
;;              (:aref identifier expression [expression])
;;              (:aval identifier expression [expression])
;;              chaine
;;              nombre
;;              numero)


(defun unparse-expression (expr)
  (macrolet ((with-parens ((op1 op2) item)
               (let ((vitem (gensym))
                     (vop1  (gensym))
                     (vop2  (gensym)))
                 `(let ((,vop1 ,op1)
                        (,vop2 ,op2)
                        (,vitem ,item))
                    (if (or (atom ,vitem)
                            (atom ,vop2)
                            (<= (operator-priority (first ,vop1))
                                (operator-priority (first ,vop2))))
                        (unparse-expression ,vitem)
                        (progn
                          (princ "(")
                          (unparse-expression ,vitem)
                          (princ ")")))))))
   (etypecase expr
     ((or integer nombre symbol)
      (princ expr))
     (token
      (ecase (token-kind expr)
        (tok-identificateur (princ (identificateur-nom expr)))
        (tok-procident      (princ (identificateur-nom expr)))
        (tok-chaine         (princ (token-text expr)))
        (tok-nombre         (princ (nombre-valeur expr)))
        (tok-numero         (princ (numero-valeur expr)))))
     (cons
      (ecase (first expr)

        ((:neg) (princ "-")    (with-parens (expr (second expr))
                                 (second expr)))

        ((:non) (princ "NON ") (with-parens (expr (second expr))
                                 (second expr)))

        ((:moins :divise)
         (if (or (atom (second expr))
                 (eql (first expr) (first (second expr))))
             (unparse-expression (second expr))
             (with-parens (expr (second expr))
               (second expr)))
         (princ (case (first expr)
                  (:moins  "-")
                  (:divise "/")))
         (if (and (consp (third expr))
                  (eql (first expr) (first (third expr))))
             (progn
               (princ "(") (unparse-expression (third expr)) (princ ")"))
             (with-parens (expr (third expr))
               (third expr))))
        
        ((:plus :concat
                :fois 
                :puissance 
                :lt :le :gt :ge :eg :ne)
         (with-parens (expr (second expr))
           (second expr))
         (princ (case (first expr)
                  (:plus  "+")
                  (:fois  "*")
                  (:puissance "^")
                  (:concat "!")
                  (:lt "<")
                  (:le "<=")
                  (:gt ">")
                  (:ge ">=")
                  (:eg "=")
                  (:ne "#")))
         (with-parens (expr (third expr))
           (third expr)))

        (:xi
         (princ "SI ")
         (unparse-expression (second expr))
         (princ " ALORS ")
         (unparse-expression (third expr))
         (princ " SINON ")
         (unparse-expression (fourth expr)))

        ((:ou :et)
         (dolist/separator (item (rest expr) (case (first expr)
                                               (:ou " OU ")
                                               (:et " ET ")))
           (with-parens (expr item)
             item)))

        ((:adecl :aval :aref)
         (princ (identificateur-nom (second expr)))
         (princ "[")
         (unparse-expression (third expr))
         (when (cdddr expr)
           (princ ",")
           (unparse-expression (fourth expr)))
         (princ "]"))

        ((:vval :vref)
         (princ (identificateur-nom (second expr))))

        ((:fonction :appel)
         (princ (identificateur-nom (second expr)))
         (princ "(")
         (dolist/separator (item (cddr expr) ",")
           (unparse-expression item))
         (princ ")")))))))


(defun unparse-tree (expr)
  (macrolet ((with-parens (item)
               (let ((vitem (gensym)))
                 `(let ((,vitem ,item))
                    (if (atom ,vitem)
                        (unparse-tree ,vitem)
                        (progn
                          (princ "(")
                          (unparse-tree ,vitem)
                          (princ ")")))))))
    (etypecase expr
      ((or integer nombre symbol)
       (princ expr))
      (token
       (ecase (token-kind expr)
         (tok-chaine         (princ (token-text expr)))
         (tok-identificateur (princ (identificateur-nom expr)))
         (tok-nombre         (princ (nombre-valeur expr)))
         (tok-numero         (princ (numero-valeur expr)))
         (tok-commentaire    (princ (token-text expr)))))
      (cons
       (ecase (first expr)
         ((:neg :non
                :moins :plus :fois :divise :puissance :concat
                :lt :le :gt :ge :eg :ne
                :xi :ou :et
                :adecl :aval :aref
                :vval :vref
                :fonction :appel)
          (unparse-expression expr))
         
         ((:commentaire :terminer :pause :aller-en :retour :retour-en :resultat
                        :liberer :chaine :tableau
                        :lire :garer :charger :supprimer :executer)
          (princ (case (first expr)
                   (:commentaire "")
                   (:terminer   "TERMINER")
                   (:pause      "PAUSE")
                   (:aller-en   "ALLER EN ")
                   (:resultat   "RESULTAT ")
                   (:retour-en  "RETOUR EN ")
                   (:retour     "RETOUR")
                   (:liberer    "LIBERER ")
                   (:chaine     "CHAINE ")
                   (:tableau    "TABLEAU ")
                   (:lire       "LIRE ")
                   (:garer      "GARER ")
                   (:charger    "CHARGER ")
                   (:supprimer  "SUPPRIMER ")
                   (:executer   "EXECUTER ")))
          (dolist/separator (item (rest expr) ",")
            (unparse-tree item)))
         
         (:decl-procedure
          (princ "PROCEDURE ")
          (princ (identificateur-nom (second expr)))
          (princ "(")
          (dolist/separator (item (third expr) ",")
            (unparse-tree item))
          (princ ")")
          (when (fourth expr)
            (princ " LOCAL ")
            (dolist/separator (item (fourth expr) ",")
              (unparse-tree item))))

         (:affectation
          (unparse-expression (second expr))
          (princ "_")
          (unparse-expression (third expr)))

         (:afficher
          (if (second expr)
              (progn (princ "AFFICHER[")
                     (dolist/separator (item (second expr) ",")
                       (ecase (first (second item))
                         (:rep-1     )
                         (:rep       (princ (numero-valeur (second (second item)))))
                         (:rep-var   (princ "*")))
                       (ecase (first item)
                         (:spec-chaine
                          (unparse-expression (third item)))
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
                          (unparse-expression (third item))
                          (princ ".")
                          (unparse-expression  (fourth item)))
                         (:spec-u
                          (princ "U"))))
                     (princ "]"))
              (princ "AFFICHER "))
          (dolist/separator (item (cddr expr) ",")
            (unparse-expression item)))


         (:si
          (princ "SI ")
          (unparse-expression (second expr))
          (princ " ALORS ")
          (unparse-tree (third expr))
          (when (cdddr expr)
            (princ " SINON ")
            (unparse-tree (fourth expr))))

         ((:faire-jusqu-a :faire-tant-que) ;; lino ident init pas jusqua|tantque)
          (princ "FAIRE ")
          (unparse-expression (second expr))
          (princ " POUR ")
          (unparse-expression (third expr))
          (princ "_")
          (unparse-expression (fourth expr))
          (let ((pas (typecase (fifth expr)
                       (tok-numero (numero-valeur (fifth expr)))
                       (tok-nombre (nombre-valeur (fifth expr)))
                       (t          (fifth expr)))))
            (unless (and (numberp pas) (= 1 pas))
              (princ " PAS ")
              (unparse-expression (fifth expr))))
          (princ (case (first expr)
                   (:faire-jusqu-a  " JUSQUA ")
                   (:faire-tant-que " TANT QUE ")))
          (unparse-expression (sixth expr)))

         (:debut
          (loop
            :for item :in (cddr expr)
            :initially (princ "DEBUT ") (unparse-tree (second expr))
            :do (princ ";") (unparse-tree item)
            :finally (princ " FIN"))))))))


(defun unparse-slist (parse-tree)
  (with-output-to-string (*standard-output*)
    (with-standard-io-syntax
      (cond
        ((null parse-tree))              ; nothing to do
        ((atom parse-tree)
         (error 'lse-error
                :format-control "INTERNE: UN ATOME ~S DANS L'ARBRE SYNTAXIQUE."
                :format-arguments (list parse-tree)))
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
         (error 'lse-error
                :format-control "INTERNE: UNE LISTE INVALIDE ~S DANS L'ARBRE SYNTAXIQUE."
                :format-arguments (list parse-tree)))))))



;;;---------------------------------------------------------------------
;;; 2- Compiler, byte-code generator.
;;;---------------------------------------------------------------------


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



;; (defun gen (&rest items)
;;   (cond
;;     ((null (cdr items))
;;      (car items))
;;     ((or (numberp (car items))
;;          (stringp (car items))
;;          (and (symbolp (car items))
;;               (or (eq (symbol-package (car items)) (find-package "BC"))
;;                   (eq (symbol-package (car items)) (find-package "ID")))))
;;      (cons (car items) (apply (function gen) (cdr items))))
;;     (t
;;      (generate-statement (car items) (apply (function gen) (cdr items))))))
;; 
;; 
;; 
;; (defun generate-statement (stat next)
;;   (etypecase stat
;;     ((or integer nombre symbol)
;;      (cons stat next))
;;     (token
;;      (ecase (token-kind stat)
;;        (tok-chaine         (gen !pushi (chaine-valeur stat) next))
;;        (tok-nombre         (gen !pushi (nombre-valeur stat) next))
;;        (tok-numero         (gen !pushi (numero-valeur stat) next))
;;        (tok-identificateur (gen (identificateur-nom stat) next))
;;        (tok-commentaire    (gen !comment (token-text stat) next))))
;;     (cons
;;      (ecase (first stat)
;; 
;;        ((:neg :non)
;;         (gen (second stat) (case (first stat)
;;                              (:neg !neg)
;;                              (:non !non)) next))
;;        ((:moins :plus :fois :divise :puissance :concat
;;                 :lt :le :gt :ge :eg :ne)
;;         (gen (second stat) (third stat)
;;              (case (first stat)
;;                (:moins !sub)
;;                (:plus  !add)
;;                (:fois  !mul)
;;                (:divise !div)
;;                (:puissance !pow)
;;                (:concat !concat)
;;                (:lt !lt)
;;                (:le !le)
;;                (:gt !gt)
;;                (:ge !ge)
;;                (:eg !eg)
;;                (:ne !ne)) next))
;;        (:xi
;;         (let* ((else (gen (fourth stat) next))
;;                (offset.e (cons-position next else))
;;                (then (gen (third stat) !balways  offset.e else))
;;                (offset.t (cons-position else then)))
;;           (gen (second stat) !bfalse offset.t then)))
;;        ((:ou :et)
;;         (loop
;;            :with suite = next
;;            :for item :in (reverse (cddr stat))
;;            :do (setf suite (gen item (case (first stat)
;;                                        (:ou !ou)
;;                                        (:et !et)) suite))
;;            :finally (return (gen (second stat) suite))))
;; 
;;        (:commentaire
;;         (gen (second stat) next))
;;        
;;        ((:liberer :chaine)
;;         (loop
;;            :with suite = next
;;            :for item :in (reverse (rest stat))
;;            :do (setf suite (gen (case (first stat)
;;                                   (:liberer !liberer)
;;                                   (:chaine  !chaine))
;;                                 (identificateur-nom item) suite))
;;            :finally (return suite)))
;;        (:tableau
;;         (loop
;;            :for item :in (reverse (rest stat))
;;            :do (setf next (generate-statement item next))
;;            :finally (return next)))
;;        (:adecl
;;         (if (null (cdddr stat))
;;             (gen (third stat) !tableau1
;;                  (identificateur-nom (second stat)) next)
;;             (gen (third stat) (fourth stat) !tableau2
;;                  (identificateur-nom (second stat)) next)))
;;        
;;        ((:aval :aref)
;;         (if (null (cdddr stat))
;;             (gen (third stat)
;;                  (case (first stat)
;;                    (:aval !aref1&push-val)
;;                    (:aref !aref1&push-ref))
;;                  (identificateur-nom (second stat)) next)
;;             (gen (third stat) (fourth stat)
;;                  (case (first stat)
;;                    (:aval !aref2&push-val)
;;                    (:aref !aref2&push-ref))
;;                  (identificateur-nom (second stat)) next)))
;;        ((:vval :vref)
;;         (gen (case (first stat)
;;                (:vval !push-val)
;;                (:vref !push-ref))
;;              (identificateur-nom (second stat)) next))
;; 
;;        ((:fonction :appel)
;;         (loop
;;            :with suite = (gen !call (identificateur-nom (second stat))
;;                               (length (cddr stat)) next)
;;            :for item :in (reverse (cddr stat))
;;            :do (setf suite (gen item suite))
;;            :finally (return suite)))
;; 
;;        
;;        ;; (:decl-procedure ident nil       nil)
;;        ;; (:decl-procedure ident (fpid...) nil)
;;        ;; (:decl-procedure ident nil       (locid...))
;;        ;; (:decl-procedure ident (fpid...) (locid...))
;;        ;; --> :trap-proc-reached *
;;        ;; ==> fpid inter locid == arguments par valeur  ==> copier
;;        ;; ==> fpid diff  locid == arguments par reference
;;        ;; ==> locid diff fpid  == variable locales --> table variable locale pour la proc.
;; 
;;        ;; Parameters by reference
;;        ;; Parameters by value       = Local variables
;;        ;; Global Variables
;;        ;; Local Variables
;; 
;;        
;;     ;; - Appel de  PROCEDURE
;;     ;; 
;;     ;;   We cannot generate vref or vval for parameters of PROCEDUREs
;;     ;;   before knowing what parameters are by reference and what are by
;;     ;;   value, therefore before having seen the PROCEDURE declaration in
;;     ;;   the source.
;;     ;; 
;;     ;;   ⚠ Therefore we can generate the code only globally, just before we
;;     ;;     run the program.  Chaining or loading parts of the program while
;;     ;;     running means we need to recompile it.
;;        
;;        (:decl-procedure
;;         ;; Les procedures ne sont pas executable, !procedure generates an error.
;;         (destructuring-bind (decl name params locals) stat
;;           (make-procedure :name name
;;                           :parameters (mapcar (lambda (param)
;;                                                 (if (member param locals)
;;                                                     `(:par-valeur    ,param)
;;                                                     `(:par-reference ,param)))
;;                                               params)
;;                           :local-variables (local-variables (set-difference locals params))
;;                           :line line
;;                           :offset ))
;;         
;;         (gen !procedure next))
;;        
;;        (:resultat   (gen (second stat) !result next))
;;        (:retour-en  (gen (second stat) !retour-en next))
;;        (:retour     (gen !retour next))
;; 
;;        ;; :result             OR  :retour-en      OR   :retour
;;        ;; -------------------     ---------------     ------------
;;        ;; result    <-- :pop  OR  goto <-- :pop   OR   nothing
;;        ;; 
;;        ;; return-pc <-- sf.return-pc
;;        ;; next-sf   <-- sf.next-sf
;;        ;; argcnt    <-- sf.argcnt
;;        ;; sf        <-- next-sf
;;        ;; :pop-sf
;;        ;; :pop-n argcnt
;;        ;;                
;;        ;; :push  result              --                 --
;;        ;; pc <-- return-pc       OR  pc <-- goto     or pc <-- return-pc
;; 
;;        (:affectation
;;         ;; (:vref ident) expression) --> expression :pop&store ident
;;         ;; (:aref ident expr) expression) --> expression expr :pop&astore1 ident
;;         ;; (:aref ident expr.1 expr.2) expression) --> expression expr.1 expr.2 :pop&astore2 ident
;;         (setf next (cons (identificateur-nom (second (second stat))) next))
;;         (case (length (second stat))
;;           (2  (gen (third stat)
;;                    !pop&store    next))
;;           (3  (gen (third stat) (third (second stat))
;;                    !pop&astore1  next))
;;           (4  (gen (third stat) (third (second stat))
;;                    (fourth (second stat))
;;                    !pop&astore2  next))))
;; 
;;        (:lire
;;         ;; (:vref ident) --> :lire&store ident
;;         ;; (:aref ident expr) -->  expr :lire&astore1 ident
;;         ;; (:aref ident expr.1 expr.2) -->  expr.1 expr.2 :lire&astore2 ident
;;         (loop
;;            :for item :in (reverse (rest stat))
;;            :do (setf next 
;;                      (case (length item)
;;                        (2  (gen !lire&store
;;                                 (identificateur-nom (second item)) next))
;;                        (3  (gen (third item) !lire&astore1
;;                                 (identificateur-nom (second item)) next))
;;                        (4  (gen (third item) (fourth item) !lire&astore2
;;                                 (identificateur-nom (second item)) next))))
;;            :finally (return (gen !beep next))))
;; 
;;        (:rep-1     (gen !pushi 1 next))
;;        (:rep       (gen !pushi (numero-valeur (second stat)) next))
;;        (:rep-var   (gen !push-val 'id::$index !dup !pushi 1 !add
;;                         !pop&store 'id::$index !aref1&push-val 'id::$vals
;;                         next))
;; 
;; 
;;        ;; AFFICHER expr…
;;        ;; --> (:afficher nil expr…)
;;        ;;
;;        ;; AFFICHER [spec…] expr…
;;        ;; --> (:afficher (spec…) expr…)                 
;; 
;;        ;; First, the expr… are evaluated and stored in a TABLEAU $VALS[number of expr…]
;;        ;; Then $INDEX is set to 1,
;;        ;; and finally the :afficher-* codops are generated.
;;        ;; Each :afficher-* operation should use $VALS[$INDEX] and up, incrementing $INDEX.
;;        
;;        ;; ['litchaine']
;;        ;; --> spec ::= (:spec-chaine (:rep-1) litchaine)    ==> (rep litchaine :afficher-u)
;;        ;; [42'litchaine']
;;        ;; --> spec ::= (:spec-chaine (:rep 42) litchaine)   ==> (rep litchaine :afficher-u)
;;        ;; [*'litchaine']
;;        ;; --> spec ::= (:spec-chaine (:rep-var) litchaine)  ==> (rep litchaine :afficher-u)
;; 
;;        ;; [/]
;;        ;; --> spec ::= (:spec-slash (:rep-1))      ==> (rep :afficher-newline)
;;        ;; [42/]
;;        ;; --> spec ::= (:spec-slash (:rep 42))     ==> (rep :afficher-newline)
;;        ;; [*/]
;;        ;; --> spec ::= (:spec-slash (:rep-var))    ==> (rep :afficher-newline)
;; 
;;        ;; X --> :spec-space    ==> (rep :afficher-space)
;;        ;; C --> :spec-cr       ==> (rep :afficher-cr)
;;        ;; L --> :spec-nl       ==> (rep :afficher-nl)
;; 
;;        ;; For the following specifiers, only fixed repeatitions are allowed: :rep-1 and (:rep n)
;;        ;; U --> :spec-u        ==> (rep :afficher-u)
;;        ;; Fe.d --> (:spec-f rep e d) ==> (rep e d :afficher-f)
;;        ;; Ee.d --> (:spec-d rep e d) ==> (rep e d :afficher-e)
;;        
;;        ;; This rules means that the number of expressions processed is
;;        ;; known at compilation time.
;;        
;;        ;; The format specifiers and the expressions are processed in
;;        ;; parallel, threfore the following program:
;;        ;;
;;        ;; 10 AFFICHER['(',U,')']&P(42)
;;        ;; 11 TERMINER
;;        ;; 100 PROCEDURE &P(X)
;;        ;; 101 AFFICHER['[',U,']']X
;;        ;; 102 RESULTAT X
;;        ;;
;;        ;; should print:
;;        ;;
;;        ;; ([42]42)
;; 
;;        (:afficher
;;         ;; (:afficher nil expr...)      --> :pushi n expr... :afficher-u
;;         ;; (:afficher (form...) expr...)
;;         ;; (:afficher (form...))
;; 
;;         ;; TODO: the $INDEX and $VALS variables should be "local"
;;         ;; (gen  !LIBERER 'ID::$VALS next)
;;         (let ((result '())
;;               (exprs (cddr stat)))
;;           (labels ((collect (code)
;;                      (setf result (nconc result (copy-list code))))
;;                    (spec-simple (spec format)
;;                      (if (atom spec)
;;                          (error "SPECIFICATEUR DE FORMAT INVALIDE ~S" format)
;;                          (case (first spec)
;;                            (:rep-1   (collect (gen !pushi 1 nil)))
;;                            (:rep     (collect (gen !pushi (numero-valeur (second spec)) nil)))
;;                            (:rep-var (when (null exprs)
;;                                        (error "IL MANQUE AU MOINS UNE EXPRESSION POUR LE SPECIFICATEUR DE FORMAT ~S" format))
;;                                      (collect (generate-statement (pop exprs) nil)))
;;                            (otherwise
;;                             (error "SPECIFICATEUR DE FORMAT INVALIDE ~S" format)))))
;;                    (spec-expr (spec op format)
;;                      (if (atom spec)
;;                          (error "SPECIFICATEUR DE FORMAT INVALIDE ~S" format)
;;                          (case (first spec)
;;                            (:rep-1 (when (null exprs)
;;                                      (error "IL MANQUE AU MOINS UNE EXPRESSION POUR LE SPECIFICATEUR DE FORMAT ~S" format))
;;                                    (collect (generate-statement (pop exprs) nil))
;;                                    (collect op))
;;                            (:rep   (loop
;;                                      :repeat (numero-valeur (second spec))
;;                                      :do (progn
;;                                            (when (null exprs)
;;                                              (error "IL MANQUE AU MOINS UNE EXPRESSION POUR LE SPECIFICATEUR DE FORMAT ~S" format))
;;                                            (collect (generate-statement (pop exprs) nil))
;;                                            (collect op))))
;;                            (otherwise
;;                             (error "SPECIFICATEUR DE FORMAT INVALIDE ~S" format))))))
;;             (if (second stat)
;;                 (loop
;;                   :for format :in (second stat)
;;                   :do (ecase (first format)
;;                         (:spec-chaine
;;                          ;; (print stat)
;;                           ;; (print format)
;;                          (spec-simple (second format) format)
;;                          (collect (gen (third format) !afficher-chaine nil)))
;;                         ((:spec-slash :spec-space :spec-cr :spec-nl)
;;                          (spec-simple (second format) format)
;;                          (collect (gen (case (first format)
;;                                          (:spec-slash !afficher-newline)
;;                                          (:spec-space !afficher-space)
;;                                          (:spec-cr    !afficher-cr)
;;                                          (:spec-nl    !afficher-nl))
;;                                        nil)))
;;                         ((:spec-f :spec-e)
;;                          (spec-expr (second format) (gen (third format)
;;                                                          (fourth format)
;;                                                          (case (first format)
;;                                                            (:spec-f !afficher-f)
;;                                                            (:spec-e !afficher-e))
;;                                                          nil) format))
;;                         (:spec-u
;;                          (spec-expr (second format) (gen !afficher-u nil) format)))
;;                   :finally (loop
;;                              :for expr :in exprs
;;                              :do (collect (gen (generate-statement expr nil) !afficher-u nil))))
;;                 ;; (5) afficher[{n}u]expr...
;;                 (progn
;;                   (collect (gen !pushi 1 !afficher-newline nil))
;;                   (loop
;;                     :for expr :in (cddr stat)
;;                     :do (progn
;;                           (collect (generate-statement expr nil))
;;                           (collect (gen !afficher-u nil)))))))
;;           (nconc result next)))
;; 
;;        
;;        ;; (setf next (gen !pushi (length (cddr stat)) !pop&store 'id::$valscnt next))
;;        ;; (setf next (gen !pushi 1 !pop&store 'id::$index next))
;;        ;; (4) set $index to 1
;;        ;; to avoid too much stack usage, we evalute each expression
;;        ;; and store it into $vals in turn.
;;        ;; (3) pop one expression and store them in $vals[$index]
;;        ;; (2) push one expression 
;;        ;; (loop
;;        ;;    :for expr :in (reverse (cddr stat))
;;        ;;    :for i :from (length (cddr stat)) :downto 1 
;;        ;;    :do (setf next (gen  !pushi i !pop&astore1 'id::$vals next))
;;        ;;    :do (setf next (generate-statement expr next)))
;;        ;; (1) declare the tableau $vals[n]
;;        ;; (gen  !pushi (length (cddr stat)) !tableau1 'id::$vals next)
;; 
;; 
;;        ;; rep :affiche-u -- 
;;        ;; 
;;        ;; @loop: dup pushi 0 eg btrue @end
;;        ;; dup pushi 1 add swap pushi 1 swap
;;        ;; aref1&push $vals :afficher-u branch @loop
;;        ;; @end: pop
;; 
;; 
;; 
;;        (:aller-en (gen (second stat) !goto next))
;;        (:si
;;         ;;(:si test then)       --> test :bfalse offset.t then  
;;         ;;(:si test then else)  --> test :bfalse offset.t then  :balways offset.e else
;;         (if (cdddr stat)
;;             ;; same as :xi
;;             (let* ((else (gen (fourth stat) next))
;;                    (offset.e (cons-position next else))
;;                    (then (gen (third stat) !balways  offset.e else))
;;                    (offset.t (cons-position else then)))
;;               (gen (second stat) !bfalse offset.t then))
;;             (let* ((then (gen (third stat) next))
;;                    (offset.t (cons-position next then)))
;;               (gen (second stat) !bfalse offset.t then))))
;; 
;;        (:terminer (gen !terminer next))
;;        (:pause    (gen !pause    next))
;; 
;;        (:faire-jusqu-a ;; lino ident init pas jusqua)
;;         ;; --> lino init pas jusqua :faire-jusqu-a ident
;;         (gen (second stat) (fourth stat) (fifth stat) (sixth stat)
;;              !faire-jusqu-a (third stat) next))
;; 
;;        (:faire-tant-que ;; lino ident init pas test)
;;         ;; --> lino init pas :faire-tant-que ident test
;;         (gen (second stat) (fourth stat) (fifth stat) 
;;              !faire-tant-que (third stat)
;;              (sixth stat) !tant-que next))
;; 
;;        ;; ==> create a faire bloc. When we reach the end of each line, we must
;;        ;;     check for loop blocks available for this line. (kind of come from...).
;; 
;; 
;;        (:garer ;; var enr fic) --> enr fic :garer var
;;         (gen (third stat) (fourth stat) !garer (identificateur-nom (second stat)) next))
;;        
;;        (:charger
;;         ;;(:charger  var enr fic)         --> enr fic :charger var nil
;;         ;;(:charger  var enr fic varstat) --> enr fic :charger var varstat
;;         (if (cddddr stat)
;;             (gen (third stat) (fourth stat) !charger
;;                  (identificateur-nom (second stat))
;;                  (identificateur-nom (fifth stat))
;;                  next)
;;             (gen (third stat) (fourth stat) !charger
;;                  (identificateur-nom (second stat))
;;                  nil
;;                  next)))
;;        
;;        (:supprimer
;;         (if (third stat)
;;             (gen (second stat) (third stat) !supprimer-enregistrement next)
;;             (gen (second stat) !supprimer-fichier next)))
;;        
;;        (:executer
;;         (if (third stat)
;;             (gen (second stat) (third stat)  !executer next)
;;             (gen (second stat) !pushi 1      !executer next)))
;; 
;;        (:debut
;;         (loop
;;            :for i :in (reverse (cdr stat))
;;            :do (setf next (generate-statement i next))
;;            :finally (return next)))))))



(defstruct procedure
  name
  (parameters      '())
  (local-variables '())
  line
  (offset 1))

(defstruct code
  (line      0)
  (vector    (make-array 8 :adjustable t :fill-pointer 0))
  (procedure nil)
  (source    nil))

(defun code-offset (code)
  (fill-pointer (code-vector code)))

(defun gen-code (code code1 &rest codes)
  (dolist (item (cons code1 codes))
    (if (or (numberp item)
            (stringp item)
            (and (symbolp item)
                 (or (null item)
                     (eq (symbol-package item) (find-package "BC"))
                     (eq (symbol-package item) (find-package "ID")))))
        (vector-push-extend item
                            (code-vector code)
                            (length (code-vector code)))
        (error "Invalid code ~S" item))))



(defun generate-expression (code expression)
  (etypecase expression
    (null)
    ((or integer nombre symbol)
     (gen-code code expression))
    (token
     (ecase (token-kind expression)
       (tok-chaine         (gen-code code !pushi (chaine-valeur expression)))
       (tok-nombre         (gen-code code !pushi (nombre-valeur expression)))
       (tok-numero         (gen-code code !pushi (numero-valeur expression)))
       (tok-identificateur (gen-code code (identificateur-nom expression)))
       (tok-commentaire    (gen-code code !comment (token-text expression)))))
    (atom (error "Unknown expression ~S" expression))
    (cons
     (case (first expression)

       ((:neg :non)
        (generate-expression code (second expression))
        (gen-code code (case (first expression)
                                (:neg !neg)
                                (:non !non))))
       ((:moins :plus :fois :divise :puissance :concat
                :lt :le :gt :ge :eg :ne)
        (generate-expression code (second expression))
        (generate-expression code (third  expression))
        (gen-code code (case (first expression)
                                (:moins !sub)
                                (:plus  !add)
                                (:fois  !mul)
                                (:divise !div)
                                (:puissance !pow)
                                (:concat !concat)
                                (:lt !lt)
                                (:le !le)
                                (:gt !gt)
                                (:ge !ge)
                                (:eg !eg)
                                (:ne !ne))))
       (:xi
        (generate-expression code (second expression))
        (gen-code code !bfalse)
        (let ((index.t (code-offset code)))
          (gen-code code 0)
          (generate-expression code (third expression))
          (gen-code code !balways)
          (let ((index.e (code-offset code)))
            (gen-code code 0)
            (generate-expression code (fourth expression))
            (let ((index.f (code-offset code)))
              (setf (aref (code-vector code) index.t) (- index.e index.t)
                    (aref (code-vector code) index.e) (- index.f index.e 1))))))
       
       ((:ou :et)
        (generate-expression code (second expression))
        (loop
          :with operator = (case (first expression)
                             (:ou !ou)
                             (:et !et))
          :for item :in (cddr expression)
          :do (progn
                (generate-expression code item)
                (gen-code code operator))))
    
       ((:aval :aref)
        (if (null (cdddr expression))
            (progn
              (generate-expression code (third expression))
              (gen-code code
                        (case (first expression)
                          (:aval !aref1&push-val)
                          (:aref !aref1&push-ref))
                        (identificateur-nom (second expression))))
            (progn
              (generate-expression code (third  expression))
              (generate-expression code (fourth expression))
              (gen-code code 
                        (case (first expression)
                          (:aval !aref2&push-val)
                          (:aref !aref2&push-ref))
                        (identificateur-nom (second expression))))))
       ((:vval :vref)
        (gen-code code (case (first expression)
                                (:vval !push-val)
                                (:vref !push-ref))
                  (identificateur-nom (second expression))))

       ((:fonction)
        (loop
          :for item :in (cddr expression)
          :do (generate-expression code item)
          :finally (gen-code code !callf
                             (identificateur-nom (second expression))
                             (length (cddr expression)))))

       (t
        (error "Unknown expression ~S" expression)))))
  code)


(defun generate-statement (code statement)
  (etypecase statement
    (null)
    (atom (error "Unknown statement ~S" statement))
    (cons
     (case (first statement)

       (:commentaire
        (generate-expression code (second statement)))       

       ((:liberer :chaine)
        (loop
          :with operator = (case (first statement)
                             (:liberer !liberer)
                             (:chaine  !chaine))
          :for item :in (rest statement)
          :do (gen-code code operator (identificateur-nom item))))

       (:tableau
        (loop
          :for item :in (rest statement)
          :do (destructuring-bind (adecl ident index.1 &optional index.2) item
                (unless (eql :adecl adecl)
                  (error "Unknown statement ~S" (list :tableau item)))
                (generate-expression code index.1)
                (if index.2
                    (progn
                      (generate-expression code index.2)
                      (gen-code code !tableau2))
                    (gen-code code !tableau1))
                (gen-code code (identificateur-nom ident)))))

       ((:appel)
        (loop
          :for item :in (cddr statement)
          :do (generate-expression code item)
          :finally (gen-code code !callp
                             (identificateur-nom (second statement))
                             (length (cddr statement)))))

       
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

       
       ;; - Appel de  PROCEDURE
       ;; 
       ;;   We cannot generate vref or vval for parameters of PROCEDUREs
       ;;   before knowing what parameters are by reference and what are by
       ;;   value, therefore before having seen the PROCEDURE declaration in
       ;;   the source.
       ;; 
       ;;   ⚠ Therefore we can generate the code only globally, just before we
       ;;     run the program.  Chaining or loading parts of the program while
       ;;     running means we need to recompile it.
       

       ;; :result             OR  :retour-en      OR   :retour
       ;; -------------------     ---------------     ------------
       ;; result    <-- :pop  OR  goto <-- :pop   OR   nothing
       ;; 
       ;; return-pc <-- sf.return-pc
       ;;-sf   <-- sf.next-sf
       ;; argcnt    <-- sf.argcnt
       ;; sf        <---sf
       ;; :pop-sf
       ;; :pop-n argcnt
       ;;                
       ;; :push  result              --                 --
       ;; pc <-- return-pc       OR  pc <-- goto     or pc <-- return-pc


       (:decl-procedure
        ;; Les procedures ne sont pas executable, !procedure generates an error.
        (gen-code code !procedure)
        (destructuring-bind (decl name params locals) statement
          (declare (ignore decl))
          (let ((params (mapcar (function identificateur-nom)  params))
                (locals (mapcar (function identificateur-nom)  locals)))
           (setf (code-procedure code)
                 (make-procedure :name (identificateur-nom name)
                                 :parameters (nreverse
                                              (mapcar (lambda (param)   
                                                        (if (member param locals)
                                                            `(:par-valeur    ,param)
                                                            `(:par-reference ,param)))
                                                      params))
                                 :local-variables (set-difference locals params)
                                 :line (code-line code)
                                 :offset (code-offset code))))))
       
       (:resultat   (generate-expression code (second statement))
                    (gen-code code !result))
       (:retour-en  (generate-expression code (second statement))
                    (gen-code code !retour-en))
       (:retour     (gen-code code !retour))

       (:affectation
        ;; (:vref ident) expression) --> expression :pop&store ident
        ;; (:aref ident expr) expression) --> expression expr :pop&astore1 ident
        ;; (:aref ident expr.1 expr.2) expression) --> expression expr.1 expr.2 :pop&astore2 ident
        (generate-expression code (third statement))
        (ecase (length (second statement))
          (2  (gen-code code !pop&store))
          (3  (generate-expression code (third (second statement)))
              (gen-code code !pop&astore1))
          (4  (generate-expression code (third  (second statement)))
              (generate-expression code (fourth (second statement)))
              (gen-code code !pop&astore2)))
        (gen-code code (identificateur-nom (second (second statement)))))

       (:lire
        ;; (:vref ident) --> :lire&store ident
        ;; (:aref ident expr) -->  expr :lire&astore1 ident
        ;; (:aref ident expr.1 expr.2) -->  expr.1 expr.2 :lire&astore2 ident
        (gen-code code !beep) 
        (loop
          :for item :in (rest statement)
          :do (progn 
                (ecase (length item)
                  (2  (gen-code code !lire&store))
                  (3  (generate-expression code (third  item))
                      (gen-code code !lire&astore1))
                  (4  (generate-expression code (third  item))
                      (generate-expression code (fourth item))
                      (gen-code code !lire&astore2)))
                (gen-code code (identificateur-nom (second item))))))


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
        (let ((exprs (cddr statement)))
          (labels ((spec-simple (spec format)
                     (if (atom spec)
                         (error "SPECIFICATEUR DE FORMAT INVALIDE ~S" format)
                         (case (first spec)
                           (:rep-1   (gen-code code !pushi 1))
                           (:rep     (gen-code code !pushi (numero-valeur (second spec))))
                           (:rep-var (when (null exprs)
                                       (error "IL MANQUE AU MOINS UNE EXPRESSION POUR LE SPECIFICATEUR DE FORMAT ~S" format))
                                     (generate-expression code (pop exprs)))
                           (otherwise
                            (error "SPECIFICATEUR DE FORMAT INVALIDE ~S" format)))))
                   (spec-expr (spec ops format)
                     (if (atom spec)
                         (error "SPECIFICATEUR DE FORMAT INVALIDE ~S" format)
                         (case (first spec)
                           (:rep-1 (when (null exprs)
                                     (error "IL MANQUE AU MOINS UNE EXPRESSION POUR LE SPECIFICATEUR DE FORMAT ~S" format))
                                   (generate-expression code (pop exprs))
                                   (dolist (op ops) (gen-code code op)))
                           (:rep   (loop
                                     :repeat (numero-valeur (second spec))
                                     :do (progn
                                           (when (null exprs)
                                             (error "IL MANQUE AU MOINS UNE EXPRESSION POUR LE SPECIFICATEUR DE FORMAT ~S" format))
                                           (generate-expression code (pop exprs))
                                           (dolist (op ops) (gen-code code op)))))
                           (otherwise
                            (error "SPECIFICATEUR DE FORMAT INVALIDE ~S" format))))))
            (if (second statement)
                (loop
                  :for format :in (second statement)
                  :do (ecase (first format)
                        (:spec-chaine
                         ;; (print statement)
                         ;; (print format)
                         (spec-simple (second format) format)
                         (generate-expression code (third format))
                         (gen-code code !afficher-chaine))
                        ((:spec-slash :spec-space :spec-cr :spec-nl)
                         (spec-simple (second format) format)
                         (gen-code code (case (first format)
                                          (:spec-slash !afficher-newline)
                                          (:spec-space !afficher-space)
                                          (:spec-cr    !afficher-cr)
                                          (:spec-nl    !afficher-nl))))
                        ((:spec-f :spec-e)
                         (spec-expr (second format)
                                    (list !pushi (numero-valeur (third format))
                                          !pushi (numero-valeur (fourth format))
                                          (case (first format)
                                            (:spec-f !afficher-f)
                                            (:spec-e !afficher-e)))
                                    format))
                        (:spec-u
                         (spec-expr (second format) '(!afficher-u) format)))
                  :finally (loop
                             :for expr :in exprs
                             :do (progn
                                   (generate-expression code expr)
                                   (gen-code code !afficher-u))))
                ;; (5) afficher[{n}u]expr...
                (progn
                  (gen-code code !pushi 1 !afficher-newline)
                  (loop
                    :for expr :in (cddr statement)
                    :do (progn
                          (generate-expression code expr)
                          (gen-code code !afficher-u))))))))

       

       (:aller-en (generate-expression code (second statement))
                  (gen-code code !goto))

       (:si
        ;;(:si test then)       --> test :bfalse offset.t then  
        ;;(:si test then else)  --> test :bfalse offset.t then  :balways offset.e else
        (generate-expression code (second statement))
        (gen-code code !bfalse)
        (let ((index.t (code-offset code)))
          (gen-code code 0)
          (generate-statement code (third statement))
          (if (fourth statement)
              (progn
                (gen-code code !balways)
                (let ((index.e (code-offset code)))
                  (gen-code code 0)
                  (generate-statement code (fourth statement))
                  (let ((index.f (code-offset code)))
                    (setf (aref (code-vector code) index.t) (- index.e index.t)
                          (aref (code-vector code) index.e) (- index.f index.e 1)))))
              (let ((index.e (code-offset code)))
                (setf (aref (code-vector code) index.t) (- index.e index.t 1))))))

       (:terminer (gen-code code !terminer))
       (:pause    (gen-code code !pause   ))

       (:faire-jusqu-a ;; lino ident init pas jusqua)
        ;; --> lino init pas jusqua :faire-jusqu-a ident
        (generate-expression code (second statement))
        (generate-expression code (fourth statement))
        (generate-expression code (fifth  statement))
        (generate-expression code (sixth  statement))
        (gen-code code !faire-jusqu-a (identificateur-nom (third statement))))

       (:faire-tant-que ;; lino ident init pas test)
        ;; --> lino init pas :faire-tant-que ident test
        (generate-expression code (second statement))
        (generate-expression code (fourth statement))
        (generate-expression code (fifth  statement))
        (gen-code code !faire-tant-que (identificateur-nom (third statement)))
        (generate-expression code (sixth  statement))
        (gen-code code !tant-que))

       ;; ==> create a faire bloc. When we reach the end of each line, we must
       ;;     check for loop blocks available for this line. (kind of come from...).


       (:garer ;; var enr fic) --> enr fic :garer var
        (generate-expression code (third statement))
        (generate-expression code (fourth statement))
        (gen-code code !garer (identificateur-nom (second statement))))
       
       (:charger
        ;;(:charger  var enr fic)         --> enr fic :charger var nil
        ;;(:charger  var enr fic varstat) --> enr fic :charger var varstat
        (generate-expression code (third statement))
        (generate-expression code (fourth statement))
        (if (fifth statement)
            (gen-code code !charger
                      (identificateur-nom (second statement))
                      (identificateur-nom (fifth statement)))
            (gen-code code !charger
                      (identificateur-nom (second statement))
                      nil)))
       
       (:supprimer
        (generate-expression code (second statement))
        (if (third statement)
            (progn
              (generate-expression code (third statement))
              (gen-code code !supprimer-enregistrement))
            (gen-code code !supprimer-fichier)))
       
       (:executer
        (generate-expression code (second statement))
        (if (third statement)
            (progn
              (generate-expression code (third statement))
              (gen-code code !executer))
            (gen-code code !pushi 1 !executer)))

       (:debut
        (compile-statement-list code (rest statement)))

       (t
         (error "Unknown statement ~S" statement)))))
  code)


(defun compile-statement-list (code slist)
  (dolist (statement slist code)
    (generate-statement code statement)))


(defun compile-lse-line-parse-tree (parse-tree)
  ;; (let ((*print-pretty* nil)) (terpri) (princ "Parse tree: ") (prin1 parse-tree) (terpri))
  (cond
    ((null parse-tree)
     (let ((code (make-code :line 0)))
       (gen-code code !stop)                    ; nothing to do
       code))
    ((atom parse-tree)
     (error 'lse-error
            :format-control "INTERNE: UN ATOME ~S DANS L'ARBRE SYNTAXIQUE."
            :format-arguments (list parse-tree)))
    ((eq (car parse-tree) :liste-instructions)
     (let ((code (make-code :line 0)))
       (compile-statement-list code (cdr parse-tree))
       (gen-code code !stop)
       code))
    ((eq (car parse-tree) :ligne-programme)
     (let ((linum (numero-valeur (cadr parse-tree))))
       (unless (line-number-valid-p linum)
         (lse-error "NUMERO DE LIGNE INVALIDE ~D; CE DEVRAIT ETRE UN ENTIER ENTRE 1 ET ~D INCLUS."
                    linum (line-number-maximum)))
       (let ((code (make-code :line linum)))
         (compile-statement-list code (cddr parse-tree))
         (gen-code code !next-line)
         code)))
    (t
     (error 'lse-error
            :format-control "INTERNE: UNE LISTE INVALIDE ~S DANS L'ARBRE SYNTAXIQUE."
            :format-arguments (list parse-tree)))))



(defmacro converting-parser-errors (&body body)
  ;; We convert parser and scanner errors into lse-parser and
  ;; lse-scanner errors, rewriting the error messages.
  `(handler-bind ((scanner-error-invalid-character
                   (lambda (err)
                     (error 'lse-scanner-error-invalid-character
                            :line              (scanner-error-line          err)
                            :column            (scanner-error-column        err)
                            :state             (scanner-error-state         err)
                            :current-token     (scanner-error-current-token err)
                            :scanner           (scanner-error-scanner       err)
                            :invalid-character (scanner-error-invalid-character err)
                            :format-control    "CARACTÈRE INVALIDE ~A~:[~*~; (~D)~]"
                            :format-arguments   (let ((ch (scanner-error-invalid-character err)))
                                                  (list
                                                   (if (<= 32 (char-code ch))
                                                       (format nil "'~A'" ch)
                                                       (format nil ".~A." (char-code ch)))
                                                   (<= 32 (char-code ch))
                                                   (char-code ch))))))
                  (parser-end-of-source-not-reached
                   (lambda (err)
                     (error 'lse-parser-error-end-of-source-not-reached
                            :line    (parser-error-line err)
                            :column  (parser-error-column err)
                            :grammar (parser-error-grammar err)
                            :scanner (parser-error-scanner err)
                            :non-terminal-stack (parser-error-non-terminal-stack err)
                            :format-control "SYMBOLE SURNUMÉRAIRE ~:[~A ~S~;~S~*~]; ATTENDU: FIN DE LA LIGNE."
                            :format-arguments
                            (let ((token (scanner-current-token
                                          (parser-error-scanner err))))
                              (list (string= (token-kind-label (token-kind token))
                                             (token-text token))
                                    (token-kind-label (token-kind token))
                                    (token-text token))))))
                  (parser-error-unexpected-token
                   (lambda (err)
                     (error 'lse-parser-error-unexpected-token
                            :line    (parser-error-line err)
                            :column  (parser-error-column err)
                            :grammar (parser-error-grammar err)
                            :scanner (parser-error-scanner err)
                            :non-terminal-stack (parser-error-non-terminal-stack err)
                            :expected-token (parser-error-expected-token err)
                            :format-control "~:[FIN DE LIGNE INATTENDU~3*~;SYMBOLE INATTENDU ~:[~A ~S~;~*~S~]~]~@[; ATTENDU: ~A~]."
                            :format-arguments
                            (let ((token (scanner-current-token
                                          (parser-error-scanner err))))
                              (list (not (or (eolp token) (eofp token)))
                                    (string= (token-kind-label (token-kind token))
                                             (token-text token))
                                    (token-kind-label (token-kind token))
                                    (token-text token)
                                    (token-kind-label (parser-error-expected-token err)))))))
                  (parser-error
                    (lambda (err)
                     (error 'lse-parser-error
                            :line    (parser-error-line err)
                            :column  (parser-error-column err)
                            :grammar (parser-error-grammar err)
                            :scanner (parser-error-scanner err)
                            :non-terminal-stack (parser-error-non-terminal-stack err)
                            :format-control   (parser-error-format-control   err)
                            :format-arguments (parser-error-format-arguments err)))))
     ,@body))


(defun lse-parser (*scanner*)
  (converting-parser-errors
   (parse-lse *scanner*)))



(defun compile-lse-line (source-line)
  (let* ((*scanner*  (make-instance 'lse-scanner :source source-line))
         (parse-tree (lse-parser *scanner*))
         (code       (compile-lse-line-parse-tree parse-tree)))
    (setf (code-source code)  (unparse-slist parse-tree))
    code))



(defun compile-lse-stream (stream)
  (let ((*scanner* (make-instance 'lse-scanner :source stream)))
    (loop
      :until (typep (scanner-current-token *scanner*) 'tok-eof)
      :for parse-tree = (lse-parser *scanner*)
      :when parse-tree
      :collect (let ((code (compile-lse-line-parse-tree parse-tree)))
                 (setf (code-source code)  (unparse-slist parse-tree))
                 code))))


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



(defun decompile-lse-line (lino code)
  (declare (ignore lino))
  ;; For now, we cheat, we don't decompile anything, we just return
  ;; the unparsed source kept along with the code vector.
  (code-source code))


(defun decompile-lse-line-2 (lino byte-code)
  (with-output-to-string (source)
    (princ lino source) (princ " " source)
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
        (princ listing source)
        #-(and)
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
    #-(and)
    (loop

      (case

          
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
                               CHARGER                  ; enr fic CHARGER datavar statusvar
                               SUPPRIMER-ENREGISTREMENT ; enr fic SUPPRIMER-ENREGISTREMENT
                               SUPPRIMER-FICHIER        ;     fic SUPPRIMER-FICHIER
                               EXECUTER  ; fic lin EXECUTED
                               PAUSE     ; PAUSE
                               TERMINER  ; TERMINER

                               PROCEDURE
                               comment ; COMMENT comment
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
                               ))

        (cl:defparameter *2* '(CALL            ; CALL identificateur-procedure nombre-d-argument
                               ))))))




;;;---------------------------------------------------------------------
;;; 3- Disassembler
;;;---------------------------------------------------------------------


(defparameter *cop-info*
  (let ((table (make-hash-table)))
    (dolist (c bc::*0*) (setf (gethash (symbol-value c) table) (cons c 0)))
    (dolist (c bc::*1*) (setf (gethash (symbol-value c) table) (cons c 1)))
    (dolist (c bc::*2*) (setf (gethash (symbol-value c) table) (cons c 2)))
    table)
  "Maps the code operation to a (cons symbol number-of-parameters).")


;; (with-output-to-string (*standard-output*)
;;   (disassemble-lse (vm-code (task-vm *task*))
;;                   :start (- (vm-pc.offset (task-vm *task*)) 10)
;;                   :end (+ (vm-pc.offset (task-vm *task*)) 10)
;;                   :one-instruction nil))



(defun disassemble-lse (byte-code &key (start 0) (end nil) (one-instruction nil))
  (let ((end (or end (length byte-code))))
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
          :do (if (and (<= start pc) (< pc end))
                  (progn (format t "~3D: " pc)
                         (if (member pc labels)
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
                         (incf pc (length line))
                         (when one-instruction (loop-finish)))
                  (incf pc (length line)))))))
  (values))




;;;---------------------------------------------------------------------
;;; Test functions
;;;---------------------------------------------------------------------

(defun test/parse-stream (src)
  (let ((*scanner* (make-instance 'lse-scanner :source src)))
   (loop
     :until (typep (scanner-current-token *scanner*) 'tok-eof)
     :collect  (lse-parser *scanner*))))


(defun test/parse-file (path)
  (with-open-file (src path)
    (test/parse-stream src)))

(defun test/parse-string (source)
  (with-input-from-string (src source)
    (test/parse-stream src)))


(defun test/compile-lse-stream (src)
  (loop
    :for line = (read-line src nil nil)
    :while line
    :do (terpri) (princ ";; |  ") (write-string line)
    :do (let ((comp (compile-lse-line line)))
          (print comp)
          (print (code-source comp))
          (print (disassemble-lse (code-vector comp))))
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


;; (test/parse-file #P"SYNTERR.LSE")
;; (test/parse-file #P"../BOURG/BOUR.LSE")
;; (test/parse-string "18*")
;; (let ((*print-escape* nil) (*print-pretty* t) (*print-right-margin* 80)) (write (test/parse-file "tpars.lse")))

;; (compile-lse-line "95 AFFICHER['Après la pause…',/]")
;; (compile-lse-line "20 afficher [10/,20x,'Hello',/,20x,5'*',2/]")


;; (test/compile-lse-line "4 executer 'tfic'")
;; (4 #(50 "tfic" 50 1 35 26) "4 EXECUTER 'tfic'")

;; (test/parse-string "6 TABLEAU V[3],M[2,2]")
;; (test/compile-lse-string "6 TABLEAU V[3],M[2,2]")








;; (compile-lse-file #P "../BOURG/BOUR.LSE")
;; (compile-lse-file #P "TESTCOMP.LSE")

;; (test/compile-lse-file #P "TESTCOMP.LSE")
;; (test/compile-lse-file "tpars.lse")



;; Local Variables:
;; (cl-indent 'dolist/separator 1)
;; End:

;;;; THE END ;;;;
