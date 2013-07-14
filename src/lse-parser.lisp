;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;*****************************************************************************
;;;;FILE:               lse-parser.lisp
;;;;LANGUAGE:           ANSI-C
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     POSIX
;;;;DESCRIPTION
;;;;    
;;;;    EMULSE : L.S.E. [ EMULATION MITRA-15 ]
;;;;    
;;;;    An emultator of the CII MITRA-15 L.S.E. System 
;;;;    and programming language interpreter.
;;;;    
;;;;    This file describes the syntax and grammar of the L.S.E. language.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon
;;;;MODIFICATIONS
;;;;    2012-02-07 <PJB> Converted to rdp (zebu uses too much files for the grammars).
;;;;    2005-08-21 <PJB> Completed conversion to zebu.
;;;;    2003-11-13 <PJB> Converted from bison to zebu.
;;;;    2000-12-09 <PJB> Added this header comment.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2000 - 2013
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
;;;;*****************************************************************************

(in-package "COM.INFORMATIMAGO.LSE")


(defun snoc (list item)
  "nconc the ITEM to the LIST."
  (nconc list (cons item nil)))

(defun uncomb (expr)
  (cond
    ((atom expr) expr)
    ((endp (cdr expr)) (car expr))
    (t (uncomb (cons (list* (caadr expr) (car expr) (cdadr expr)) (cddr expr))))))


;; (defgrammar test
;;     :terminals ((ident "[A-Za-z][A-Za-z0-9]+")
;;                 (numb "[0-9]+"))
;;     :start start
;;     :rules ((--> start
;;                  (alt (seq ident numb  :action (list :in $1 $2))
;;                       (seq ident ident :action (list :ii $1 $2))))))
;; (parse-test "hello world") --> expected numb.
;; 
;; 
;; (defgrammar test
;;     :terminals ((ident "[A-Za-z][A-Za-z0-9]+")
;;                 (numb "[0-9]+"))
;;     :start start
;;     :rules ((--> start
;;                  (seq ident (alt (seq numb  :action (list :in $1))
;;                                  (seq ident :action (list :ii $1)))
;;                       :action (list (first $2) $1 (second $2)))
;;                  :action $1)))
;; 
;; (parse-test "hello world") --> (:II (IDENT "hello" 0) (IDENT "world" 6))


(defgrammar lse
    :scanner nil ; so that we can pass the scanner ourselves.
    :trace nil
    :terminals (
                ;; keywords
                (tok-AFFICHER  "AFFICHER")
                (tok-ALLER     "ALLER")
                (tok-ALORS     "ALORS")
                (tok-CHAINE    "CHAINE")
                (tok-CHARGER   "CHARGER")
                (tok-DEBUT     "DEBUT")
                (tok-EN        "EN")
                (tok-ET        "ET")
                (tok-EXECUTER  "EXECUTER")
                (tok-FIN       "FIN")
                (tok-FAIRE     "FAIRE")
                (tok-GARER     "GARER")
                (tok-JUSQUA    "JUSQUA")
                (tok-LIBERER   "LIBERER")
                (tok-LIRE      "LIRE")
                (tok-LOCAL     "LOCAL")
                (tok-NON       "NON")
                (tok-OU        "OU")
                (tok-PAS       "PAS")
                (tok-PAUSE     "PAUSE")
                (tok-POUR      "POUR")
                (tok-PROCEDURE "PROCEDURE")
                (tok-QUE       "QUE")
                (tok-RESULTAT  "RESULTAT")
                (tok-RETOUR    "RETOUR")
                (tok-SI        "SI")
                (tok-SINON     "SINON")
                (tok-SUPPRIMER "SUPPRIMER")
                (tok-TABLEAU   "TABLEAU")
                (tok-TANT      "TANT")
                (tok-TERMINER  "TERMINER")
                ;; keyword or identifier:            
                (tok-x         "X")
                (tok-C         "C")
                (tok-L         "L")
                (tok-f         "F")
                (tok-e         "E")
                (tok-u         "U")
                ;; caractères spéciaux
                (tok-GT        ">")
                (tok-GE        ">=")
                (tok-LT        "<")
                (tok-LE        "<=")
                (tok-NE        "#")
                (tok-EQ        "=")
                (tok-affic     "\\?")
                (tok-plus      "\\+")
                (tok-moins     "-")
                (tok-concat    "!")
                (tok-fois      "\\*")
                (tok-divise    "/")
                (tok-puissance "\\^")
                (tok-virgule   ",")
                (tok-point     "\\.")
                (tok-pargauche "\\(")
                (tok-pardroite "\\)")
                (tok-crogauche "\\[")
                (tok-crodroite "\\]")
                (tok-fleche    "_")
                (tok-ptvirg    ";")
                (tok-at        "@")

                ;; (tok-ligne          "{NEWLINE}") ;; #.(string #\newline))
                ;; numero : chiffre {chiffre} .
                ;; nombre : chiffre {chiffre} [ '.' {chiffre} ]
                ;;                            [ 'E' ['+'|'-'] chiffre {chiffre} ] .
                ;; chaine : ' { caractere_sauf_quote | '' } ' .
                ;; ident : [ '&' ] lettre { lettre | chiffre } .
                ;; commentaire    : '*' { car } tok_EOLN .
                
                ;; nombre must come first to match the longest first.
                (tok-nombre         "[-+]?[0-9]+\\.[0-9]+[Ee][-+]?[0-9]+?")
                (tok-numero         "[0-9]+") 
                ;; "[0-9]+\\(\\.[0-9]*\\)?\\(E[-+]?[0-9]+\\)?")
                (tok-litchaine      "'(('')?[^']*)*'")
                ;; "\\('[^']*'\\)\\('[^']*'\\)*")
                ;; "\\'[^']*\\'\\(\\'[^']*\\'\\)*")
                ;; "\\'\\([^']*\\|\\(\\'\\'\\)\\)*\\'")
                (tok-procident      "&[A-Z][0-9A-Z]*")
                (tok-identificateur "[A-Z][0-9A-Z]*")
                (tok-commentaire    "\\*.*$")
                ;; (tok-erreur         "{ERREUR}")
                )

    ;; The LSE language is line based.  We parse sources line by line.
    ;; Program files (tapes) can contain both lines and commands.
    ;; The LSE compiler worked on punch cards, line by line too.
 
    :start debut
    :rules (

            (--> debut
                 (alt
                  (seq ligne-programme    :action $1)
                  (seq liste-inst-ou-decl :action (cons :liste-instructions $1))
                  (seq affic              :action (list :liste-instructions $1))
                  (seq                    :action (progn nil)))
                 :action $1)

            (--> ligne-programme
                 (seq (seq tok-numero :action (progn (setf (scanner-line *scanner*) (numero-valeur tok-numero))
                                                     tok-numero))
                      (alt liste-inst-ou-decl
                           (seq decl-procedure (opt (seq tok-ptvirg liste-inst-ou-decl :action $2)
                                                    :action $1) :action (cons $1 $2)))
                      :action (list* :ligne-programme $1 $2))
                 :action $1)

            (--> decl-procedure
                 (seq tok-PROCEDURE procident tok-pargauche
                      (alt
                       (seq tok-pardroite                      (opt decl-local :action decl-local)  :action (list nil $2))
                       (seq liste-identificateur tok-pardroite (opt decl-local :action decl-local)  :action (list $1 $3)))
                      :action (list* :decl-procedure $2 $4))
                 :action $1)

            (--> decl-local
                 (seq tok-LOCAL liste-identificateur :action $2)
                 :action $1)

            (--> liste-inst-ou-decl
                 (alt
                  (seq instruction (opt tok-ptvirg liste-inst-ou-decl :action liste-inst-ou-decl) :action (cons $1 $2))
                  (seq decl        (opt tok-ptvirg liste-inst-ou-decl :action liste-inst-ou-decl) :action (cons $1 $2))
                  (seq tok-commentaire                                :action (list (list :commentaire $1))))
                 :action $1)

            (--> decl
                 (alt decl-chaine decl-tableau)
                 :action $1)
            
            (--> decl-chaine
                 (seq tok-CHAINE liste-identificateur  :action (cons :chaine $2))
                 :action $1)

            (--> decl-tableau
                 (seq tok-TABLEAU liste-decl-tabl      :action (cons :tableau $2))
                 :action $1)

            (--> liste-decl-tabl
                 (seq decl-tabl (rep tok-virgule decl-tabl :action $2) :action (cons $1 $2))
                 :action $1)

            (--> decl-tabl
                 (seq identificateur tok-crogauche expression (opt (seq tok-virgule expression :action expression)) tok-crodroite
                      :action (list* :adecl $1 expression $4))
                 :action $1)

            
            (--> instruction
                 (alt
                  liberation      
                  affectation     
                  appel           
                  lire            
                  afficher        
                  aller-en        
                  si-alors-sinon  
                  terminer        
                  pause           
                  debut-fin       
                  faire
                  retour          
                  resultat        
                  garer           
                  charger         
                  supprimer       
                  executer)
                 :action $1)


            (--> liberation
                 (seq tok-LIBERER liste-identificateur :action (cons :liberer $2))
                 :action $1)

            (--> affectation
                 (seq reference tok-fleche expression :action (list :affectation reference expression))
                 :action $1)

            (--> appel
                 (seq procident tok-pargauche (opt liste-argument :action $1) tok-pardroite
                      :action (list* :appel $1 $3))
                 :action $1)

            (--> lire
                 (seq tok-LIRE liste-reference :action (cons :lire $2))
                 :action $1)

            (--> affic    ; ? […] …
                 (seq tok-affic (alt
                                 (seq format (opt liste-expression :action $1) :action (list* :afficher $1 $2))
                                 (seq             liste-expression  :action (list* :afficher nil $1)))
                      :action $2)
                 :action $1)

            (--> afficher ; AFFICHER […] …
                 (seq tok-AFFICHER (alt
                                    (seq format (opt liste-expression :action $1) :action (list* :afficher $1 $2))
                                    (seq             liste-expression  :action (list* :afficher nil $1)))
                      :action $2)
                 :action $1)
            
            (--> format
                 (seq tok-crogauche liste-spec tok-crodroite :action $2)
                 :action $1)

            (--> liste-spec
                 (seq specification (rep tok-virgule specification :action $2) :action (cons $1 $2))
                 :action $1)

            (--> specification
                 (alt
                  ;; Afin d'éviter une ambiguité sur first(spec-rep-num) inter first(spec-rep),
                  ;; on distingue les deux cas:
                  ;; 1) sans facteur de répétition:
                  (seq tok-litchaine :action (list :spec-chaine '(:rep-1) $1))
                  (seq tok-divise    :action (list :spec-slash  '(:rep-1)))
                  (seq tok-X         :action (list :spec-space  '(:rep-1)))
                  (seq tok-C         :action (list :spec-cr     '(:rep-1)))
                  (seq tok-L         :action (list :spec-nl     '(:rep-1)))
                  (seq tok-U         :action (list :spec-u      '(:rep-1)))
                  (seq tok-F tok-numero tok-point tok-numero :action (list :spec-f '(:rep-1) $2 $4))
                  (seq tok-E tok-numero tok-point tok-numero :action (list :spec-e '(:rep-1) $2 $4))
                  ;; 2) avec facteur de répétition:
                  (seq spec-rep-fois (alt
                                      (seq tok-litchaine :action (list :spec-chaine $1))
                                      (seq tok-divise    :action :spec-slash)
                                      (seq tok-X         :action :spec-space)
                                      (seq tok-C         :action :spec-cr)
                                      (seq tok-L         :action :spec-nl))
                       :action  (if (listp $2)
                                    (list* (first $2) spec-rep-fois (rest $2))
                                    (list $2 spec-rep-fois)))
                  (seq spec-rep-num  (alt
                                      (seq tok-litchaine :action (list :spec-chaine $1))
                                      (seq tok-divise    :action :spec-slash)
                                      (seq tok-X         :action :spec-space)
                                      (seq tok-C         :action :spec-cr)
                                      (seq tok-L         :action :spec-nl)
                                      (seq tok-U         :action :spec-u)
                                      (seq tok-F tok-numero tok-point tok-numero :action (list :spec-f tok-numero.1 tok-numero.2))
                                      (seq tok-E tok-numero tok-point tok-numero :action (list :spec-e tok-numero.1 tok-numero.2)))
                       :action (if (listp $2)
                                   (list* (first $2) spec-rep-num (rest $2))
                                   (list $2 spec-rep-num))))
                 :action $1)

            (--> spec-rep-num
                 (seq tok-numero  :action (list :rep $1))
                 :action $1)

            (--> spec-rep-fois
                 (seq tok-fois    :action (list :rep-var))
                 :action $1)

            (--> aller-en
                 (seq tok-ALLER tok-EN expression :action (list :aller-en $3))
                 :action $1)

            (--> si-alors-sinon
                 (seq tok-SI disjonction tok-ALORS instruction (opt (seq tok-SINON instruction :action $2) :action $1)
                      :action (if $5
                                  (list :si $2 $4 $5)
                                  (list :si $2 $4)))
                 :action $1)
            
            (--> terminer
                 tok-TERMINER :action (list :terminer))
            
            (--> pause
                 tok-PAUSE    :action (list :pause))
            
            (--> debut-fin
                 (seq tok-DEBUT liste-inst-ou-decl tok-FIN :action (cons :debut $2))
                 :action $1)

            (--> faire
                 (seq tok-FAIRE expression tok-POUR identificateur tok-fleche expression
                      (opt (seq tok-PAS expression :action expression) :action $1)
                      (alt (seq tok-JUSQUA expression         :action (list :faire-jusqu-a  $2))
                           (seq tok-TANT tok-QUE disjonction  :action (list :faire-tant-que $3)))
                      :action (list (first $8) $2 $4 $6
                                    (or $7 (make-instance 'tok-numero
                                               :kind 'tok-numero
                                               :text "1"
                                               :column (token-column $6)
                                               :line (token-line $6)))
                                    (second $8)))
                 :action $1)
            
            (--> retour
                 (alt
                  (seq tok-RETOUR (opt (seq tok-EN expression :action expression) :action $1)
                       :action (if $2
                                   (list :retour-en $2)
                                   (list :retour))))
                 :action $1)

            (--> resultat
                 (seq tok-RESULTAT expression       :action (list :resultat expression))
                 :action $1)

            (--> garer
                 (seq tok-GARER identificateur tok-virgule expression tok-virgule expression
                      :action (list :garer $2 $4 $6))
                 :action $1)

            (--> charger
                 (seq tok-CHARGER identificateur tok-virgule expression tok-virgule expression
                      (opt (seq tok-virgule identificateur :action $2) :action $1)
                      :action (list :charger $2 $4 $6 $7))
                 :action $1)

            (--> supprimer
                 (seq tok-SUPPRIMER expression
                      (opt (seq tok-virgule expression :action expression))
                      :action  (list* :supprimer expression $3))
                 :action $1)

            (--> executer
                 (seq tok-EXECUTER expression (opt (seq tok-virgule expression :action expression))
                      :action  (list* :executer $2 $3))
                 :action $1)

            
            ;; We cannot distinguish references from
            ;; right-references in function arguments because they
            ;; have the same form, and both are possible. &F(A,B), so
            ;; we must keep references, and also for other expressions.
            
            (--> expression
                 (seq terme-signe (rep (alt (seq tok-moins  terme :action (list :moins  terme))
                                            (seq tok-plus   terme :action (list :plus   terme))
                                            (seq tok-concat terme :action (list :concat terme)))
                                       :action $1)
                      :action (if $2
                                  (uncomb (cons terme-signe $2))
                                  terme-signe))
                 :action $1)

            ;; Two signs cannot be adjacent: a*-b must be written a*(-b)

            (--> terme-signe
                 (seq (opt tok-moins) terme
                      :action (if $1
                                  (list :neg terme)
                                  terme))
                 :action $1)
            
            (--> terme
                 (seq facteur (rep (alt (seq tok-fois    facteur :action (list :fois   facteur))
                                        (seq tok-divise  facteur :action (list :divise facteur)))
                                   :action $1)
                      :action (if $2
                                  (uncomb (cons facteur $2))
                                  facteur))
                 :action $1)

            
            (--> facteur
                 (seq simple (rep (seq tok-puissance simple :action (list :puissance simple))
                                  :action $1)
                      :action (if $2
                                  (uncomb (cons simple $2))
                                  simple))
                 :action $1)

            (--> simple
                 (alt
                  (seq tok-SI disjonction tok-ALORS expression tok-SINON expression           :action (list :xi disjonction expression.1 expression.2))
                  (seq procident (opt (seq tok-pargauche (opt liste-argument :action $1) tok-pardroite :action $2) :action $1)
                       :action (if $2
                                   (list* :fonction procident $2)
                                   (list :vval procident)))
                  ;; For the following rule, since disjonction and
                  ;; expression have elements in common in their
                  ;; first-sets, we must  use a disjonction here.
                  ;; Type checking will ensure that we only have an
                  ;; expression when we need an expression, and a
                  ;; condition when we need it.
                  (seq tok-pargauche disjonction tok-pardroite :action $2)
                  reference
                  at
                  tok-litchaine
                  tok-nombre
                  tok-numero)
                 :action $1)


            (--> liste-argument
                 (seq  expression (rep tok-virgule expression :action $2)
                       :action (cons $1 $2))
                 :action $1)

            (--> liste-expression
                 (seq expression (rep tok-virgule expression :action expression) :action (cons expression $2))
                 :action $1)

            (--> liste-identificateur
                 (seq identificateur (rep tok-virgule identificateur :action $2)
                      :action (cons $1 $2))
                 :action $1)

            (--> disjonction
                 (seq conjonction (rep tok-OU conjonction :action conjonction)
                      :action (if $2
                                  (list* :ou conjonction $2)
                                  conjonction))
                 :action $1)

            (--> conjonction
                 (seq condition (rep tok-ET condition :action condition)
                      :action (if $2
                                  (list* :et condition $2)
                                  condition))
                 :action $1)

            (--> condition
                 (alt
                  (seq tok-NON condition                :action (list :non condition))
                  ;; (seq tok-pargauche disjonction tok-pardroite  :action disjonction)
                  (seq expression (opt (seq (alt (seq tok-EQ :action :eg)
                                                 (seq tok-LT :action :lt)
                                                 (seq tok-LE :action :le)
                                                 (seq tok-NE :action :ne)
                                                 (seq tok-GT :action :gt)
                                                 (seq tok-GE :action :ge))
                                            expression :action (list $1 expression))
                                       :action $1) 
                       :action (if $2
                                   (list (first $2) expression (second $2))
                                   expression)))
                 :action $1)

            (--> reference
                 (seq identificateur (opt (alt (seq tok-crogauche expression
                                                    (opt (seq tok-virgule expression :action expression))
                                                    tok-crodroite
                                                    :action (list* :aref expression $3))
                                               (seq tok-pargauche
                                                    (opt liste-argument :action $1)
                                                    tok-pardroite
                                                    :action (list* :fonction $2)))
                                          :action $1)
                      :action (if $2
                                  (list* (first $2) $1 (rest $2))
                                  (list :vref $1)))
                 :action $1)

            (--> liste-reference
                 (seq reference (rep (seq tok-virgule reference :action $2) :action $1) :action (cons $1 $2))
                 :action $1)

            (--> identificateur
                 tok-identificateur
                 :action $1)

            (--> at
                 tok-at
                 :action (make-instance 'tok-identificateur
                             :kind 'tok-identificateur
                             :text "@"
                             :line (token-line $1)
                             :column  (token-column $1)))

            (--> procident
                 (seq tok-procident :action  $1)
                 :action $1)))


;;;; THE END ;;;;
