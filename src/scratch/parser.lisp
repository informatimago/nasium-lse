(in-package "COM.INFORMATIMAGO.LSE")
#-(and)
(let ((*print-circle* nil)
      (*print-right-margin* 120))
  (pprint (macroexpand-1 '
           
(defgrammar lse
  :scanner nil            ; so that we can pass the scanner ourselves.
  :trace nil
  :eof-symbol eol
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
              ;; commentaire    : '*' { car } tok-EOL .
                
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
               (opt
                (alt
                 (seq ligne-programme             :action $1)
                 (seq liste-inst-ou-decl-ou-affic :action (cons :liste-instructions $1)))
                :action $1)
               :action $1)

          (--> ligne-programme
               (seq (seq tok-numero :action (progn (setf (scanner-line *scanner*) (numero-valeur tok-numero))
                                                   tok-numero))
                    (alt liste-inst-ou-decl
                         (seq decl-procedure (opt (seq tok-ptvirg liste-inst-ou-decl :action $2)
                                                  :action $1)
                              :action (cons $1 $2)))
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

          (--> liste-inst-ou-decl-ou-affic
               (alt
                (seq affic       (opt tok-ptvirg liste-inst-ou-decl-ou-affic :action $2) :action (cons $1 $2))
                (seq instruction (opt tok-ptvirg liste-inst-ou-decl-ou-affic :action $2) :action (cons $1 $2))
                (seq decl        (opt tok-ptvirg liste-inst-ou-decl-ou-affic :action $2) :action (cons $1 $2))
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
               (seq identificateur tok-crogauche expression (opt tok-virgule expression :action expression) tok-crodroite
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

          (--> affic                    ; ? […] …
               (seq tok-affic (alt
                               (seq format (opt liste-expression :action $1) :action (list* :afficher $1 $2))
                               (seq             liste-expression  :action (list* :afficher nil $1)))
                    :action $2)
               :action $1)

          (--> afficher                 ; AFFICHER […] …
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
               (seq tok-SI disjonction tok-ALORS instruction (opt tok-SINON instruction :action instruction)
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
                    (opt tok-PAS expression :action expression)
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
                (seq tok-RETOUR (opt tok-EN expression :action expression)
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
                    (opt tok-virgule identificateur :action identificateur)
                    :action (list :charger $2 $4 $6 $7))
               :action $1)

          (--> supprimer
               (seq tok-SUPPRIMER expression
                    (opt tok-virgule expression :action expression)
                    :action  (list* :supprimer expression $3))
               :action $1)

          (--> executer
               (seq tok-EXECUTER expression (opt tok-virgule expression :action expression)
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
                (seq procident (opt tok-pargauche (opt liste-argument :action $1) tok-pardroite :action (list $2))
                     :action (if $2
                                 (list* :fonction procident (first $2))
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
                (seq expression (opt  (alt (seq tok-EQ :action :eg)
                                           (seq tok-LT :action :lt)
                                           (seq tok-LE :action :le)
                                           (seq tok-NE :action :ne)
                                           (seq tok-GT :action :gt)
                                           (seq tok-GE :action :ge))
                                      expression
                                      :action (list $1 expression)) 
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

)))



(let ((com.informatimago.rdp::*eof-symbol* 'eol))
  (register-grammar
   (com.informatimago.rdp::make-normalized-grammar
    :name
    'lse
    :terminals
    '((tok-afficher "AFFICHER") (tok-aller "ALLER") (tok-alors "ALORS") (tok-chaine "CHAINE") (tok-charger "CHARGER")
      (tok-debut "DEBUT") (tok-en "EN") (tok-et "ET") (tok-executer "EXECUTER") (tok-fin "FIN") (tok-faire "FAIRE")
      (tok-garer "GARER") (tok-jusqua "JUSQUA") (tok-liberer "LIBERER") (tok-lire "LIRE") (tok-local "LOCAL")
      (tok-non "NON") (tok-ou "OU") (tok-pas "PAS") (tok-pause "PAUSE") (tok-pour "POUR") (tok-procedure "PROCEDURE")
      (tok-que "QUE") (tok-resultat "RESULTAT") (tok-retour "RETOUR") (tok-si "SI") (tok-sinon "SINON")
      (tok-supprimer "SUPPRIMER") (tok-tableau "TABLEAU") (tok-tant "TANT") (tok-terminer "TERMINER") (tok-x "X")
      (tok-c "C") (tok-l "L") (tok-f "F") (tok-e "E") (tok-u "U") (tok-gt ">") (tok-ge ">=") (tok-lt "<")
      (tok-le "<=") (tok-ne "#") (tok-eq "=") (tok-affic "\\?") (tok-plus "\\+") (tok-moins "-") (tok-concat "!")
      (tok-fois "\\*") (tok-divise "/") (tok-puissance "\\^") (tok-virgule ",") (tok-point "\\.")
      (tok-pargauche "\\(") (tok-pardroite "\\)") (tok-crogauche "\\[") (tok-crodroite "\\]") (tok-fleche "_")
      (tok-ptvirg ";") (tok-at "@") (tok-nombre "[-+]?[0-9]+\\.[0-9]+[Ee][-+]?[0-9]+?") (tok-numero "[0-9]+")
      (tok-litchaine "'(('')?[^']*)*'") (tok-procident "&[A-Z][0-9A-Z]*") (tok-identificateur "[A-Z][0-9A-Z]*")
      (tok-commentaire "\\*.*$"))
    :start
    'debut
    :rules
    '((debut
       (seq
        ((opt
          ((seq
            ((alt
              ((seq (ligne-programme) ($1)) (seq (liste-inst-ou-decl-ou-affic) ((cons :liste-instructions $1))))))
            ($1)))))
        ($1)))
      (ligne-programme
       (seq
        ((seq
          ((seq (tok-numero) ((progn (setf (scanner-line *scanner*) (numero-valeur tok-numero)) tok-numero)))
           (alt
            (liste-inst-ou-decl
             (seq (decl-procedure (opt ((seq ((seq (tok-ptvirg liste-inst-ou-decl) ($2))) ($1))))) ((cons $1 $2))))))
          ((list* :ligne-programme $1 $2))))
        ($1)))
      (decl-procedure
       (seq
        ((seq
          (tok-procedure procident tok-pargauche
           (alt
            ((seq (tok-pardroite (opt ((seq (decl-local) (decl-local))))) ((list nil $2)))
             (seq (liste-identificateur tok-pardroite (opt ((seq (decl-local) (decl-local))))) ((list $1 $3))))))
          ((list* :decl-procedure $2 $4))))
        ($1)))
      (decl-local (seq ((seq (tok-local liste-identificateur) ($2))) ($1)))
      (liste-inst-ou-decl
       (seq
        ((alt
          ((seq (instruction (opt ((seq (tok-ptvirg liste-inst-ou-decl) (liste-inst-ou-decl))))) ((cons $1 $2)))
           (seq (decl (opt ((seq (tok-ptvirg liste-inst-ou-decl) (liste-inst-ou-decl))))) ((cons $1 $2)))
           (seq (tok-commentaire) ((list (list :commentaire $1)))))))
        ($1)))
      (liste-inst-ou-decl-ou-affic
       (seq
        ((alt
          ((seq (affic (opt ((seq (tok-ptvirg liste-inst-ou-decl-ou-affic) ($2))))) ((cons $1 $2)))
           (seq (instruction (opt ((seq (tok-ptvirg liste-inst-ou-decl-ou-affic) ($2))))) ((cons $1 $2)))
           (seq (decl (opt ((seq (tok-ptvirg liste-inst-ou-decl-ou-affic) ($2))))) ((cons $1 $2)))
           (seq (tok-commentaire) ((list (list :commentaire $1)))))))
        ($1)))
      (decl (seq ((alt (decl-chaine decl-tableau))) ($1)))
      (decl-chaine (seq ((seq (tok-chaine liste-identificateur) ((cons :chaine $2)))) ($1)))
      (decl-tableau (seq ((seq (tok-tableau liste-decl-tabl) ((cons :tableau $2)))) ($1)))
      (liste-decl-tabl (seq ((seq (decl-tabl (rep ((seq (tok-virgule decl-tabl) ($2))))) ((cons $1 $2)))) ($1)))
      (decl-tabl
       (seq
        ((seq
          (identificateur tok-crogauche expression (opt ((seq (tok-virgule expression) (expression)))) tok-crodroite)
          ((list* :adecl $1 expression $4))))
        ($1)))
      (instruction
       (seq
        ((alt
          (liberation affectation appel lire afficher aller-en si-alors-sinon terminer pause debut-fin faire retour
           resultat garer charger supprimer executer)))
        ($1)))
      (liberation (seq ((seq (tok-liberer liste-identificateur) ((cons :liberer $2)))) ($1)))
      (affectation (seq ((seq (reference tok-fleche expression) ((list :affectation reference expression)))) ($1)))
      (appel
       (seq
        ((seq (procident tok-pargauche (opt ((seq (liste-argument) ($1)))) tok-pardroite) ((list* :appel $1 $3))))
        ($1)))
      (lire (seq ((seq (tok-lire liste-reference) ((cons :lire $2)))) ($1)))
      (affic
       (seq
        ((seq
          (tok-affic
           (alt
            ((seq (format (opt ((seq (liste-expression) ($1))))) ((list* :afficher $1 $2)))
             (seq (liste-expression) ((list* :afficher nil $1))))))
          ($2)))
        ($1)))
      (afficher
       (seq
        ((seq
          (tok-afficher
           (alt
            ((seq (format (opt ((seq (liste-expression) ($1))))) ((list* :afficher $1 $2)))
             (seq (liste-expression) ((list* :afficher nil $1))))))
          ($2)))
        ($1)))
      (format (seq ((seq (tok-crogauche liste-spec tok-crodroite) ($2))) ($1)))
      (liste-spec (seq ((seq (specification (rep ((seq (tok-virgule specification) ($2))))) ((cons $1 $2)))) ($1)))
      (specification
       (seq
        ((alt
          ((seq (tok-litchaine) ((list :spec-chaine '(:rep-1) $1))) (seq (tok-divise) ((list :spec-slash '(:rep-1))))
           (seq (tok-x) ((list :spec-space '(:rep-1)))) (seq (tok-c) ((list :spec-cr '(:rep-1))))
           (seq (tok-l) ((list :spec-nl '(:rep-1)))) (seq (tok-u) ((list :spec-u '(:rep-1))))
           (seq (tok-f tok-numero tok-point tok-numero) ((list :spec-f '(:rep-1) $2 $4)))
           (seq (tok-e tok-numero tok-point tok-numero) ((list :spec-e '(:rep-1) $2 $4)))
           (seq
            (spec-rep-fois
             (alt
              ((seq (tok-litchaine) ((list :spec-chaine $1))) (seq (tok-divise) (:spec-slash))
                                                              (seq (tok-x) (:spec-space)) (seq (tok-c) (:spec-cr)) (seq (tok-l) (:spec-nl)))))
            ((if (listp $2) (list* (first $2) spec-rep-fois (rest $2)) (list $2 spec-rep-fois))))
           (seq
            (spec-rep-num
             (alt
              ((seq (tok-litchaine) ((list :spec-chaine $1))) (seq (tok-divise) (:spec-slash))
                                                              (seq (tok-x) (:spec-space)) (seq (tok-c) (:spec-cr)) (seq (tok-l) (:spec-nl)) (seq (tok-u) (:spec-u))
                                                              (seq (tok-f tok-numero tok-point tok-numero) ((list :spec-f tok-numero.1 tok-numero.2)))
                                                              (seq (tok-e tok-numero tok-point tok-numero) ((list :spec-e tok-numero.1 tok-numero.2))))))
            ((if (listp $2) (list* (first $2) spec-rep-num (rest $2)) (list $2 spec-rep-num)))))))
        ($1)))
      (spec-rep-num (seq ((seq (tok-numero) ((list :rep $1)))) ($1)))
      (spec-rep-fois (seq ((seq (tok-fois) ((list :rep-var)))) ($1)))
      (aller-en (seq ((seq (tok-aller tok-en expression) ((list :aller-en $3)))) ($1)))
      (si-alors-sinon
       (seq
        ((seq (tok-si disjonction tok-alors instruction (opt ((seq (tok-sinon instruction) (instruction)))))
          ((if $5 (list :si $2 $4 $5) (list :si $2 $4)))))
        ($1)))
      (terminer (seq (tok-terminer) ((list :terminer)))) (pause (seq (tok-pause) ((list :pause))))
      (debut-fin (seq ((seq (tok-debut liste-inst-ou-decl tok-fin) ((cons :debut $2)))) ($1)))
      (faire (seq
              ((seq
                (tok-faire expression tok-pour identificateur tok-fleche expression
                 (opt ((seq (tok-pas expression) (expression))))
                 (alt
                  ((seq (tok-jusqua expression) ((list :faire-jusqu-a $2)))
                   (seq (tok-tant tok-que disjonction) ((list :faire-tant-que $3))))))
                ((list (first $8)
                       $2
                       $4
                       $6
                       (or $7
                           (make-instance
                            'tok-numero
                            :kind
                            'tok-numero
                            :text
                            "1"
                            :column
                            (token-column $6)
                            :line
                            (token-line $6)))
                       (second $8)))))
              ($1)))
      (retour (seq
               ((seq (tok-retour (opt ((seq (tok-en expression) (expression)))))
                 ((if $2 (list :retour-en $2) (list :retour)))))
               ($1)))
      (resultat (seq ((seq (tok-resultat expression) ((list :resultat expression)))) ($1)))
      (garer (seq
              ((seq (tok-garer identificateur tok-virgule expression tok-virgule expression)
                ((list :garer $2 $4 $6))))
              ($1)))
      (charger (seq
                ((seq
                  (tok-charger identificateur tok-virgule expression tok-virgule expression
                   (opt ((seq (tok-virgule identificateur) (identificateur)))))
                  ((list :charger $2 $4 $6 $7))))
                ($1)))
      (supprimer (seq
                  ((seq (tok-supprimer expression (opt ((seq (tok-virgule expression) (expression)))))
                    ((list* :supprimer expression $3))))
                  ($1)))
      (executer (seq
                 ((seq (tok-executer expression (opt ((seq (tok-virgule expression) (expression)))))
                   ((list* :executer $2 $3))))
                 ($1)))
      (expression
       (seq
        ((seq
          (terme-signe
           (rep
            ((seq
              ((alt
                ((seq (tok-moins terme) ((list :moins terme))) (seq (tok-plus terme) ((list :plus terme)))
                                                               (seq (tok-concat terme) ((list :concat terme))))))
              ($1)))))
          ((if $2 (uncomb (cons terme-signe $2)) terme-signe))))
        ($1)))
      (terme-signe (seq ((seq ((opt ((seq (tok-moins) ($0)))) terme) ((if $1 (list :neg terme) terme)))) ($1)))
      (terme
       (seq
        ((seq
          (facteur
           (rep
            ((seq
              ((alt
                ((seq (tok-fois facteur) ((list :fois facteur)))
                 (seq (tok-divise facteur) ((list :divise facteur))))))
              ($1)))))
          ((if $2 (uncomb (cons facteur $2)) facteur))))
        ($1)))
      (facteur
       (seq
        ((seq (simple (rep ((seq ((seq (tok-puissance simple) ((list :puissance simple)))) ($1)))))
          ((if $2 (uncomb (cons simple $2)) simple))))
        ($1)))
      (simple
       (seq
        ((alt
          ((seq (tok-si disjonction tok-alors expression tok-sinon expression)
                ((list :xi disjonction expression.1 expression.2)))
           (seq
            (procident (opt ((seq (tok-pargauche (opt ((seq (liste-argument) ($1)))) tok-pardroite) ((list $2))))))
            ((if $2 (list* :fonction procident (first $2)) (list :vval procident))))
           (seq (tok-pargauche disjonction tok-pardroite) ($2)) reference at tok-litchaine tok-nombre tok-numero)))
        ($1)))
      (liste-argument (seq ((seq (expression (rep ((seq (tok-virgule expression) ($2))))) ((cons $1 $2)))) ($1)))
      (liste-expression
       (seq ((seq (expression (rep ((seq (tok-virgule expression) (expression))))) ((cons expression $2)))) ($1)))
      (liste-identificateur
       (seq ((seq (identificateur (rep ((seq (tok-virgule identificateur) ($2))))) ((cons $1 $2)))) ($1)))
      (disjonction
       (seq
        ((seq (conjonction (rep ((seq (tok-ou conjonction) (conjonction)))))
          ((if $2 (list* :ou conjonction $2) conjonction))))
        ($1)))
      (conjonction
       (seq
        ((seq (condition (rep ((seq (tok-et condition) (condition))))) ((if $2 (list* :et condition $2) condition))))
        ($1)))
      (condition
       (seq
        ((alt
          ((seq (tok-non condition) ((list :non condition)))
           (seq
            (expression
             (opt
              ((seq
                ((alt
                  ((seq (tok-eq) (:eg)) (seq (tok-lt) (:lt)) (seq (tok-le) (:le)) (seq (tok-ne) (:ne))
                                        (seq (tok-gt) (:gt)) (seq (tok-ge) (:ge))))
                 expression)
                ((list $1 expression))))))
            ((if $2 (list (first $2) expression (second $2)) expression))))))
        ($1)))
      (reference
       (seq
        ((seq
          (identificateur
           (opt
            ((seq
              ((alt
                ((seq
                  (tok-crogauche expression (opt ((seq ((seq (tok-virgule expression) (expression))) ($0))))
                                 tok-crodroite)
                  ((list* :aref expression $3)))
                 (seq (tok-pargauche (opt ((seq (liste-argument) ($1)))) tok-pardroite) ((list* :fonction $2))))))
              ($1)))))
          ((if $2 (list* (first $2) $1 (rest $2)) (list :vref $1)))))
        ($1)))
      (liste-reference
       (seq ((seq (reference (rep ((seq ((seq (tok-virgule reference) ($2))) ($1))))) ((cons $1 $2)))) ($1)))
      (identificateur (seq (tok-identificateur) ($1)))
      (at
       (seq (tok-at)
        ((make-instance
          'tok-identificateur
          :kind
          'tok-identificateur
          :text
          "@"
          :line
          (token-line $1)
          :column
          (token-column $1)))))
      (procident (seq ((seq (tok-procident) ($1))) ($1))))
    :scanner
    'nil
    :skip-spaces
    't))
  nil
  'nil
  (progn (fmakunbound 'lse/parse-reference)
         (defun lse/parse-reference (scanner)
           "(--> reference (seq (identificateur reference-1-1) ((if $2 (list* (first $2) $1 (rest $2)) (list :vref $1)))))"
           (com.informatimago.rdp::with-non-terminal
               (reference scanner)
             (cond ((word-equal (scanner-current-token scanner) 'tok-identificateur)
                    (let* (($1 (lse/parse-identificateur scanner))
                           (identificateur $1)
                           (identificateur.1 $1)
                           ($2 (lse/parse-reference-1-1 scanner))
                           ($0 (list $1 $2)))
                      (declare (ignorable $0 identificateur.1 identificateur $1 $2))
                      (if $2 (list* (first $2) $1 (rest $2)) (list :vref $1))))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-identificateur)
                     '(--> reference
                       (alt
                        (seq (identificateur reference-1-1)
                         ((if $2 (list* (first $2) $1 (rest $2)) (list :vref $1))))))))))))
  (progn (fmakunbound 'lse/parse-liste-reference-1-1)
         (defun lse/parse-liste-reference-1-1 (scanner)
           "(--> liste-reference-1-1 (alt ((seq nil ('nil)) (seq (liste-reference-1-1-1 liste-reference-1-1) ((cons (progn $1) $2))))))"
           (com.informatimago.rdp::with-non-terminal
               (liste-reference-1-1 scanner)
             (loop :with com.informatimago.rdp::$items := 'nil
                   :do (cond ((word-equal (scanner-current-token scanner) 'tok-virgule)
                              (push (let* (($1 (lse/parse-liste-reference-1-1-1 scanner))
                                           ($2 (let* (($0 (list))) (declare (ignorable $0)) 'nil))
                                           ($0 (list $1 $2)))
                                      (declare (ignorable $0 $1 $2))
                                      (cons (progn $1) $2))
                                    com.informatimago.rdp::$items))
                             (t (loop-finish)))
                   :finally (return (reduce #'append (nreverse com.informatimago.rdp::$items) :initial-value nil))))))
  (progn (fmakunbound 'lse/parse-liste-reference-1-1-1)
         (defun lse/parse-liste-reference-1-1-1 (scanner)
           "(--> liste-reference-1-1-1 (seq (tok-virgule reference) ($2)))"
           (com.informatimago.rdp::with-non-terminal
               (liste-reference-1-1-1 scanner)
             (cond ((word-equal (scanner-current-token scanner) 'tok-virgule)
                    (let* (($1 (accept scanner 'tok-virgule))
                           (tok-virgule $1)
                           (tok-virgule.1 $1)
                           ($2 (lse/parse-reference scanner))
                           (reference $2)
                           (reference.1 $2)
                           ($0 (list $1 $2)))
                      (declare (ignorable $0 reference.1 reference tok-virgule.1 tok-virgule $1 $2))
                      $2))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-virgule)
                     '(--> liste-reference-1-1-1 (alt (seq (tok-virgule reference) ($2))))))))))
  (progn (fmakunbound 'lse/parse-expression)
         (defun lse/parse-expression (scanner)
           "(--> expression (seq (terme-signe expression-1-1) ((if $2 (uncomb (cons terme-signe $2)) terme-signe))))"
           (com.informatimago.rdp::with-non-terminal
               (expression scanner)
             (cond ((member (scanner-current-token scanner)
                            '(tok-identificateur tok-litchaine tok-nombre tok-numero tok-si tok-at tok-pargauche
                              tok-procident tok-moins)
                            :test
                            #'word-equal)
                    (let* (($1 (lse/parse-terme-signe scanner))
                           (terme-signe $1)
                           (terme-signe.1 $1)
                           ($2 (lse/parse-expression-1-1 scanner))
                           ($0 (list $1 $2)))
                      (declare (ignorable $0 terme-signe.1 terme-signe $1 $2))
                      (if $2 (uncomb (cons terme-signe $2)) terme-signe)))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-identificateur tok-litchaine tok-nombre tok-numero tok-si tok-at tok-pargauche
                       tok-procident tok-moins)
                     '(--> expression
                       (alt
                        (seq (terme-signe expression-1-1)
                         ((if $2 (uncomb (cons terme-signe $2)) terme-signe)))))))))))
  (progn (fmakunbound 'lse/parse-reference-1-1-1-1-1-1)
         (defun lse/parse-reference-1-1-1-1-1-1 (scanner)
           "(--> reference-1-1-1-1-1-1 (seq (tok-virgule expression) (expression)))"
           (com.informatimago.rdp::with-non-terminal
               (reference-1-1-1-1-1-1 scanner)
             (cond ((word-equal (scanner-current-token scanner) 'tok-virgule)
                    (let* (($1 (accept scanner 'tok-virgule))
                           (tok-virgule $1)
                           (tok-virgule.1 $1)
                           ($2 (lse/parse-expression scanner))
                           (expression $2)
                           (expression.1 $2)
                           ($0 (list $1 $2)))
                      (declare (ignorable $0 expression.1 expression tok-virgule.1 tok-virgule $1 $2))
                      expression))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-virgule)
                     '(--> reference-1-1-1-1-1-1 (alt (seq (tok-virgule expression) (expression))))))))))
  (progn (fmakunbound 'lse/parse-liste-argument)
         (defun lse/parse-liste-argument (scanner)
           "(--> liste-argument (seq (expression liste-argument-1-1) ((cons $1 $2))))"
           (com.informatimago.rdp::with-non-terminal
               (liste-argument scanner)
             (cond ((member (scanner-current-token scanner)
                            '(tok-moins tok-procident tok-pargauche tok-at tok-si tok-numero tok-nombre
                              tok-litchaine tok-identificateur)
                            :test
                            #'word-equal)
                    (let* (($1 (lse/parse-expression scanner))
                           (expression $1)
                           (expression.1 $1)
                           ($2 (lse/parse-liste-argument-1-1 scanner))
                           ($0 (list $1 $2)))
                      (declare (ignorable $0 expression.1 expression $1 $2))
                      (cons $1 $2)))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-moins tok-procident tok-pargauche tok-at tok-si tok-numero tok-nombre tok-litchaine
                       tok-identificateur)
                     '(--> liste-argument (alt (seq (expression liste-argument-1-1) ((cons $1 $2)))))))))))
  (progn (fmakunbound 'lse/parse-reference-1-1-1)
         (defun lse/parse-reference-1-1-1 (scanner)
           "(--> reference-1-1-1 (alt ((seq (tok-pargauche reference-1-1-1-2-1 tok-pardroite) ((list* :fonction $2))) (seq (tok-crogauche expression reference-1-1-1-1-1 tok-crodroite) ((list* :aref expression $3))))))"
           (com.informatimago.rdp::with-non-terminal
               (reference-1-1-1 scanner)
             (cond ((word-equal (scanner-current-token scanner) 'tok-pargauche)
                    (let* (($1 (accept scanner 'tok-pargauche))
                           (tok-pargauche $1)
                           (tok-pargauche.1 $1)
                           ($2 (lse/parse-reference-1-1-1-2-1 scanner))
                           ($3 (accept scanner 'tok-pardroite))
                           (tok-pardroite $3)
                           (tok-pardroite.1 $3)
                           ($0 (list $1 $2 $3)))
                      (declare
                       (ignorable $0 tok-pardroite.1 tok-pardroite tok-pargauche.1 tok-pargauche $1 $2 $3))
                      (list* :fonction $2)))
                   ((word-equal (scanner-current-token scanner) 'tok-crogauche)
                    (let* (($1 (accept scanner 'tok-crogauche))
                           (tok-crogauche $1)
                           (tok-crogauche.1 $1)
                           ($2 (lse/parse-expression scanner))
                           (expression $2)
                           (expression.1 $2)
                           ($3 (lse/parse-reference-1-1-1-1-1 scanner))
                           ($4 (accept scanner 'tok-crodroite))
                           (tok-crodroite $4)
                           (tok-crodroite.1 $4)
                           ($0 (list $1 $2 $3 $4)))
                      (declare
                       (ignorable $0 tok-crodroite.1 tok-crodroite expression.1 expression tok-crogauche.1
                                  tok-crogauche $1 $2 $3 $4))
                      (list* :aref expression $3)))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-pargauche tok-crogauche)
                     '(--> reference-1-1-1
                       (alt (seq (tok-pargauche reference-1-1-1-2-1 tok-pardroite) ((list* :fonction $2)))
                        (seq (tok-crogauche expression reference-1-1-1-1-1 tok-crodroite)
                         ((list* :aref expression $3)))))))))))
  (progn (fmakunbound 'lse/parse-condition-1)
         (defun lse/parse-condition-1 (scanner)
           "(--> condition-1 (alt ((seq (expression condition-1-2-1) ((if $2 (list (first $2) expression (second $2)) expression))) (seq (tok-non condition) ((list :non condition))))))"
           (com.informatimago.rdp::with-non-terminal
               (condition-1 scanner)
             (cond ((member (scanner-current-token scanner)
                            '(tok-moins tok-procident tok-pargauche tok-at tok-si tok-numero tok-nombre
                              tok-litchaine tok-identificateur)
                            :test
                            #'word-equal)
                    (let* (($1 (lse/parse-expression scanner))
                           (expression $1)
                           (expression.1 $1)
                           ($2 (lse/parse-condition-1-2-1 scanner))
                           ($0 (list $1 $2)))
                      (declare (ignorable $0 expression.1 expression $1 $2))
                      (if $2 (list (first $2) expression (second $2)) expression)))
                   ((word-equal (scanner-current-token scanner) 'tok-non)
                    (let* (($1 (accept scanner 'tok-non))
                           (tok-non $1)
                           (tok-non.1 $1)
                           ($2 (lse/parse-condition scanner))
                           (condition $2)
                           (condition.1 $2)
                           ($0 (list $1 $2)))
                      (declare (ignorable $0 condition.1 condition tok-non.1 tok-non $1 $2))
                      (list :non condition)))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-moins tok-procident tok-pargauche tok-at tok-si tok-numero tok-nombre tok-litchaine
                       tok-identificateur tok-non)
                     '(--> condition-1
                       (alt
                        (seq (expression condition-1-2-1)
                         ((if $2 (list (first $2) expression (second $2)) expression)))
                        (seq (tok-non condition) ((list :non condition)))))))))))
  (progn (fmakunbound 'lse/parse-condition)
         (defun lse/parse-condition (scanner)
           "(--> condition (seq (condition-1) ($1)))"
           (com.informatimago.rdp::with-non-terminal
               (condition scanner)
             (cond ((member (scanner-current-token scanner)
                            '(tok-non tok-identificateur tok-litchaine tok-nombre tok-numero tok-si tok-at
                              tok-pargauche tok-procident tok-moins)
                            :test
                            #'word-equal)
                    (let* (($1 (lse/parse-condition-1 scanner)) ($0 (list $1))) (declare (ignorable $0 $1)) $1))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-non tok-identificateur tok-litchaine tok-nombre tok-numero tok-si tok-at
                       tok-pargauche tok-procident tok-moins)
                     '(--> condition (alt (seq (condition-1) ($1))))))))))
  (progn (fmakunbound 'lse/parse-conjonction-1-1)
         (defun lse/parse-conjonction-1-1 (scanner)
           "(--> conjonction-1-1 (alt ((seq nil ('nil)) (seq (conjonction-1-1-1 conjonction-1-1) ((cons $1 $2))))))"
           (com.informatimago.rdp::with-non-terminal
               (conjonction-1-1 scanner)
             (loop :with com.informatimago.rdp::$items := 'nil
                   :do (cond ((word-equal (scanner-current-token scanner) 'tok-et)
                              (push (let* (($1 (lse/parse-conjonction-1-1-1 scanner))
                                           ($2 (let* (($0 (list))) (declare (ignorable $0)) 'nil))
                                           ($0 (list $1 $2)))
                                      (declare (ignorable $0 $1 $2))
                                      (cons $1 $2))
                                    com.informatimago.rdp::$items))
                             (t (loop-finish)))
                   :finally (return (reduce #'append
                                            (nreverse com.informatimago.rdp::$items)
                                            :initial-value
                                            nil))))))
  (progn (fmakunbound 'lse/parse-conjonction-1-1-1)
         (defun lse/parse-conjonction-1-1-1 (scanner)
           "(--> conjonction-1-1-1 (seq (tok-et condition) (condition)))"
           (com.informatimago.rdp::with-non-terminal
               (conjonction-1-1-1 scanner)
             (cond ((word-equal (scanner-current-token scanner) 'tok-et)
                    (let* (($1 (accept scanner 'tok-et))
                           (tok-et $1)
                           (tok-et.1 $1)
                           ($2 (lse/parse-condition scanner))
                           (condition $2)
                           (condition.1 $2)
                           ($0 (list $1 $2)))
                      (declare (ignorable $0 condition.1 condition tok-et.1 tok-et $1 $2))
                      condition))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-et)
                     '(--> conjonction-1-1-1 (alt (seq (tok-et condition) (condition))))))))))
  (progn (fmakunbound 'lse/parse-conjonction)
         (defun lse/parse-conjonction (scanner)
           "(--> conjonction (seq (condition conjonction-1-1) ((if $2 (list* :et condition $2) condition))))"
           (com.informatimago.rdp::with-non-terminal
               (conjonction scanner)
             (cond ((member (scanner-current-token scanner)
                            '(tok-moins tok-procident tok-pargauche tok-at tok-si tok-numero tok-nombre
                              tok-litchaine tok-identificateur tok-non)
                            :test
                            #'word-equal)
                    (let* (($1 (lse/parse-condition scanner))
                           (condition $1)
                           (condition.1 $1)
                           ($2 (lse/parse-conjonction-1-1 scanner))
                           ($0 (list $1 $2)))
                      (declare (ignorable $0 condition.1 condition $1 $2))
                      (if $2 (list* :et condition $2) condition)))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-moins tok-procident tok-pargauche tok-at tok-si tok-numero tok-nombre
                       tok-litchaine tok-identificateur tok-non)
                     '(--> conjonction
                       (alt
                        (seq (condition conjonction-1-1)
                         ((if $2 (list* :et condition $2) condition)))))))))))
  (progn (fmakunbound 'lse/parse-disjonction-1-1)
         (defun lse/parse-disjonction-1-1 (scanner)
           "(--> disjonction-1-1 (alt ((seq nil ('nil)) (seq (disjonction-1-1-1 disjonction-1-1) ((cons $1 $2))))))"
           (com.informatimago.rdp::with-non-terminal
               (disjonction-1-1 scanner)
             (loop :with com.informatimago.rdp::$items := 'nil
                   :do (cond ((word-equal (scanner-current-token scanner) 'tok-ou)
                              (push (let* (($1 (lse/parse-disjonction-1-1-1 scanner))
                                           ($2 (let* (($0 (list))) (declare (ignorable $0)) 'nil))
                                           ($0 (list $1 $2)))
                                      (declare (ignorable $0 $1 $2))
                                      (cons $1 $2))
                                    com.informatimago.rdp::$items))
                             (t (loop-finish)))
                   :finally (return (reduce #'append
                                            (nreverse com.informatimago.rdp::$items)
                                            :initial-value
                                            nil))))))
  (progn (fmakunbound 'lse/parse-disjonction-1-1-1)
         (defun lse/parse-disjonction-1-1-1 (scanner)
           "(--> disjonction-1-1-1 (seq (tok-ou conjonction) (conjonction)))"
           (com.informatimago.rdp::with-non-terminal
               (disjonction-1-1-1 scanner)
             (cond ((word-equal (scanner-current-token scanner) 'tok-ou)
                    (let* (($1 (accept scanner 'tok-ou))
                           (tok-ou $1)
                           (tok-ou.1 $1)
                           ($2 (lse/parse-conjonction scanner))
                           (conjonction $2)
                           (conjonction.1 $2)
                           ($0 (list $1 $2)))
                      (declare (ignorable $0 conjonction.1 conjonction tok-ou.1 tok-ou $1 $2))
                      conjonction))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-ou)
                     '(--> disjonction-1-1-1
                       (alt (seq (tok-ou conjonction) (conjonction))))))))))
  (progn (fmakunbound 'lse/parse-identificateur)
         (defun lse/parse-identificateur (scanner)
           "(--> identificateur (seq (tok-identificateur) ($1)))"
           (com.informatimago.rdp::with-non-terminal
               (identificateur scanner)
             (cond ((word-equal (scanner-current-token scanner) 'tok-identificateur)
                    (let* (($1 (accept scanner 'tok-identificateur))
                           (tok-identificateur $1)
                           (tok-identificateur.1 $1)
                           ($0 (list $1)))
                      (declare (ignorable $0 tok-identificateur.1 tok-identificateur $1))
                      $1))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-identificateur)
                     '(--> identificateur (alt (seq (tok-identificateur) ($1))))))))))
  (progn (fmakunbound 'lse/parse-liste-identificateur-1-1)
         (defun lse/parse-liste-identificateur-1-1 (scanner)
           "(--> liste-identificateur-1-1 (alt ((seq nil ('nil)) (seq (liste-identificateur-1-1-1 liste-identificateur-1-1) ((cons $1 $2))))))"
           (com.informatimago.rdp::with-non-terminal
               (liste-identificateur-1-1 scanner)
             (loop :with com.informatimago.rdp::$items := 'nil
                   :do (cond ((word-equal (scanner-current-token scanner) 'tok-virgule)
                              (push (let* (($1 (lse/parse-liste-identificateur-1-1-1 scanner))
                                           ($2 (let* (($0 (list))) (declare (ignorable $0)) 'nil))
                                           ($0 (list $1 $2)))
                                      (declare (ignorable $0 $1 $2))
                                      (cons $1 $2))
                                    com.informatimago.rdp::$items))
                             (t (loop-finish)))
                   :finally (return (reduce #'append
                                            (nreverse com.informatimago.rdp::$items)
                                            :initial-value
                                            nil))))))
  (progn (fmakunbound 'lse/parse-liste-identificateur-1-1-1)
         (defun lse/parse-liste-identificateur-1-1-1 (scanner)
           "(--> liste-identificateur-1-1-1 (seq (tok-virgule identificateur) ($2)))"
           (com.informatimago.rdp::with-non-terminal
               (liste-identificateur-1-1-1 scanner)
             (cond ((word-equal (scanner-current-token scanner) 'tok-virgule)
                    (let* (($1 (accept scanner 'tok-virgule))
                           (tok-virgule $1)
                           (tok-virgule.1 $1)
                           ($2 (lse/parse-identificateur scanner))
                           (identificateur $2)
                           (identificateur.1 $2)
                           ($0 (list $1 $2)))
                      (declare
                       (ignorable $0 identificateur.1 identificateur tok-virgule.1
                                  tok-virgule $1 $2))
                      $2))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-virgule)
                     '(--> liste-identificateur-1-1-1
                       (alt (seq (tok-virgule identificateur) ($2))))))))))
  (progn (fmakunbound 'lse/parse-liste-expression-1-1)
         (defun lse/parse-liste-expression-1-1 (scanner)
           "(--> liste-expression-1-1 (alt ((seq nil ('nil)) (seq (liste-expression-1-1-1 liste-expression-1-1) ((cons $1 $2))))))"
           (com.informatimago.rdp::with-non-terminal
               (liste-expression-1-1 scanner)
             (loop :with com.informatimago.rdp::$items := 'nil
                   :do (cond ((word-equal (scanner-current-token scanner) 'tok-virgule)
                              (push (let* (($1 (lse/parse-liste-expression-1-1-1 scanner))
                                           ($2
                                             (let* (($0 (list)))
                                               (declare (ignorable $0))
                                               'nil))
                                           ($0 (list $1 $2)))
                                      (declare (ignorable $0 $1 $2))
                                      (cons $1 $2))
                                    com.informatimago.rdp::$items))
                             (t (loop-finish)))
                   :finally (return (reduce #'append
                                            (nreverse com.informatimago.rdp::$items)
                                            :initial-value
                                            nil))))))
  (progn (fmakunbound 'lse/parse-liste-expression-1-1-1)
         (defun lse/parse-liste-expression-1-1-1 (scanner)
           "(--> liste-expression-1-1-1 (seq (tok-virgule expression) (expression)))"
           (com.informatimago.rdp::with-non-terminal
               (liste-expression-1-1-1 scanner)
             (cond ((word-equal (scanner-current-token scanner) 'tok-virgule)
                    (let* (($1 (accept scanner 'tok-virgule))
                           (tok-virgule $1)
                           (tok-virgule.1 $1)
                           ($2 (lse/parse-expression scanner))
                           (expression $2)
                           (expression.1 $2)
                           ($0 (list $1 $2)))
                      (declare
                       (ignorable $0 expression.1 expression tok-virgule.1
                                  tok-virgule $1 $2))
                      expression))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-virgule)
                     '(--> liste-expression-1-1-1
                       (alt (seq (tok-virgule expression) (expression))))))))))
  (progn (fmakunbound 'lse/parse-liste-argument-1-1)
         (defun lse/parse-liste-argument-1-1 (scanner)
           "(--> liste-argument-1-1 (alt ((seq nil ('nil)) (seq (liste-argument-1-1-1 liste-argument-1-1) ((cons $1 $2))))))"
           (com.informatimago.rdp::with-non-terminal
               (liste-argument-1-1 scanner)
             (loop :with com.informatimago.rdp::$items := 'nil
                   :do (cond ((word-equal (scanner-current-token scanner)
                                          'tok-virgule)
                              (push (let* (($1
                                             (lse/parse-liste-argument-1-1-1
                                              scanner))
                                           ($2
                                             (let*
                                                 (($0 (list)))
                                               (declare (ignorable $0))
                                               'nil))
                                           ($0 (list $1 $2)))
                                      (declare (ignorable $0 $1 $2))
                                      (cons $1 $2))
                                    com.informatimago.rdp::$items))
                             (t (loop-finish)))
                   :finally (return (reduce #'append
                                            (nreverse com.informatimago.rdp::$items)
                                            :initial-value
                                            nil))))))
  (progn (fmakunbound 'lse/parse-liste-argument-1-1-1)
         (defun lse/parse-liste-argument-1-1-1 (scanner)
           "(--> liste-argument-1-1-1 (seq (tok-virgule expression) ($2)))"
           (com.informatimago.rdp::with-non-terminal
               (liste-argument-1-1-1 scanner)
             (cond ((word-equal (scanner-current-token scanner) 'tok-virgule)
                    (let* (($1 (accept scanner 'tok-virgule))
                           (tok-virgule $1)
                           (tok-virgule.1 $1)
                           ($2 (lse/parse-expression scanner))
                           (expression $2)
                           (expression.1 $2)
                           ($0 (list $1 $2)))
                      (declare
                       (ignorable $0 expression.1 expression tok-virgule.1
                                  tok-virgule $1 $2))
                      $2))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-virgule)
                     '(--> liste-argument-1-1-1
                       (alt (seq (tok-virgule expression) ($2))))))))))
  (progn (fmakunbound 'lse/parse-simple-1)
         (defun lse/parse-simple-1 (scanner)
           "(--> simple-1 (alt ((seq (procident simple-1-2-1) ((if $2 (list* :fonction procident (first $2)) (list :vval procident)))) (seq (tok-pargauche disjonction tok-pardroite) ($2)) (seq (tok-at) ((make-instance 'tok-identificateur :kind 'tok-identificateur :text \"@\" :line (token-line $1) :column (token-column $1)))) (seq (tok-si disjonction tok-alors expression tok-sinon expression) ((list :xi disjonction expression.1 expression.2))) (seq (tok-numero) ($1)) (seq (tok-nombre) ($1)) (seq (tok-litchaine) ($1)) (seq (reference) ($1)))))"
           (com.informatimago.rdp::with-non-terminal
               (simple-1 scanner)
             (cond ((word-equal (scanner-current-token scanner)
                                'tok-procident)
                    (let* (($1 (lse/parse-procident scanner))
                           (procident $1)
                           (procident.1 $1)
                           ($2 (lse/parse-simple-1-2-1 scanner))
                           ($0 (list $1 $2)))
                      (declare (ignorable $0 procident.1 procident $1 $2))
                      (if $2
                          (list* :fonction procident (first $2))
                          (list :vval procident))))
                   ((word-equal (scanner-current-token scanner)
                                'tok-pargauche)
                    (let* (($1 (accept scanner 'tok-pargauche))
                           (tok-pargauche $1)
                           (tok-pargauche.1 $1)
                           ($2 (lse/parse-disjonction scanner))
                           (disjonction $2)
                           (disjonction.1 $2)
                           ($3 (accept scanner 'tok-pardroite))
                           (tok-pardroite $3)
                           (tok-pardroite.1 $3)
                           ($0 (list $1 $2 $3)))
                      (declare
                       (ignorable $0 tok-pardroite.1 tok-pardroite
                                  disjonction.1 disjonction tok-pargauche.1
                                  tok-pargauche $1 $2 $3))
                      $2))
                   ((word-equal (scanner-current-token scanner) 'tok-at)
                    (let* (($1 (accept scanner 'tok-at))
                           (tok-at $1)
                           (tok-at.1 $1)
                           ($0 (list $1)))
                      (declare (ignorable $0 tok-at.1 tok-at $1))
                      (make-instance
                       'tok-identificateur
                       :kind
                       'tok-identificateur
                       :text
                       "@"
                       :line
                       (token-line $1)
                       :column
                       (token-column $1))))
                   ((word-equal (scanner-current-token scanner) 'tok-si)
                    (let* (($1 (accept scanner 'tok-si))
                           (tok-si $1)
                           (tok-si.1 $1)
                           ($2 (lse/parse-disjonction scanner))
                           (disjonction $2)
                           (disjonction.1 $2)
                           ($3 (accept scanner 'tok-alors))
                           (tok-alors $3)
                           (tok-alors.1 $3)
                           ($4 (lse/parse-expression scanner))
                           (expression $4)
                           (expression.1 $4)
                           ($5 (accept scanner 'tok-sinon))
                           (tok-sinon $5)
                           (tok-sinon.1 $5)
                           ($6 (lse/parse-expression scanner))
                           (expression.2 $6)
                           ($0 (list $1 $2 $3 $4 $5 $6)))
                      (declare
                       (ignorable $0 expression.2 tok-sinon.1 tok-sinon
                                  expression.1 expression tok-alors.1 tok-alors
                                  disjonction.1 disjonction tok-si.1 tok-si $1 $2 $3 $4
                                  $5 $6))
                      (list :xi disjonction expression.1 expression.2)))
                   ((word-equal (scanner-current-token scanner) 'tok-numero)
                    (let* (($1 (accept scanner 'tok-numero))
                           (tok-numero $1)
                           (tok-numero.1 $1)
                           ($0 (list $1)))
                      (declare (ignorable $0 tok-numero.1 tok-numero $1))
                      $1))
                   ((word-equal (scanner-current-token scanner) 'tok-nombre)
                    (let* (($1 (accept scanner 'tok-nombre))
                           (tok-nombre $1)
                           (tok-nombre.1 $1)
                           ($0 (list $1)))
                      (declare (ignorable $0 tok-nombre.1 tok-nombre $1))
                      $1))
                   ((word-equal (scanner-current-token scanner)
                                'tok-litchaine)
                    (let* (($1 (accept scanner 'tok-litchaine))
                           (tok-litchaine $1)
                           (tok-litchaine.1 $1)
                           ($0 (list $1)))
                      (declare
                       (ignorable $0 tok-litchaine.1 tok-litchaine $1))
                      $1))
                   ((word-equal (scanner-current-token scanner)
                                'tok-identificateur)
                    (let* (($1 (lse/parse-reference scanner))
                           (reference $1)
                           (reference.1 $1)
                           ($0 (list $1)))
                      (declare (ignorable $0 reference.1 reference $1))
                      $1))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-procident tok-pargauche tok-at tok-si tok-numero
                       tok-nombre tok-litchaine tok-identificateur)
                     '(--> simple-1
                       (alt
                        (seq (procident simple-1-2-1)
                         ((if $2
                              (list* :fonction procident (first $2))
                              (list :vval procident))))
                        (seq (tok-pargauche disjonction tok-pardroite) ($2))
                        (seq (tok-at)
                         ((make-instance
                           'tok-identificateur
                           :kind
                           'tok-identificateur
                           :text
                           "@"
                           :line
                           (token-line $1)
                           :column
                           (token-column $1))))
                        (seq
                         (tok-si disjonction tok-alors expression tok-sinon
                          expression)
                         ((list :xi disjonction expression.1 expression.2)))
                        (seq (tok-numero) ($1)) (seq (tok-nombre) ($1))
                        (seq (tok-litchaine) ($1))
                        (seq (reference) ($1))))))))))
  (progn (fmakunbound 'lse/parse-simple)
         (defun lse/parse-simple (scanner)
           "(--> simple (seq (simple-1) ($1)))"
           (com.informatimago.rdp::with-non-terminal
               (simple scanner)
             (cond ((member (scanner-current-token scanner)
                            '(tok-identificateur tok-litchaine tok-nombre
                              tok-numero tok-si tok-at tok-pargauche
                              tok-procident)
                            :test
                            #'word-equal)
                    (let* (($1 (lse/parse-simple-1 scanner)) ($0 (list $1)))
                      (declare (ignorable $0 $1))
                      $1))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-identificateur tok-litchaine tok-nombre
                       tok-numero tok-si tok-at tok-pargauche tok-procident)
                     '(--> simple (alt (seq (simple-1) ($1))))))))))
  (progn (fmakunbound 'lse/parse-facteur-1-1)
         (defun lse/parse-facteur-1-1 (scanner)
           "(--> facteur-1-1 (alt ((seq nil ('nil)) (seq (facteur-1-1-1 facteur-1-1) ((cons (progn $1) $2))))))"
           (com.informatimago.rdp::with-non-terminal
               (facteur-1-1 scanner)
             (loop :with com.informatimago.rdp::$items := 'nil
                   :do (cond ((word-equal (scanner-current-token scanner)
                                          'tok-puissance)
                              (push (let* (($1
                                             (lse/parse-facteur-1-1-1
                                              scanner))
                                           ($2
                                             (let*
                                                 (($0 (list)))
                                               (declare (ignorable $0))
                                               'nil))
                                           ($0 (list $1 $2)))
                                      (declare (ignorable $0 $1 $2))
                                      (cons (progn $1) $2))
                                    com.informatimago.rdp::$items))
                             (t (loop-finish)))
                   :finally (return (reduce #'append
                                            (nreverse
                                             com.informatimago.rdp::$items)
                                            :initial-value
                                            nil))))))
  (progn (fmakunbound 'lse/parse-facteur-1-1-1)
         (defun lse/parse-facteur-1-1-1 (scanner)
           "(--> facteur-1-1-1 (seq (tok-puissance simple) ((list :puissance simple))))"
           (com.informatimago.rdp::with-non-terminal
               (facteur-1-1-1 scanner)
             (cond ((word-equal (scanner-current-token scanner)
                                'tok-puissance)
                    (let* (($1 (accept scanner 'tok-puissance))
                           (tok-puissance $1)
                           (tok-puissance.1 $1)
                           ($2 (lse/parse-simple scanner))
                           (simple $2)
                           (simple.1 $2)
                           ($0 (list $1 $2)))
                      (declare
                       (ignorable $0 simple.1 simple tok-puissance.1
                                  tok-puissance $1 $2))
                      (list :puissance simple)))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-puissance)
                     '(--> facteur-1-1-1
                       (alt
                        (seq (tok-puissance simple)
                         ((list :puissance simple)))))))))))
  (progn (fmakunbound 'lse/parse-terme-1-1)
         (defun lse/parse-terme-1-1 (scanner)
           "(--> terme-1-1 (alt ((seq nil ('nil)) (seq (terme-1-1-1 terme-1-1) ((cons (progn $1) $2))))))"
           (com.informatimago.rdp::with-non-terminal
               (terme-1-1 scanner)
             (loop :with com.informatimago.rdp::$items := 'nil
                   :do (cond ((member
                               (scanner-current-token scanner)
                               '(tok-fois tok-divise)
                               :test
                               #'word-equal)
                              (push
                               (let*
                                   (($1 (lse/parse-terme-1-1-1 scanner))
                                    ($2
                                      (let*
                                          (($0 (list)))
                                        (declare (ignorable $0))
                                        'nil))
                                    ($0 (list $1 $2)))
                                 (declare (ignorable $0 $1 $2))
                                 (cons (progn $1) $2))
                               com.informatimago.rdp::$items))
                             (t (loop-finish)))
                   :finally (return (reduce
                                     #'append
                                     (nreverse
                                      com.informatimago.rdp::$items)
                                     :initial-value
                                     nil))))))
  (progn (fmakunbound 'lse/parse-terme-1-1-1)
         (defun lse/parse-terme-1-1-1 (scanner)
           "(--> terme-1-1-1 (alt ((seq (tok-divise facteur) ((list :divise facteur))) (seq (tok-fois facteur) ((list :fois facteur))))))"
           (com.informatimago.rdp::with-non-terminal
               (terme-1-1-1 scanner)
             (cond ((word-equal (scanner-current-token scanner)
                                'tok-divise)
                    (let* (($1 (accept scanner 'tok-divise))
                           (tok-divise $1)
                           (tok-divise.1 $1)
                           ($2 (lse/parse-facteur scanner))
                           (facteur $2)
                           (facteur.1 $2)
                           ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        facteur.1
                        facteur
                        tok-divise.1
                        tok-divise
                        $1
                        $2))
                      (list :divise facteur)))
                   ((word-equal (scanner-current-token scanner)
                                'tok-fois)
                    (let* (($1 (accept scanner 'tok-fois))
                           (tok-fois $1)
                           (tok-fois.1 $1)
                           ($2 (lse/parse-facteur scanner))
                           (facteur $2)
                           (facteur.1 $2)
                           ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        facteur.1
                        facteur
                        tok-fois.1
                        tok-fois
                        $1
                        $2))
                      (list :fois facteur)))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-divise tok-fois)
                     '(-->
                       terme-1-1-1
                       (alt
                        (seq
                         (tok-divise facteur)
                         ((list :divise facteur)))
                        (seq
                         (tok-fois facteur)
                         ((list :fois facteur)))))))))))
  (progn (fmakunbound 'lse/parse-expression-1-1)
         (defun lse/parse-expression-1-1 (scanner)
           "(--> expression-1-1 (alt ((seq nil ('nil)) (seq (expression-1-1-1 expression-1-1) ((cons (progn $1) $2))))))"
           (com.informatimago.rdp::with-non-terminal
               (expression-1-1 scanner)
             (loop :with com.informatimago.rdp::$items := 'nil
                   :do (cond
                         ((member
                           (scanner-current-token scanner)
                           '(tok-moins tok-concat tok-plus)
                           :test
                           #'word-equal)
                          (push
                           (let*
                               (($1
                                  (lse/parse-expression-1-1-1
                                   scanner))
                                ($2
                                  (let*
                                      (($0 (list)))
                                    (declare (ignorable $0))
                                    'nil))
                                ($0 (list $1 $2)))
                             (declare (ignorable $0 $1 $2))
                             (cons (progn $1) $2))
                           com.informatimago.rdp::$items))
                         (t (loop-finish)))
                   :finally (return
                              (reduce
                               #'append
                               (nreverse
                                com.informatimago.rdp::$items)
                               :initial-value
                               nil))))))
  (progn (fmakunbound 'lse/parse-expression-1-1-1)
         (defun lse/parse-expression-1-1-1 (scanner)
           "(--> expression-1-1-1 (alt ((seq (tok-plus terme) ((list :plus terme))) (seq (tok-concat terme) ((list :concat terme))) (seq (tok-moins terme) ((list :moins terme))))))"
           (com.informatimago.rdp::with-non-terminal
               (expression-1-1-1 scanner)
             (cond ((word-equal
                     (scanner-current-token scanner)
                     'tok-plus)
                    (let*
                        (($1 (accept scanner 'tok-plus))
                         (tok-plus $1)
                         (tok-plus.1 $1)
                         ($2 (lse/parse-terme scanner))
                         (terme $2)
                         (terme.1 $2)
                         ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        terme.1
                        terme
                        tok-plus.1
                        tok-plus
                        $1
                        $2))
                      (list :plus terme)))
                   ((word-equal
                     (scanner-current-token scanner)
                     'tok-concat)
                    (let*
                        (($1 (accept scanner 'tok-concat))
                         (tok-concat $1)
                         (tok-concat.1 $1)
                         ($2 (lse/parse-terme scanner))
                         (terme $2)
                         (terme.1 $2)
                         ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        terme.1
                        terme
                        tok-concat.1
                        tok-concat
                        $1
                        $2))
                      (list :concat terme)))
                   ((word-equal
                     (scanner-current-token scanner)
                     'tok-moins)
                    (let*
                        (($1 (accept scanner 'tok-moins))
                         (tok-moins $1)
                         (tok-moins.1 $1)
                         ($2 (lse/parse-terme scanner))
                         (terme $2)
                         (terme.1 $2)
                         ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        terme.1
                        terme
                        tok-moins.1
                        tok-moins
                        $1
                        $2))
                      (list :moins terme)))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-plus tok-concat tok-moins)
                     '(-->
                       expression-1-1-1
                       (alt
                        (seq
                         (tok-plus terme)
                         ((list :plus terme)))
                        (seq
                         (tok-concat terme)
                         ((list :concat terme)))
                        (seq
                         (tok-moins terme)
                         ((list :moins terme)))))))))))
  (progn (fmakunbound 'lse/parse-executer-1-1)
         (defun lse/parse-executer-1-1 (scanner)
           "(--> executer-1-1 (alt ((seq (tok-virgule expression) (expression)) (seq nil ('nil)))))"
           (com.informatimago.rdp::with-non-terminal
               (executer-1-1 scanner)
             (cond ((word-equal
                     (scanner-current-token scanner)
                     'tok-virgule)
                    (let*
                        (($1 (accept scanner 'tok-virgule))
                         (tok-virgule $1)
                         (tok-virgule.1 $1)
                         ($2
                           (lse/parse-expression scanner))
                         (expression $2)
                         (expression.1 $2)
                         ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        expression.1
                        expression
                        tok-virgule.1
                        tok-virgule
                        $1
                        $2))
                      expression))))))
  (progn (fmakunbound 'lse/parse-supprimer-1-1)
         (defun lse/parse-supprimer-1-1 (scanner)
           "(--> supprimer-1-1 (alt ((seq (tok-virgule expression) (expression)) (seq nil ('nil)))))"
           (com.informatimago.rdp::with-non-terminal
               (supprimer-1-1 scanner)
             (cond ((word-equal
                     (scanner-current-token scanner)
                     'tok-virgule)
                    (let*
                        (($1 (accept scanner 'tok-virgule))
                         (tok-virgule $1)
                         (tok-virgule.1 $1)
                         ($2
                           (lse/parse-expression scanner))
                         (expression $2)
                         (expression.1 $2)
                         ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        expression.1
                        expression
                        tok-virgule.1
                        tok-virgule
                        $1
                        $2))
                      expression))))))
  (progn (fmakunbound 'lse/parse-charger-1-1)
         (defun lse/parse-charger-1-1 (scanner)
           "(--> charger-1-1 (alt ((seq (tok-virgule identificateur) (identificateur)) (seq nil ('nil)))))"
           (com.informatimago.rdp::with-non-terminal
               (charger-1-1 scanner)
             (cond ((word-equal
                     (scanner-current-token scanner)
                     'tok-virgule)
                    (let*
                        (($1 (accept scanner 'tok-virgule))
                         (tok-virgule $1)
                         (tok-virgule.1 $1)
                         ($2
                           (lse/parse-identificateur
                            scanner))
                         (identificateur $2)
                         (identificateur.1 $2)
                         ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        identificateur.1
                        identificateur
                        tok-virgule.1
                        tok-virgule
                        $1
                        $2))
                      identificateur))))))
  (progn (fmakunbound 'lse/parse-retour-1-1)
         (defun lse/parse-retour-1-1 (scanner)
           "(--> retour-1-1 (alt ((seq (tok-en expression) (expression)) (seq nil ('nil)))))"
           (com.informatimago.rdp::with-non-terminal
               (retour-1-1 scanner)
             (cond ((word-equal
                     (scanner-current-token scanner)
                     'tok-en)
                    (let*
                        (($1 (accept scanner 'tok-en))
                         (tok-en $1)
                         (tok-en.1 $1)
                         ($2
                           (lse/parse-expression scanner))
                         (expression $2)
                         (expression.1 $2)
                         ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        expression.1
                        expression
                        tok-en.1
                        tok-en
                        $1
                        $2))
                      expression))))))
  (progn (fmakunbound 'lse/parse-faire-1-2)
         (defun lse/parse-faire-1-2 (scanner)
           "(--> faire-1-2 (alt ((seq (tok-tant tok-que disjonction) ((list :faire-tant-que $3))) (seq (tok-jusqua expression) ((list :faire-jusqu-a $2))))))"
           (com.informatimago.rdp::with-non-terminal
               (faire-1-2 scanner)
             (cond ((word-equal
                     (scanner-current-token scanner)
                     'tok-tant)
                    (let*
                        (($1 (accept scanner 'tok-tant))
                         (tok-tant $1)
                         (tok-tant.1 $1)
                         ($2 (accept scanner 'tok-que))
                         (tok-que $2)
                         (tok-que.1 $2)
                         ($3
                           (lse/parse-disjonction scanner))
                         (disjonction $3)
                         (disjonction.1 $3)
                         ($0 (list $1 $2 $3)))
                      (declare
                       (ignorable
                        $0
                        disjonction.1
                        disjonction
                        tok-que.1
                        tok-que
                        tok-tant.1
                        tok-tant
                        $1
                        $2
                        $3))
                      (list :faire-tant-que $3)))
                   ((word-equal
                     (scanner-current-token scanner)
                     'tok-jusqua)
                    (let*
                        (($1 (accept scanner 'tok-jusqua))
                         (tok-jusqua $1)
                         (tok-jusqua.1 $1)
                         ($2
                           (lse/parse-expression scanner))
                         (expression $2)
                         (expression.1 $2)
                         ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        expression.1
                        expression
                        tok-jusqua.1
                        tok-jusqua
                        $1
                        $2))
                      (list :faire-jusqu-a $2)))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-tant tok-jusqua)
                     '(-->
                       faire-1-2
                       (alt
                        (seq
                         (tok-tant tok-que disjonction)
                         ((list :faire-tant-que $3)))
                        (seq
                         (tok-jusqua expression)
                         ((list
                           :faire-jusqu-a
                           $2)))))))))))
  (progn (fmakunbound 'lse/parse-faire-1-1)
         (defun lse/parse-faire-1-1 (scanner)
           "(--> faire-1-1 (alt ((seq (tok-pas expression) (expression)) (seq nil ('nil)))))"
           (com.informatimago.rdp::with-non-terminal
               (faire-1-1 scanner)
             (cond ((word-equal
                     (scanner-current-token scanner)
                     'tok-pas)
                    (let*
                        (($1 (accept scanner 'tok-pas))
                         (tok-pas $1)
                         (tok-pas.1 $1)
                         ($2
                           (lse/parse-expression scanner))
                         (expression $2)
                         (expression.1 $2)
                         ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        expression.1
                        expression
                        tok-pas.1
                        tok-pas
                        $1
                        $2))
                      expression))))))
  (progn (fmakunbound 'lse/parse-liste-inst-ou-decl)
         (defun lse/parse-liste-inst-ou-decl (scanner)
           "(--> liste-inst-ou-decl (seq (liste-inst-ou-decl-1) ($1)))"
           (com.informatimago.rdp::with-non-terminal
               (liste-inst-ou-decl scanner)
             (cond ((member
                     (scanner-current-token scanner)
                     '(tok-liberer
                       tok-executer
                       tok-supprimer
                       tok-charger
                       tok-garer
                       tok-resultat
                       tok-retour
                       tok-faire
                       tok-debut
                       tok-pause
                       tok-terminer
                       tok-si
                       tok-aller
                       tok-afficher
                       tok-lire
                       tok-procident
                       tok-identificateur
                       tok-commentaire
                       tok-chaine
                       tok-tableau)
                     :test
                     #'word-equal)
                    (let*
                        (($1
                           (lse/parse-liste-inst-ou-decl-1
                            scanner))
                         ($0 (list $1)))
                      (declare (ignorable $0 $1))
                      $1))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-liberer
                       tok-executer
                       tok-supprimer
                       tok-charger
                       tok-garer
                       tok-resultat
                       tok-retour
                       tok-faire
                       tok-debut
                       tok-pause
                       tok-terminer
                       tok-si
                       tok-aller
                       tok-afficher
                       tok-lire
                       tok-procident
                       tok-identificateur
                       tok-commentaire
                       tok-chaine
                       tok-tableau)
                     '(-->
                       liste-inst-ou-decl
                       (alt
                        (seq
                         (liste-inst-ou-decl-1)
                         ($1))))))))))
  (progn (fmakunbound 'lse/parse-si-alors-sinon-1-1)
         (defun lse/parse-si-alors-sinon-1-1 (scanner)
           "(--> si-alors-sinon-1-1 (alt ((seq (tok-sinon instruction) (instruction)) (seq nil ('nil)))))"
           (com.informatimago.rdp::with-non-terminal
               (si-alors-sinon-1-1 scanner)
             (cond ((word-equal
                     (scanner-current-token scanner)
                     'tok-sinon)
                    (let*
                        (($1 (accept scanner 'tok-sinon))
                         (tok-sinon $1)
                         (tok-sinon.1 $1)
                         ($2
                           (lse/parse-instruction scanner))
                         (instruction $2)
                         (instruction.1 $2)
                         ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        instruction.1
                        instruction
                        tok-sinon.1
                        tok-sinon
                        $1
                        $2))
                      instruction))))))
  (progn (fmakunbound 'lse/parse-instruction)
         (defun lse/parse-instruction (scanner)
           "(--> instruction (seq (instruction-1) ($1)))"
           (com.informatimago.rdp::with-non-terminal
               (instruction scanner)
             (cond ((member
                     (scanner-current-token scanner)
                     '(tok-liberer
                       tok-executer
                       tok-supprimer
                       tok-charger
                       tok-garer
                       tok-resultat
                       tok-retour
                       tok-faire
                       tok-debut
                       tok-pause
                       tok-terminer
                       tok-si
                       tok-aller
                       tok-afficher
                       tok-lire
                       tok-procident
                       tok-identificateur)
                     :test
                     #'word-equal)
                    (let*
                        (($1
                           (lse/parse-instruction-1
                            scanner))
                         ($0 (list $1)))
                      (declare (ignorable $0 $1))
                      $1))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-liberer
                       tok-executer
                       tok-supprimer
                       tok-charger
                       tok-garer
                       tok-resultat
                       tok-retour
                       tok-faire
                       tok-debut
                       tok-pause
                       tok-terminer
                       tok-si
                       tok-aller
                       tok-afficher
                       tok-lire
                       tok-procident
                       tok-identificateur)
                     '(-->
                       instruction
                       (alt
                        (seq
                         (instruction-1)
                         ($1))))))))))
  (progn (fmakunbound 'lse/parse-disjonction)
         (defun lse/parse-disjonction (scanner)
           "(--> disjonction (seq (conjonction disjonction-1-1) ((if $2 (list* :ou conjonction $2) conjonction))))"
           (com.informatimago.rdp::with-non-terminal
               (disjonction scanner)
             (cond ((member
                     (scanner-current-token scanner)
                     '(tok-non
                       tok-identificateur
                       tok-litchaine
                       tok-nombre
                       tok-numero
                       tok-si
                       tok-at
                       tok-pargauche
                       tok-procident
                       tok-moins)
                     :test
                     #'word-equal)
                    (let*
                        (($1
                           (lse/parse-conjonction scanner))
                         (conjonction $1)
                         (conjonction.1 $1)
                         ($2
                           (lse/parse-disjonction-1-1
                            scanner))
                         ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        conjonction.1
                        conjonction
                        $1
                        $2))
                      (if
                       $2
                       (list* :ou conjonction $2)
                       conjonction)))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-non
                       tok-identificateur
                       tok-litchaine
                       tok-nombre
                       tok-numero
                       tok-si
                       tok-at
                       tok-pargauche
                       tok-procident
                       tok-moins)
                     '(-->
                       disjonction
                       (alt
                        (seq
                         (conjonction disjonction-1-1)
                         ((if
                           $2
                           (list* :ou conjonction $2)
                           conjonction)))))))))))
  (progn (fmakunbound 'lse/parse-specification-1)
         (defun lse/parse-specification-1 (scanner)
           "(--> specification-1 (alt ((seq (tok-divise) ((list :spec-slash '(:rep-1)))) (seq (tok-x) ((list :spec-space '(:rep-1)))) (seq (tok-c) ((list :spec-cr '(:rep-1)))) (seq (tok-l) ((list :spec-nl '(:rep-1)))) (seq (tok-u) ((list :spec-u '(:rep-1)))) (seq (tok-f tok-numero tok-point tok-numero) ((list :spec-f '(:rep-1) $2 $4))) (seq (tok-e tok-numero tok-point tok-numero) ((list :spec-e '(:rep-1) $2 $4))) (seq (spec-rep-fois specification-1-9-1) ((if (listp $2) (list* (first $2) spec-rep-fois (rest $2)) (list $2 spec-rep-fois)))) (seq (spec-rep-num specification-1-10-1) ((if (listp $2) (list* (first $2) spec-rep-num (rest $2)) (list $2 spec-rep-num)))) (seq (tok-litchaine) ((list :spec-chaine '(:rep-1) $1))))))"
           (com.informatimago.rdp::with-non-terminal
               (specification-1 scanner)
             (cond ((word-equal
                     (scanner-current-token scanner)
                     'tok-divise)
                    (let*
                        (($1 (accept scanner 'tok-divise))
                         (tok-divise $1)
                         (tok-divise.1 $1)
                         ($0 (list $1)))
                      (declare
                       (ignorable
                        $0
                        tok-divise.1
                        tok-divise
                        $1))
                      (list :spec-slash '(:rep-1))))
                   ((word-equal
                     (scanner-current-token scanner)
                     'tok-x)
                    (let*
                        (($1 (accept scanner 'tok-x))
                         (tok-x $1)
                         (tok-x.1 $1)
                         ($0 (list $1)))
                      (declare
                       (ignorable $0 tok-x.1 tok-x $1))
                      (list :spec-space '(:rep-1))))
                   ((word-equal
                     (scanner-current-token scanner)
                     'tok-c)
                    (let*
                        (($1 (accept scanner 'tok-c))
                         (tok-c $1)
                         (tok-c.1 $1)
                         ($0 (list $1)))
                      (declare
                       (ignorable $0 tok-c.1 tok-c $1))
                      (list :spec-cr '(:rep-1))))
                   ((word-equal
                     (scanner-current-token scanner)
                     'tok-l)
                    (let*
                        (($1 (accept scanner 'tok-l))
                         (tok-l $1)
                         (tok-l.1 $1)
                         ($0 (list $1)))
                      (declare
                       (ignorable $0 tok-l.1 tok-l $1))
                      (list :spec-nl '(:rep-1))))
                   ((word-equal
                     (scanner-current-token scanner)
                     'tok-u)
                    (let*
                        (($1 (accept scanner 'tok-u))
                         (tok-u $1)
                         (tok-u.1 $1)
                         ($0 (list $1)))
                      (declare
                       (ignorable $0 tok-u.1 tok-u $1))
                      (list :spec-u '(:rep-1))))
                   ((word-equal
                     (scanner-current-token scanner)
                     'tok-f)
                    (let*
                        (($1 (accept scanner 'tok-f))
                         (tok-f $1)
                         (tok-f.1 $1)
                         ($2 (accept scanner 'tok-numero))
                         (tok-numero $2)
                         (tok-numero.1 $2)
                         ($3 (accept scanner 'tok-point))
                         (tok-point $3)
                         (tok-point.1 $3)
                         ($4 (accept scanner 'tok-numero))
                         (tok-numero.2 $4)
                         ($0 (list $1 $2 $3 $4)))
                      (declare
                       (ignorable
                        $0
                        tok-numero.2
                        tok-point.1
                        tok-point
                        tok-numero.1
                        tok-numero
                        tok-f.1
                        tok-f
                        $1
                        $2
                        $3
                        $4))
                      (list :spec-f '(:rep-1) $2 $4)))
                   ((word-equal
                     (scanner-current-token scanner)
                     'tok-e)
                    (let*
                        (($1 (accept scanner 'tok-e))
                         (tok-e $1)
                         (tok-e.1 $1)
                         ($2 (accept scanner 'tok-numero))
                         (tok-numero $2)
                         (tok-numero.1 $2)
                         ($3 (accept scanner 'tok-point))
                         (tok-point $3)
                         (tok-point.1 $3)
                         ($4 (accept scanner 'tok-numero))
                         (tok-numero.2 $4)
                         ($0 (list $1 $2 $3 $4)))
                      (declare
                       (ignorable
                        $0
                        tok-numero.2
                        tok-point.1
                        tok-point
                        tok-numero.1
                        tok-numero
                        tok-e.1
                        tok-e
                        $1
                        $2
                        $3
                        $4))
                      (list :spec-e '(:rep-1) $2 $4)))
                   ((word-equal
                     (scanner-current-token scanner)
                     'tok-fois)
                    (let*
                        (($1
                           (lse/parse-spec-rep-fois
                            scanner))
                         (spec-rep-fois $1)
                         (spec-rep-fois.1 $1)
                         ($2
                           (lse/parse-specification-1-9-1
                            scanner))
                         ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        spec-rep-fois.1
                        spec-rep-fois
                        $1
                        $2))
                      (if
                       (listp $2)
                       (list*
                        (first $2)
                        spec-rep-fois
                        (rest $2))
                       (list $2 spec-rep-fois))))
                   ((word-equal
                     (scanner-current-token scanner)
                     'tok-numero)
                    (let*
                        (($1
                           (lse/parse-spec-rep-num scanner))
                         (spec-rep-num $1)
                         (spec-rep-num.1 $1)
                         ($2
                           (lse/parse-specification-1-10-1
                            scanner))
                         ($0 (list $1 $2)))
                      (declare
                       (ignorable
                        $0
                        spec-rep-num.1
                        spec-rep-num
                        $1
                        $2))
                      (if
                       (listp $2)
                       (list*
                        (first $2)
                        spec-rep-num
                        (rest $2))
                       (list $2 spec-rep-num))))
                   ((word-equal
                     (scanner-current-token scanner)
                     'tok-litchaine)
                    (let*
                        (($1
                           (accept scanner 'tok-litchaine))
                         (tok-litchaine $1)
                         (tok-litchaine.1 $1)
                         ($0 (list $1)))
                      (declare
                       (ignorable
                        $0
                        tok-litchaine.1
                        tok-litchaine
                        $1))
                      (list :spec-chaine '(:rep-1) $1)))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-divise
                       tok-x
                       tok-c
                       tok-l
                       tok-u
                       tok-f
                       tok-e
                       tok-fois
                       tok-numero
                       tok-litchaine)
                     '(-->
                       specification-1
                       (alt
                        (seq
                         (tok-divise)
                         ((list :spec-slash '(:rep-1))))
                        (seq
                         (tok-x)
                         ((list :spec-space '(:rep-1))))
                        (seq
                         (tok-c)
                         ((list :spec-cr '(:rep-1))))
                        (seq
                         (tok-l)
                         ((list :spec-nl '(:rep-1))))
                        (seq
                         (tok-u)
                         ((list :spec-u '(:rep-1))))
                        (seq
                         (tok-f
                          tok-numero
                          tok-point
                          tok-numero)
                         ((list
                           :spec-f
                           '(:rep-1)
                           $2
                           $4)))
                        (seq
                         (tok-e
                          tok-numero
                          tok-point
                          tok-numero)
                         ((list
                           :spec-e
                           '(:rep-1)
                           $2
                           $4)))
                        (seq
                         (spec-rep-fois
                          specification-1-9-1)
                         ((if
                           (listp $2)
                           (list*
                            (first $2)
                            spec-rep-fois
                            (rest $2))
                           (list $2 spec-rep-fois))))
                        (seq
                         (spec-rep-num
                          specification-1-10-1)
                         ((if
                           (listp $2)
                           (list*
                            (first $2)
                            spec-rep-num
                            (rest $2))
                           (list $2 spec-rep-num))))
                        (seq
                         (tok-litchaine)
                         ((list
                           :spec-chaine
                           '(:rep-1)
                           $1)))))))))))
  (progn (fmakunbound 'lse/parse-specification)
         (defun lse/parse-specification (scanner)
           "(--> specification (seq (specification-1) ($1)))"
           (com.informatimago.rdp::with-non-terminal
               (specification scanner)
             (cond ((member
                     (scanner-current-token scanner)
                     '(tok-litchaine
                       tok-numero
                       tok-fois
                       tok-e
                       tok-f
                       tok-u
                       tok-l
                       tok-c
                       tok-x
                       tok-divise)
                     :test
                     #'word-equal)
                    (let*
                        (($1
                           (lse/parse-specification-1
                            scanner))
                         ($0 (list $1)))
                      (declare (ignorable $0 $1))
                      $1))
                   (t
                    (com.informatimago.rdp::error-unexpected-token
                     scanner
                     '(tok-litchaine
                       tok-numero
                       tok-fois
                       tok-e
                       tok-f
                       tok-u
                       tok-l
                       tok-c
                       tok-x
                       tok-divise)
                     '(-->
                       specification
                       (alt
                        (seq
                         (specification-1)
                         ($1))))))))))
  (progn (fmakunbound 'lse/parse-liste-spec-1-1)
         (defun lse/parse-liste-spec-1-1 (scanner)
           "(--> liste-spec-1-1 (alt ((seq nil ('nil)) (seq (liste-spec-1-1-1 liste-spec-1-1) ((cons $1 $2))))))"
           (com.informatimago.rdp::with-non-terminal
               (liste-spec-1-1 scanner)
             (loop :with com.informatimago.rdp::$items
                     :=
                   'nil
                   :do (cond
                         ((word-equal
                           (scanner-current-token
                            scanner)
                           'tok-virgule)
                          (push
                           (let*
                               (($1
                                  (lse/parse-liste-spec-1-1-1
                                   scanner))
                                ($2
                                  (let*
                                      (($0 (list)))
                                    (declare (ignorable $0))
                                    'nil))
                                ($0 (list $1 $2)))
                             (declare
                              (ignorable $0 $1 $2))
                             (cons $1 $2))
                           com.informatimago.rdp::$items))
                         (t (loop-finish)))
                   :finally (return
                              (reduce
                               #'append
                               (nreverse
                                com.informatimago.rdp::$items)
                               :initial-value
                               nil))))))
  (progn (fmakunbound
          'lse/parse-liste-spec-1-1-1)
         (defun
             lse/parse-liste-spec-1-1-1
             (scanner)
           "(--> liste-spec-1-1-1 (seq (tok-virgule specification) ($2)))"
           (com.informatimago.rdp::with-non-terminal
               (liste-spec-1-1-1 scanner)
             (cond
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-virgule)
                (let*
                    (($1 (accept scanner 'tok-virgule))
                     (tok-virgule $1)
                     (tok-virgule.1 $1)
                     ($2
                       (lse/parse-specification
                        scanner))
                     (specification $2)
                     (specification.1 $2)
                     ($0 (list $1 $2)))
                  (declare
                   (ignorable
                    $0
                    specification.1
                    specification
                    tok-virgule.1
                    tok-virgule
                    $1
                    $2))
                  $2))
               (t
                (com.informatimago.rdp::error-unexpected-token
                 scanner
                 '(tok-virgule)
                 '(-->
                   liste-spec-1-1-1
                   (alt
                    (seq
                     (tok-virgule specification)
                     ($2))))))))))
  (progn (fmakunbound
          'lse/parse-liste-expression)
         (defun
             lse/parse-liste-expression
             (scanner)
           "(--> liste-expression (seq (expression liste-expression-1-1) ((cons expression $2))))"
           (com.informatimago.rdp::with-non-terminal
               (liste-expression scanner)
             (cond
               ((member
                 (scanner-current-token scanner)
                 '(tok-moins
                   tok-procident
                   tok-pargauche
                   tok-at
                   tok-si
                   tok-numero
                   tok-nombre
                   tok-litchaine
                   tok-identificateur)
                 :test
                 #'word-equal)
                (let*
                    (($1
                       (lse/parse-expression scanner))
                     (expression $1)
                     (expression.1 $1)
                     ($2
                       (lse/parse-liste-expression-1-1
                        scanner))
                     ($0 (list $1 $2)))
                  (declare
                   (ignorable
                    $0
                    expression.1
                    expression
                    $1
                    $2))
                  (cons expression $2)))
               (t
                (com.informatimago.rdp::error-unexpected-token
                 scanner
                 '(tok-moins
                   tok-procident
                   tok-pargauche
                   tok-at
                   tok-si
                   tok-numero
                   tok-nombre
                   tok-litchaine
                   tok-identificateur)
                 '(-->
                   liste-expression
                   (alt
                    (seq
                     (expression
                      liste-expression-1-1)
                     ((cons expression $2)))))))))))
  (progn (fmakunbound 'lse/parse-afficher-1-1)
         (defun
             lse/parse-afficher-1-1
             (scanner)
           "(--> afficher-1-1 (alt ((seq (liste-expression) ((list* :afficher nil $1))) (seq (format afficher-1-1-1-1) ((list* :afficher $1 $2))))))"
           (com.informatimago.rdp::with-non-terminal
               (afficher-1-1 scanner)
             (cond
               ((member
                 (scanner-current-token scanner)
                 '(tok-identificateur
                   tok-litchaine
                   tok-nombre
                   tok-numero
                   tok-si
                   tok-at
                   tok-pargauche
                   tok-procident
                   tok-moins)
                 :test
                 #'word-equal)
                (let*
                    (($1
                       (lse/parse-liste-expression
                        scanner))
                     (liste-expression $1)
                     (liste-expression.1 $1)
                     ($0 (list $1)))
                  (declare
                   (ignorable
                    $0
                    liste-expression.1
                    liste-expression
                    $1))
                  (list* :afficher nil $1)))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-crogauche)
                (let*
                    (($1 (lse/parse-format scanner))
                     (format $1)
                     (format.1 $1)
                     ($2
                       (lse/parse-afficher-1-1-1-1
                        scanner))
                     ($0 (list $1 $2)))
                  (declare
                   (ignorable
                    $0
                    format.1
                    format
                    $1
                    $2))
                  (list* :afficher $1 $2)))
               (t
                (com.informatimago.rdp::error-unexpected-token
                 scanner
                 '(tok-identificateur
                   tok-litchaine
                   tok-nombre
                   tok-numero
                   tok-si
                   tok-at
                   tok-pargauche
                   tok-procident
                   tok-moins
                   tok-crogauche)
                 '(-->
                   afficher-1-1
                   (alt
                    (seq
                     (liste-expression)
                     ((list* :afficher nil $1)))
                    (seq
                     (format afficher-1-1-1-1)
                     ((list*
                       :afficher
                       $1
                       $2)))))))))))
  (progn (fmakunbound 'lse/parse-liste-reference)
         (defun
             lse/parse-liste-reference
             (scanner)
           "(--> liste-reference (seq (reference liste-reference-1-1) ((cons $1 $2))))"
           (com.informatimago.rdp::with-non-terminal
               (liste-reference scanner)
             (cond
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-identificateur)
                (let*
                    (($1 (lse/parse-reference scanner))
                     (reference $1)
                     (reference.1 $1)
                     ($2
                       (lse/parse-liste-reference-1-1
                        scanner))
                     ($0 (list $1 $2)))
                  (declare
                   (ignorable
                    $0
                    reference.1
                    reference
                    $1
                    $2))
                  (cons $1 $2)))
               (t
                (com.informatimago.rdp::error-unexpected-token
                 scanner
                 '(tok-identificateur)
                 '(-->
                   liste-reference
                   (alt
                    (seq
                     (reference liste-reference-1-1)
                     ((cons $1 $2)))))))))))
  (progn (fmakunbound 'lse/parse-appel-1-1)
         (defun
             lse/parse-appel-1-1
             (scanner)
           "(--> appel-1-1 (alt ((seq nil ('nil)) (seq (liste-argument) ($1)))))"
           (com.informatimago.rdp::with-non-terminal
               (appel-1-1 scanner)
             (cond
               ((member
                 (scanner-current-token scanner)
                 '(tok-identificateur
                   tok-litchaine
                   tok-nombre
                   tok-numero
                   tok-si
                   tok-at
                   tok-pargauche
                   tok-procident
                   tok-moins)
                 :test
                 #'word-equal)
                (let*
                    (($1
                       (lse/parse-liste-argument
                        scanner))
                     (liste-argument $1)
                     (liste-argument.1 $1)
                     ($0 (list $1)))
                  (declare
                   (ignorable
                    $0
                    liste-argument.1
                    liste-argument
                    $1))
                  $1))))))
  (progn (fmakunbound 'lse/parse-procident)
         (defun
             lse/parse-procident
             (scanner)
           "(--> procident (seq (tok-procident) ($1)))"
           (com.informatimago.rdp::with-non-terminal
               (procident scanner)
             (cond
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-procident)
                (let*
                    (($1
                       (accept scanner 'tok-procident))
                     (tok-procident $1)
                     (tok-procident.1 $1)
                     ($0 (list $1)))
                  (declare
                   (ignorable
                    $0
                    tok-procident.1
                    tok-procident
                    $1))
                  $1))
               (t
                (com.informatimago.rdp::error-unexpected-token
                 scanner
                 '(tok-procident)
                 '(-->
                   procident
                   (alt
                    (seq
                     (tok-procident)
                     ($1))))))))))
  (progn (fmakunbound
          'lse/parse-liste-identificateur)
         (defun
             lse/parse-liste-identificateur
             (scanner)
           "(--> liste-identificateur (seq (identificateur liste-identificateur-1-1) ((cons $1 $2))))"
           (com.informatimago.rdp::with-non-terminal
               (liste-identificateur scanner)
             (cond
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-identificateur)
                (let*
                    (($1
                       (lse/parse-identificateur
                        scanner))
                     (identificateur $1)
                     (identificateur.1 $1)
                     ($2
                       (lse/parse-liste-identificateur-1-1
                        scanner))
                     ($0 (list $1 $2)))
                  (declare
                   (ignorable
                    $0
                    identificateur.1
                    identificateur
                    $1
                    $2))
                  (cons $1 $2)))
               (t
                (com.informatimago.rdp::error-unexpected-token
                 scanner
                 '(tok-identificateur)
                 '(-->
                   liste-identificateur
                   (alt
                    (seq
                     (identificateur
                      liste-identificateur-1-1)
                     ((cons $1 $2)))))))))))
  (progn (fmakunbound 'lse/parse-instruction-1)
         (defun
             lse/parse-instruction-1
             (scanner)
           "(--> instruction-1 (alt ((seq (affectation-1) ($1)) (seq (appel-1) ($1)) (seq (lire-1) ($1)) (seq (afficher-1) ($1)) (seq (aller-en-1) ($1)) (seq (si-alors-sinon-1) ($1)) (seq (tok-terminer) ((list :terminer))) (seq (tok-pause) ((list :pause))) (seq (debut-fin-1) ($1)) (seq (faire-1) ($1)) (seq (retour-1) ($1)) (seq (resultat-1) ($1)) (seq (garer-1) ($1)) (seq (charger-1) ($1)) (seq (supprimer-1) ($1)) (seq (executer-1) ($1)) (seq (liberation-1) ($1)))))"
           (com.informatimago.rdp::with-non-terminal
               (instruction-1 scanner)
             (cond
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-identificateur)
                (let*
                    (($1
                       (lse/parse-affectation-1
                        scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-procident)
                (let*
                    (($1 (lse/parse-appel-1 scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-lire)
                (let*
                    (($1 (lse/parse-lire-1 scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-afficher)
                (let*
                    (($1
                       (lse/parse-afficher-1 scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-aller)
                (let*
                    (($1
                       (lse/parse-aller-en-1 scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-si)
                (let*
                    (($1
                       (lse/parse-si-alors-sinon-1
                        scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-terminer)
                (let*
                    (($1
                       (accept scanner 'tok-terminer))
                     (tok-terminer $1)
                     (tok-terminer.1 $1)
                     ($0 (list $1)))
                  (declare
                   (ignorable
                    $0
                    tok-terminer.1
                    tok-terminer
                    $1))
                  (list :terminer)))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-pause)
                (let*
                    (($1 (accept scanner 'tok-pause))
                     (tok-pause $1)
                     (tok-pause.1 $1)
                     ($0 (list $1)))
                  (declare
                   (ignorable
                    $0
                    tok-pause.1
                    tok-pause
                    $1))
                  (list :pause)))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-debut)
                (let*
                    (($1
                       (lse/parse-debut-fin-1 scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-faire)
                (let*
                    (($1 (lse/parse-faire-1 scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-retour)
                (let*
                    (($1 (lse/parse-retour-1 scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-resultat)
                (let*
                    (($1
                       (lse/parse-resultat-1 scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-garer)
                (let*
                    (($1 (lse/parse-garer-1 scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-charger)
                (let*
                    (($1 (lse/parse-charger-1 scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-supprimer)
                (let*
                    (($1
                       (lse/parse-supprimer-1 scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-executer)
                (let*
                    (($1
                       (lse/parse-executer-1 scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-liberer)
                (let*
                    (($1
                       (lse/parse-liberation-1 scanner))
                     ($0 (list $1)))
                  (declare (ignorable $0 $1))
                  $1))
               (t
                (com.informatimago.rdp::error-unexpected-token
                 scanner
                 '(tok-identificateur
                   tok-procident
                   tok-lire
                   tok-afficher
                   tok-aller
                   tok-si
                   tok-terminer
                   tok-pause
                   tok-debut
                   tok-faire
                   tok-retour
                   tok-resultat
                   tok-garer
                   tok-charger
                   tok-supprimer
                   tok-executer
                   tok-liberer)
                 '(-->
                   instruction-1
                   (alt
                    (seq (affectation-1) ($1))
                    (seq (appel-1) ($1))
                    (seq (lire-1) ($1))
                    (seq (afficher-1) ($1))
                    (seq (aller-en-1) ($1))
                    (seq (si-alors-sinon-1) ($1))
                    (seq
                     (tok-terminer)
                     ((list :terminer)))
                    (seq
                     (tok-pause)
                     ((list :pause)))
                    (seq (debut-fin-1) ($1))
                    (seq (faire-1) ($1))
                    (seq (retour-1) ($1))
                    (seq (resultat-1) ($1))
                    (seq (garer-1) ($1))
                    (seq (charger-1) ($1))
                    (seq (supprimer-1) ($1))
                    (seq (executer-1) ($1))
                    (seq
                     (liberation-1)
                     ($1))))))))))
  (progn (fmakunbound 'lse/parse-decl-tabl)
         (defun
             lse/parse-decl-tabl
             (scanner)
           "(--> decl-tabl (seq (identificateur tok-crogauche expression decl-tabl-1-1 tok-crodroite) ((list* :adecl $1 expression $4))))"
           (com.informatimago.rdp::with-non-terminal
               (decl-tabl scanner)
             (cond
               ((word-equal
                 (scanner-current-token scanner)
                 'tok-identificateur)
                (let*
                    (($1
                       (lse/parse-identificateur
                        scanner))
                     (identificateur $1)
                     (identificateur.1 $1)
                     ($2
                       (accept scanner 'tok-crogauche))
                     (tok-crogauche $2)
                     (tok-crogauche.1 $2)
                     ($3
                       (lse/parse-expression scanner))
                     (expression $3)
                     (expression.1 $3)
                     ($4
                       (lse/parse-decl-tabl-1-1
                        scanner))
                     ($5
                       (accept scanner 'tok-crodroite))
                     (tok-crodroite $5)
                     (tok-crodroite.1 $5)
                     ($0 (list $1 $2 $3 $4 $5)))
                  (declare
                   (ignorable
                    $0
                    tok-crodroite.1
                    tok-crodroite
                    expression.1
                    expression
                    tok-crogauche.1
                    tok-crogauche
                    identificateur.1
                    identificateur
                    $1
                    $2
                    $3
                    $4
                    $5))
                  (list* :adecl $1 expression $4)))
               (t
                (com.informatimago.rdp::error-unexpected-token
                 scanner
                 '(tok-identificateur)
                 '(-->
                   decl-tabl
                   (alt
                    (seq
                     (identificateur
                      tok-crogauche
                      expression
                      decl-tabl-1-1
                      tok-crodroite)
                     ((list*
                       :adecl
                       $1
                       expression
                       $4)))))))))))
  (progn (fmakunbound
          'lse/parse-liste-decl-tabl-1-1)
         (defun
             lse/parse-liste-decl-tabl-1-1
             (scanner)
           "(--> liste-decl-tabl-1-1 (alt ((seq nil ('nil)) (seq (liste-decl-tabl-1-1-1 liste-decl-tabl-1-1) ((cons $1 $2))))))"
           (com.informatimago.rdp::with-non-terminal
               (liste-decl-tabl-1-1 scanner)
             (loop :with com.informatimago.rdp::$items
                     :=
                   'nil
                   :do (cond
                         ((word-equal
                           (scanner-current-token
                            scanner)
                           'tok-virgule)
                          (push
                           (let*
                               (($1
                                  (lse/parse-liste-decl-tabl-1-1-1
                                   scanner))
                                ($2
                                  (let*
                                      (($0 (list)))
                                    (declare (ignorable $0))
                                    'nil))
                                ($0 (list $1 $2)))
                             (declare
                              (ignorable $0 $1 $2))
                             (cons $1 $2))
                           com.informatimago.rdp::$items))
                         (t (loop-finish)))
                   :finally (return
                              (reduce
                               #'append
                               (nreverse
                                com.informatimago.rdp::$items)
                               :initial-value
                               nil))))))
  (progn
    (fmakunbound
     'lse/parse-liste-decl-tabl-1-1-1)
    (defun
        lse/parse-liste-decl-tabl-1-1-1
        (scanner)
      "(--> liste-decl-tabl-1-1-1 (seq (tok-virgule decl-tabl) ($2)))"
      (com.informatimago.rdp::with-non-terminal
          (liste-decl-tabl-1-1-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-virgule)
           (let*
               (($1
                  (accept scanner 'tok-virgule))
                (tok-virgule $1)
                (tok-virgule.1 $1)
                ($2
                  (lse/parse-decl-tabl scanner))
                (decl-tabl $2)
                (decl-tabl.1 $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               decl-tabl.1
               decl-tabl
               tok-virgule.1
               tok-virgule
               $1
               $2))
             $2))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-virgule)
            '(-->
              liste-decl-tabl-1-1-1
              (alt
               (seq
                (tok-virgule decl-tabl)
                ($2))))))))))
  (progn
    (fmakunbound
     'lse/parse-liste-decl-tabl)
    (defun
        lse/parse-liste-decl-tabl
        (scanner)
      "(--> liste-decl-tabl (seq (decl-tabl liste-decl-tabl-1-1) ((cons $1 $2))))"
      (com.informatimago.rdp::with-non-terminal
          (liste-decl-tabl scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-identificateur)
           (let*
               (($1
                  (lse/parse-decl-tabl scanner))
                (decl-tabl $1)
                (decl-tabl.1 $1)
                ($2
                  (lse/parse-liste-decl-tabl-1-1
                   scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               decl-tabl.1
               decl-tabl
               $1
               $2))
             (cons $1 $2)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-identificateur)
            '(-->
              liste-decl-tabl
              (alt
               (seq
                (decl-tabl
                 liste-decl-tabl-1-1)
                ((cons $1 $2)))))))))))
  (progn
    (fmakunbound 'lse/parse-decl-1)
    (defun
        lse/parse-decl-1
        (scanner)
      "(--> decl-1 (alt ((seq (decl-tableau-1) ($1)) (seq (decl-chaine-1) ($1)))))"
      (com.informatimago.rdp::with-non-terminal
          (decl-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-tableau)
           (let*
               (($1
                  (lse/parse-decl-tableau-1
                   scanner))
                ($0 (list $1)))
             (declare (ignorable $0 $1))
             $1))
          ((word-equal
            (scanner-current-token scanner)
            'tok-chaine)
           (let*
               (($1
                  (lse/parse-decl-chaine-1
                   scanner))
                ($0 (list $1)))
             (declare (ignorable $0 $1))
             $1))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-tableau tok-chaine)
            '(-->
              decl-1
              (alt
               (seq (decl-tableau-1) ($1))
               (seq
                (decl-chaine-1)
                ($1))))))))))
  (progn
    (fmakunbound
     'lse/parse-liste-inst-ou-decl-ou-affic-1)
    (defun
        lse/parse-liste-inst-ou-decl-ou-affic-1
        (scanner)
      "(--> liste-inst-ou-decl-ou-affic-1 (alt ((seq (instruction liste-inst-ou-decl-ou-affic-1-2-1) ((cons $1 $2))) (seq (decl liste-inst-ou-decl-ou-affic-1-3-1) ((cons $1 $2))) (seq (tok-commentaire) ((list (list :commentaire $1)))) (seq (affic liste-inst-ou-decl-ou-affic-1-1-1) ((cons $1 $2))))))"
      (com.informatimago.rdp::with-non-terminal
          (liste-inst-ou-decl-ou-affic-1
           scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-identificateur
              tok-procident
              tok-lire
              tok-afficher
              tok-aller
              tok-si
              tok-terminer
              tok-pause
              tok-debut
              tok-faire
              tok-retour
              tok-resultat
              tok-garer
              tok-charger
              tok-supprimer
              tok-executer
              tok-liberer)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-instruction scanner))
                (instruction $1)
                (instruction.1 $1)
                ($2
                  (lse/parse-liste-inst-ou-decl-ou-affic-1-2-1
                   scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               instruction.1
               instruction
               $1
               $2))
             (cons $1 $2)))
          ((member
            (scanner-current-token scanner)
            '(tok-tableau tok-chaine)
            :test
            #'word-equal)
           (let*
               (($1 (lse/parse-decl scanner))
                (decl $1)
                (decl.1 $1)
                ($2
                  (lse/parse-liste-inst-ou-decl-ou-affic-1-3-1
                   scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable $0 decl.1 decl $1 $2))
             (cons $1 $2)))
          ((word-equal
            (scanner-current-token scanner)
            'tok-commentaire)
           (let*
               (($1
                  (accept
                   scanner
                   'tok-commentaire))
                (tok-commentaire $1)
                (tok-commentaire.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-commentaire.1
               tok-commentaire
               $1))
             (list (list :commentaire $1))))
          ((word-equal
            (scanner-current-token scanner)
            'tok-affic)
           (let*
               (($1 (lse/parse-affic scanner))
                (affic $1)
                (affic.1 $1)
                ($2
                  (lse/parse-liste-inst-ou-decl-ou-affic-1-1-1
                   scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               affic.1
               affic
               $1
               $2))
             (cons $1 $2)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-identificateur
              tok-procident
              tok-lire
              tok-afficher
              tok-aller
              tok-si
              tok-terminer
              tok-pause
              tok-debut
              tok-faire
              tok-retour
              tok-resultat
              tok-garer
              tok-charger
              tok-supprimer
              tok-executer
              tok-liberer
              tok-tableau
              tok-chaine
              tok-commentaire
              tok-affic)
            '(-->
              liste-inst-ou-decl-ou-affic-1
              (alt
               (seq
                (instruction
                 liste-inst-ou-decl-ou-affic-1-2-1)
                ((cons $1 $2)))
               (seq
                (decl
                 liste-inst-ou-decl-ou-affic-1-3-1)
                ((cons $1 $2)))
               (seq
                (tok-commentaire)
                ((list
                  (list :commentaire $1))))
               (seq
                (affic
                 liste-inst-ou-decl-ou-affic-1-1-1)
                ((cons $1 $2)))))))))))
  (progn
    (fmakunbound
     'lse/parse-liste-inst-ou-decl-1)
    (defun
        lse/parse-liste-inst-ou-decl-1
        (scanner)
      "(--> liste-inst-ou-decl-1 (alt ((seq (decl liste-inst-ou-decl-1-2-1) ((cons $1 $2))) (seq (tok-commentaire) ((list (list :commentaire $1)))) (seq (instruction liste-inst-ou-decl-1-1-1) ((cons $1 $2))))))"
      (com.informatimago.rdp::with-non-terminal
          (liste-inst-ou-decl-1 scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-tableau tok-chaine)
            :test
            #'word-equal)
           (let*
               (($1 (lse/parse-decl scanner))
                (decl $1)
                (decl.1 $1)
                ($2
                  (lse/parse-liste-inst-ou-decl-1-2-1
                   scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable $0 decl.1 decl $1 $2))
             (cons $1 $2)))
          ((word-equal
            (scanner-current-token scanner)
            'tok-commentaire)
           (let*
               (($1
                  (accept
                   scanner
                   'tok-commentaire))
                (tok-commentaire $1)
                (tok-commentaire.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-commentaire.1
               tok-commentaire
               $1))
             (list (list :commentaire $1))))
          ((member
            (scanner-current-token scanner)
            '(tok-identificateur
              tok-procident
              tok-lire
              tok-afficher
              tok-aller
              tok-si
              tok-terminer
              tok-pause
              tok-debut
              tok-faire
              tok-retour
              tok-resultat
              tok-garer
              tok-charger
              tok-supprimer
              tok-executer
              tok-liberer)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-instruction scanner))
                (instruction $1)
                (instruction.1 $1)
                ($2
                  (lse/parse-liste-inst-ou-decl-1-1-1
                   scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               instruction.1
               instruction
               $1
               $2))
             (cons $1 $2)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-tableau
              tok-chaine
              tok-commentaire
              tok-identificateur
              tok-procident
              tok-lire
              tok-afficher
              tok-aller
              tok-si
              tok-terminer
              tok-pause
              tok-debut
              tok-faire
              tok-retour
              tok-resultat
              tok-garer
              tok-charger
              tok-supprimer
              tok-executer
              tok-liberer)
            '(-->
              liste-inst-ou-decl-1
              (alt
               (seq
                (decl
                 liste-inst-ou-decl-1-2-1)
                ((cons $1 $2)))
               (seq
                (tok-commentaire)
                ((list
                  (list :commentaire $1))))
               (seq
                (instruction
                 liste-inst-ou-decl-1-1-1)
                ((cons $1 $2)))))))))))
  (progn
    (fmakunbound 'lse/parse-decl-local)
    (defun
        lse/parse-decl-local
        (scanner)
      "(--> decl-local (seq (tok-local liste-identificateur) ($2)))"
      (com.informatimago.rdp::with-non-terminal
          (decl-local scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-local)
           (let*
               (($1 (accept scanner 'tok-local))
                (tok-local $1)
                (tok-local.1 $1)
                ($2
                  (lse/parse-liste-identificateur
                   scanner))
                (liste-identificateur $2)
                (liste-identificateur.1 $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               liste-identificateur.1
               liste-identificateur
               tok-local.1
               tok-local
               $1
               $2))
             $2))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-local)
            '(-->
              decl-local
              (alt
               (seq
                (tok-local
                 liste-identificateur)
                ($2))))))))))
  (progn
    (fmakunbound 'lse/parse-debut-1-1)
    (defun
        lse/parse-debut-1-1
        (scanner)
      "(--> debut-1-1 (alt ((seq (liste-inst-ou-decl-ou-affic) ((cons :liste-instructions $1))) (seq (ligne-programme) ($1)))))"
      (com.informatimago.rdp::with-non-terminal
          (debut-1-1 scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-identificateur
              tok-procident
              tok-lire
              tok-afficher
              tok-aller
              tok-si
              tok-terminer
              tok-pause
              tok-debut
              tok-faire
              tok-retour
              tok-resultat
              tok-garer
              tok-charger
              tok-supprimer
              tok-executer
              tok-liberer
              tok-tableau
              tok-chaine
              tok-commentaire
              tok-affic)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-liste-inst-ou-decl-ou-affic
                   scanner))
                (liste-inst-ou-decl-ou-affic $1)
                (liste-inst-ou-decl-ou-affic.1
                  $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               liste-inst-ou-decl-ou-affic.1
               liste-inst-ou-decl-ou-affic
               $1))
             (cons :liste-instructions $1)))
          ((word-equal
            (scanner-current-token scanner)
            'tok-numero)
           (let*
               (($1
                  (lse/parse-ligne-programme
                   scanner))
                (ligne-programme $1)
                (ligne-programme.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               ligne-programme.1
               ligne-programme
               $1))
             $1))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-identificateur
              tok-procident
              tok-lire
              tok-afficher
              tok-aller
              tok-si
              tok-terminer
              tok-pause
              tok-debut
              tok-faire
              tok-retour
              tok-resultat
              tok-garer
              tok-charger
              tok-supprimer
              tok-executer
              tok-liberer
              tok-tableau
              tok-chaine
              tok-commentaire
              tok-affic
              tok-numero)
            '(-->
              debut-1-1
              (alt
               (seq
                (liste-inst-ou-decl-ou-affic)
                ((cons
                  :liste-instructions
                  $1)))
               (seq
                (ligne-programme)
                ($1))))))))))
  (progn
    (fmakunbound 'lse/parse-debut-1)
    (defun
        lse/parse-debut-1
        (scanner)
      "(--> debut-1 (alt ((seq nil ('nil)) (seq (debut-1-1) ($1)))))"
      (com.informatimago.rdp::with-non-terminal
          (debut-1 scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-numero
              tok-affic
              tok-commentaire
              tok-chaine
              tok-tableau
              tok-liberer
              tok-executer
              tok-supprimer
              tok-charger
              tok-garer
              tok-resultat
              tok-retour
              tok-faire
              tok-debut
              tok-pause
              tok-terminer
              tok-si
              tok-aller
              tok-afficher
              tok-lire
              tok-procident
              tok-identificateur)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-debut-1-1 scanner))
                ($0 (list $1)))
             (declare (ignorable $0 $1))
             $1))))))
  (progn
    (fmakunbound
     'lse/parse-ligne-programme)
    (defun
        lse/parse-ligne-programme
        (scanner)
      "(--> ligne-programme (seq (ligne-programme-1-1 ligne-programme-1-2) ((list* :ligne-programme $1 $2))))"
      (com.informatimago.rdp::with-non-terminal
          (ligne-programme scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-numero)
           (let*
               (($1
                  (lse/parse-ligne-programme-1-1
                   scanner))
                ($2
                  (lse/parse-ligne-programme-1-2
                   scanner))
                ($0 (list $1 $2)))
             (declare (ignorable $0 $1 $2))
             (list* :ligne-programme $1 $2)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-numero)
            '(-->
              ligne-programme
              (alt
               (seq
                (ligne-programme-1-1
                 ligne-programme-1-2)
                ((list*
                  :ligne-programme
                  $1
                  $2)))))))))))
  (progn
    (fmakunbound
     'lse/parse-liste-inst-ou-decl-ou-affic)
    (defun
        lse/parse-liste-inst-ou-decl-ou-affic
        (scanner)
      "(--> liste-inst-ou-decl-ou-affic (seq (liste-inst-ou-decl-ou-affic-1) ($1)))"
      (com.informatimago.rdp::with-non-terminal
          (liste-inst-ou-decl-ou-affic scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-affic
              tok-commentaire
              tok-chaine
              tok-tableau
              tok-liberer
              tok-executer
              tok-supprimer
              tok-charger
              tok-garer
              tok-resultat
              tok-retour
              tok-faire
              tok-debut
              tok-pause
              tok-terminer
              tok-si
              tok-aller
              tok-afficher
              tok-lire
              tok-procident
              tok-identificateur)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-liste-inst-ou-decl-ou-affic-1
                   scanner))
                ($0 (list $1)))
             (declare (ignorable $0 $1))
             $1))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-affic
              tok-commentaire
              tok-chaine
              tok-tableau
              tok-liberer
              tok-executer
              tok-supprimer
              tok-charger
              tok-garer
              tok-resultat
              tok-retour
              tok-faire
              tok-debut
              tok-pause
              tok-terminer
              tok-si
              tok-aller
              tok-afficher
              tok-lire
              tok-procident
              tok-identificateur)
            '(-->
              liste-inst-ou-decl-ou-affic
              (alt
               (seq
                (liste-inst-ou-decl-ou-affic-1)
                ($1))))))))))
  (progn
    (fmakunbound
     'lse/parse-ligne-programme-1-2)
    (defun
        lse/parse-ligne-programme-1-2
        (scanner)
      "(--> ligne-programme-1-2 (alt ((seq (decl-procedure ligne-programme-1-2-1-1) ((cons $1 $2))) (seq (liste-inst-ou-decl) ($1)))))"
      (com.informatimago.rdp::with-non-terminal
          (ligne-programme-1-2 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-procedure)
           (let*
               (($1
                  (lse/parse-decl-procedure
                   scanner))
                (decl-procedure $1)
                (decl-procedure.1 $1)
                ($2
                  (lse/parse-ligne-programme-1-2-1-1
                   scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               decl-procedure.1
               decl-procedure
               $1
               $2))
             (cons $1 $2)))
          ((member
            (scanner-current-token scanner)
            '(tok-tableau
              tok-chaine
              tok-commentaire
              tok-identificateur
              tok-procident
              tok-lire
              tok-afficher
              tok-aller
              tok-si
              tok-terminer
              tok-pause
              tok-debut
              tok-faire
              tok-retour
              tok-resultat
              tok-garer
              tok-charger
              tok-supprimer
              tok-executer
              tok-liberer)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-liste-inst-ou-decl
                   scanner))
                (liste-inst-ou-decl $1)
                (liste-inst-ou-decl.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               liste-inst-ou-decl.1
               liste-inst-ou-decl
               $1))
             $1))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-procedure
              tok-tableau
              tok-chaine
              tok-commentaire
              tok-identificateur
              tok-procident
              tok-lire
              tok-afficher
              tok-aller
              tok-si
              tok-terminer
              tok-pause
              tok-debut
              tok-faire
              tok-retour
              tok-resultat
              tok-garer
              tok-charger
              tok-supprimer
              tok-executer
              tok-liberer)
            '(-->
              ligne-programme-1-2
              (alt
               (seq
                (decl-procedure
                 ligne-programme-1-2-1-1)
                ((cons $1 $2)))
               (seq
                (liste-inst-ou-decl)
                ($1))))))))))
  (progn
    (fmakunbound
     'lse/parse-ligne-programme-1-1)
    (defun
        lse/parse-ligne-programme-1-1
        (scanner)
      "(--> ligne-programme-1-1 (seq (tok-numero) ((progn (setf (scanner-line *scanner*) (numero-valeur tok-numero)) tok-numero))))"
      (com.informatimago.rdp::with-non-terminal
          (ligne-programme-1-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-numero)
           (let*
               (($1 (accept scanner 'tok-numero))
                (tok-numero $1)
                (tok-numero.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-numero.1
               tok-numero
               $1))
             (progn
               (setf
                (scanner-line *scanner*)
                (numero-valeur tok-numero))
               tok-numero)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-numero)
            '(-->
              ligne-programme-1-1
              (alt
               (seq
                (tok-numero)
                ((progn
                   (setf
                    (scanner-line *scanner*)
                    (numero-valeur tok-numero))
                   tok-numero)))))))))))
  (progn
    (fmakunbound
     'lse/parse-ligne-programme-1-2-1-1)
    (defun
        lse/parse-ligne-programme-1-2-1-1
        (scanner)
      "(--> ligne-programme-1-2-1-1 (alt ((seq (tok-ptvirg liste-inst-ou-decl) ($2)) (seq nil ('nil)))))"
      (com.informatimago.rdp::with-non-terminal
          (ligne-programme-1-2-1-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-ptvirg)
           (let*
               (($1 (accept scanner 'tok-ptvirg))
                (tok-ptvirg $1)
                (tok-ptvirg.1 $1)
                ($2
                  (lse/parse-liste-inst-ou-decl
                   scanner))
                (liste-inst-ou-decl $2)
                (liste-inst-ou-decl.1 $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               liste-inst-ou-decl.1
               liste-inst-ou-decl
               tok-ptvirg.1
               tok-ptvirg
               $1
               $2))
             $2))))))
  (progn
    (fmakunbound 'lse/parse-decl-procedure)
    (defun
        lse/parse-decl-procedure
        (scanner)
      "(--> decl-procedure (seq (tok-procedure procident tok-pargauche decl-procedure-1-1) ((list* :decl-procedure $2 $4))))"
      (com.informatimago.rdp::with-non-terminal
          (decl-procedure scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-procedure)
           (let*
               (($1
                  (accept scanner 'tok-procedure))
                (tok-procedure $1)
                (tok-procedure.1 $1)
                ($2
                  (lse/parse-procident scanner))
                (procident $2)
                (procident.1 $2)
                ($3
                  (accept scanner 'tok-pargauche))
                (tok-pargauche $3)
                (tok-pargauche.1 $3)
                ($4
                  (lse/parse-decl-procedure-1-1
                   scanner))
                ($0 (list $1 $2 $3 $4)))
             (declare
              (ignorable
               $0
               tok-pargauche.1
               tok-pargauche
               procident.1
               procident
               tok-procedure.1
               tok-procedure
               $1
               $2
               $3
               $4))
             (list* :decl-procedure $2 $4)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-procedure)
            '(-->
              decl-procedure
              (alt
               (seq
                (tok-procedure
                 procident
                 tok-pargauche
                 decl-procedure-1-1)
                ((list*
                  :decl-procedure
                  $2
                  $4)))))))))))
  (progn
    (fmakunbound
     'lse/parse-decl-procedure-1-1)
    (defun
        lse/parse-decl-procedure-1-1
        (scanner)
      "(--> decl-procedure-1-1 (alt ((seq (liste-identificateur tok-pardroite decl-procedure-1-1-2-1) ((list $1 $3))) (seq (tok-pardroite decl-procedure-1-1-1-1) ((list nil $2))))))"
      (com.informatimago.rdp::with-non-terminal
          (decl-procedure-1-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-identificateur)
           (let*
               (($1
                  (lse/parse-liste-identificateur
                   scanner))
                (liste-identificateur $1)
                (liste-identificateur.1 $1)
                ($2
                  (accept scanner 'tok-pardroite))
                (tok-pardroite $2)
                (tok-pardroite.1 $2)
                ($3
                  (lse/parse-decl-procedure-1-1-2-1
                   scanner))
                ($0 (list $1 $2 $3)))
             (declare
              (ignorable
               $0
               tok-pardroite.1
               tok-pardroite
               liste-identificateur.1
               liste-identificateur
               $1
               $2
               $3))
             (list $1 $3)))
          ((word-equal
            (scanner-current-token scanner)
            'tok-pardroite)
           (let*
               (($1
                  (accept scanner 'tok-pardroite))
                (tok-pardroite $1)
                (tok-pardroite.1 $1)
                ($2
                  (lse/parse-decl-procedure-1-1-1-1
                   scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               tok-pardroite.1
               tok-pardroite
               $1
               $2))
             (list nil $2)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-identificateur
              tok-pardroite)
            '(-->
              decl-procedure-1-1
              (alt
               (seq
                (liste-identificateur
                 tok-pardroite
                 decl-procedure-1-1-2-1)
                ((list $1 $3)))
               (seq
                (tok-pardroite
                 decl-procedure-1-1-1-1)
                ((list nil $2)))))))))))
  (progn
    (fmakunbound
     'lse/parse-decl-procedure-1-1-1-1)
    (defun
        lse/parse-decl-procedure-1-1-1-1
        (scanner)
      "(--> decl-procedure-1-1-1-1 (alt ((seq nil ('nil)) (seq (decl-local) (decl-local)))))"
      (com.informatimago.rdp::with-non-terminal
          (decl-procedure-1-1-1-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-local)
           (let*
               (($1
                  (lse/parse-decl-local scanner))
                (decl-local $1)
                (decl-local.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               decl-local.1
               decl-local
               $1))
             decl-local))))))
  (progn
    (fmakunbound
     'lse/parse-decl-procedure-1-1-2-1)
    (defun
        lse/parse-decl-procedure-1-1-2-1
        (scanner)
      "(--> decl-procedure-1-1-2-1 (alt ((seq nil ('nil)) (seq (decl-local) (decl-local)))))"
      (com.informatimago.rdp::with-non-terminal
          (decl-procedure-1-1-2-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-local)
           (let*
               (($1
                  (lse/parse-decl-local scanner))
                (decl-local $1)
                (decl-local.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               decl-local.1
               decl-local
               $1))
             decl-local))))))
  (progn
    (fmakunbound
     'lse/parse-liste-inst-ou-decl-1-1-1)
    (defun
        lse/parse-liste-inst-ou-decl-1-1-1
        (scanner)
      "(--> liste-inst-ou-decl-1-1-1 (alt ((seq (tok-ptvirg liste-inst-ou-decl) (liste-inst-ou-decl)) (seq nil ('nil)))))"
      (com.informatimago.rdp::with-non-terminal
          (liste-inst-ou-decl-1-1-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-ptvirg)
           (let*
               (($1 (accept scanner 'tok-ptvirg))
                (tok-ptvirg $1)
                (tok-ptvirg.1 $1)
                ($2
                  (lse/parse-liste-inst-ou-decl
                   scanner))
                (liste-inst-ou-decl $2)
                (liste-inst-ou-decl.1 $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               liste-inst-ou-decl.1
               liste-inst-ou-decl
               tok-ptvirg.1
               tok-ptvirg
               $1
               $2))
             liste-inst-ou-decl))))))
  (progn
    (fmakunbound
     'lse/parse-liste-inst-ou-decl-1-2-1)
    (defun
        lse/parse-liste-inst-ou-decl-1-2-1
        (scanner)
      "(--> liste-inst-ou-decl-1-2-1 (alt ((seq (tok-ptvirg liste-inst-ou-decl) (liste-inst-ou-decl)) (seq nil ('nil)))))"
      (com.informatimago.rdp::with-non-terminal
          (liste-inst-ou-decl-1-2-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-ptvirg)
           (let*
               (($1 (accept scanner 'tok-ptvirg))
                (tok-ptvirg $1)
                (tok-ptvirg.1 $1)
                ($2
                  (lse/parse-liste-inst-ou-decl
                   scanner))
                (liste-inst-ou-decl $2)
                (liste-inst-ou-decl.1 $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               liste-inst-ou-decl.1
               liste-inst-ou-decl
               tok-ptvirg.1
               tok-ptvirg
               $1
               $2))
             liste-inst-ou-decl))))))
  (progn
    (fmakunbound 'lse/parse-decl)
    (defun
        lse/parse-decl
        (scanner)
      "(--> decl (seq (decl-1) ($1)))"
      (com.informatimago.rdp::with-non-terminal
          (decl scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-chaine tok-tableau)
            :test
            #'word-equal)
           (let*
               (($1 (lse/parse-decl-1 scanner))
                ($0 (list $1)))
             (declare (ignorable $0 $1))
             $1))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-chaine tok-tableau)
            '(-->
              decl
              (alt (seq (decl-1) ($1))))))))))
  (progn
    (fmakunbound
     'lse/parse-liste-inst-ou-decl-ou-affic-1-1-1)
    (defun
        lse/parse-liste-inst-ou-decl-ou-affic-1-1-1
        (scanner)
      "(--> liste-inst-ou-decl-ou-affic-1-1-1 (alt ((seq (tok-ptvirg liste-inst-ou-decl-ou-affic) ($2)) (seq nil ('nil)))))"
      (com.informatimago.rdp::with-non-terminal
          (liste-inst-ou-decl-ou-affic-1-1-1
           scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-ptvirg)
           (let*
               (($1 (accept scanner 'tok-ptvirg))
                (tok-ptvirg $1)
                (tok-ptvirg.1 $1)
                ($2
                  (lse/parse-liste-inst-ou-decl-ou-affic
                   scanner))
                (liste-inst-ou-decl-ou-affic $2)
                (liste-inst-ou-decl-ou-affic.1
                  $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               liste-inst-ou-decl-ou-affic.1
               liste-inst-ou-decl-ou-affic
               tok-ptvirg.1
               tok-ptvirg
               $1
               $2))
             $2))))))
  (progn
    (fmakunbound 'lse/parse-affic)
    (defun
        lse/parse-affic
        (scanner)
      "(--> affic (seq (tok-affic affic-1-1) ($2)))"
      (com.informatimago.rdp::with-non-terminal
          (affic scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-affic)
           (let*
               (($1 (accept scanner 'tok-affic))
                (tok-affic $1)
                (tok-affic.1 $1)
                ($2
                  (lse/parse-affic-1-1 scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               tok-affic.1
               tok-affic
               $1
               $2))
             $2))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-affic)
            '(-->
              affic
              (alt
               (seq
                (tok-affic affic-1-1)
                ($2))))))))))
  (progn
    (fmakunbound
     'lse/parse-liste-inst-ou-decl-ou-affic-1-3-1)
    (defun
        lse/parse-liste-inst-ou-decl-ou-affic-1-3-1
        (scanner)
      "(--> liste-inst-ou-decl-ou-affic-1-3-1 (alt ((seq (tok-ptvirg liste-inst-ou-decl-ou-affic) ($2)) (seq nil ('nil)))))"
      (com.informatimago.rdp::with-non-terminal
          (liste-inst-ou-decl-ou-affic-1-3-1
           scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-ptvirg)
           (let*
               (($1 (accept scanner 'tok-ptvirg))
                (tok-ptvirg $1)
                (tok-ptvirg.1 $1)
                ($2
                  (lse/parse-liste-inst-ou-decl-ou-affic
                   scanner))
                (liste-inst-ou-decl-ou-affic $2)
                (liste-inst-ou-decl-ou-affic.1
                  $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               liste-inst-ou-decl-ou-affic.1
               liste-inst-ou-decl-ou-affic
               tok-ptvirg.1
               tok-ptvirg
               $1
               $2))
             $2))))))
  (progn
    (fmakunbound
     'lse/parse-liste-inst-ou-decl-ou-affic-1-2-1)
    (defun
        lse/parse-liste-inst-ou-decl-ou-affic-1-2-1
        (scanner)
      "(--> liste-inst-ou-decl-ou-affic-1-2-1 (alt ((seq (tok-ptvirg liste-inst-ou-decl-ou-affic) ($2)) (seq nil ('nil)))))"
      (com.informatimago.rdp::with-non-terminal
          (liste-inst-ou-decl-ou-affic-1-2-1
           scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-ptvirg)
           (let*
               (($1 (accept scanner 'tok-ptvirg))
                (tok-ptvirg $1)
                (tok-ptvirg.1 $1)
                ($2
                  (lse/parse-liste-inst-ou-decl-ou-affic
                   scanner))
                (liste-inst-ou-decl-ou-affic $2)
                (liste-inst-ou-decl-ou-affic.1
                  $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               liste-inst-ou-decl-ou-affic.1
               liste-inst-ou-decl-ou-affic
               tok-ptvirg.1
               tok-ptvirg
               $1
               $2))
             $2))))))
  (progn
    (fmakunbound 'lse/parse-decl-chaine-1)
    (defun
        lse/parse-decl-chaine-1
        (scanner)
      "(--> decl-chaine-1 (seq (tok-chaine liste-identificateur) ((cons :chaine $2))))"
      (com.informatimago.rdp::with-non-terminal
          (decl-chaine-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-chaine)
           (let*
               (($1 (accept scanner 'tok-chaine))
                (tok-chaine $1)
                (tok-chaine.1 $1)
                ($2
                  (lse/parse-liste-identificateur
                   scanner))
                (liste-identificateur $2)
                (liste-identificateur.1 $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               liste-identificateur.1
               liste-identificateur
               tok-chaine.1
               tok-chaine
               $1
               $2))
             (cons :chaine $2)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-chaine)
            '(-->
              decl-chaine-1
              (alt
               (seq
                (tok-chaine
                 liste-identificateur)
                ((cons :chaine $2)))))))))))
  (progn
    (fmakunbound 'lse/parse-decl-tableau-1)
    (defun
        lse/parse-decl-tableau-1
        (scanner)
      "(--> decl-tableau-1 (seq (tok-tableau liste-decl-tabl) ((cons :tableau $2))))"
      (com.informatimago.rdp::with-non-terminal
          (decl-tableau-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-tableau)
           (let*
               (($1
                  (accept scanner 'tok-tableau))
                (tok-tableau $1)
                (tok-tableau.1 $1)
                ($2
                  (lse/parse-liste-decl-tabl
                   scanner))
                (liste-decl-tabl $2)
                (liste-decl-tabl.1 $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               liste-decl-tabl.1
               liste-decl-tabl
               tok-tableau.1
               tok-tableau
               $1
               $2))
             (cons :tableau $2)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-tableau)
            '(-->
              decl-tableau-1
              (alt
               (seq
                (tok-tableau liste-decl-tabl)
                ((cons :tableau $2)))))))))))
  (progn
    (fmakunbound 'lse/parse-decl-tabl-1-1)
    (defun
        lse/parse-decl-tabl-1-1
        (scanner)
      "(--> decl-tabl-1-1 (alt ((seq (tok-virgule expression) (expression)) (seq nil ('nil)))))"
      (com.informatimago.rdp::with-non-terminal
          (decl-tabl-1-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-virgule)
           (let*
               (($1
                  (accept scanner 'tok-virgule))
                (tok-virgule $1)
                (tok-virgule.1 $1)
                ($2
                  (lse/parse-expression scanner))
                (expression $2)
                (expression.1 $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               expression.1
               expression
               tok-virgule.1
               tok-virgule
               $1
               $2))
             expression))))))
  (progn
    (fmakunbound 'lse/parse-liberation-1)
    (defun
        lse/parse-liberation-1
        (scanner)
      "(--> liberation-1 (seq (tok-liberer liste-identificateur) ((cons :liberer $2))))"
      (com.informatimago.rdp::with-non-terminal
          (liberation-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-liberer)
           (let*
               (($1
                  (accept scanner 'tok-liberer))
                (tok-liberer $1)
                (tok-liberer.1 $1)
                ($2
                  (lse/parse-liste-identificateur
                   scanner))
                (liste-identificateur $2)
                (liste-identificateur.1 $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               liste-identificateur.1
               liste-identificateur
               tok-liberer.1
               tok-liberer
               $1
               $2))
             (cons :liberer $2)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-liberer)
            '(-->
              liberation-1
              (alt
               (seq
                (tok-liberer
                 liste-identificateur)
                ((cons :liberer $2)))))))))))
  (progn
    (fmakunbound 'lse/parse-executer-1)
    (defun
        lse/parse-executer-1
        (scanner)
      "(--> executer-1 (seq (tok-executer expression executer-1-1) ((list* :executer $2 $3))))"
      (com.informatimago.rdp::with-non-terminal
          (executer-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-executer)
           (let*
               (($1
                  (accept scanner 'tok-executer))
                (tok-executer $1)
                (tok-executer.1 $1)
                ($2
                  (lse/parse-expression scanner))
                (expression $2)
                (expression.1 $2)
                ($3
                  (lse/parse-executer-1-1
                   scanner))
                ($0 (list $1 $2 $3)))
             (declare
              (ignorable
               $0
               expression.1
               expression
               tok-executer.1
               tok-executer
               $1
               $2
               $3))
             (list* :executer $2 $3)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-executer)
            '(-->
              executer-1
              (alt
               (seq
                (tok-executer
                 expression
                 executer-1-1)
                ((list*
                  :executer
                  $2
                  $3)))))))))))
  (progn
    (fmakunbound 'lse/parse-supprimer-1)
    (defun
        lse/parse-supprimer-1
        (scanner)
      "(--> supprimer-1 (seq (tok-supprimer expression supprimer-1-1) ((list* :supprimer expression $3))))"
      (com.informatimago.rdp::with-non-terminal
          (supprimer-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-supprimer)
           (let*
               (($1
                  (accept scanner 'tok-supprimer))
                (tok-supprimer $1)
                (tok-supprimer.1 $1)
                ($2
                  (lse/parse-expression scanner))
                (expression $2)
                (expression.1 $2)
                ($3
                  (lse/parse-supprimer-1-1
                   scanner))
                ($0 (list $1 $2 $3)))
             (declare
              (ignorable
               $0
               expression.1
               expression
               tok-supprimer.1
               tok-supprimer
               $1
               $2
               $3))
             (list* :supprimer expression $3)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-supprimer)
            '(-->
              supprimer-1
              (alt
               (seq
                (tok-supprimer
                 expression
                 supprimer-1-1)
                ((list*
                  :supprimer
                  expression
                  $3)))))))))))
  (progn
    (fmakunbound 'lse/parse-charger-1)
    (defun
        lse/parse-charger-1
        (scanner)
      "(--> charger-1 (seq (tok-charger identificateur tok-virgule expression tok-virgule expression charger-1-1) ((list :charger $2 $4 $6 $7))))"
      (com.informatimago.rdp::with-non-terminal
          (charger-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-charger)
           (let*
               (($1
                  (accept scanner 'tok-charger))
                (tok-charger $1)
                (tok-charger.1 $1)
                ($2
                  (lse/parse-identificateur
                   scanner))
                (identificateur $2)
                (identificateur.1 $2)
                ($3
                  (accept scanner 'tok-virgule))
                (tok-virgule $3)
                (tok-virgule.1 $3)
                ($4
                  (lse/parse-expression scanner))
                (expression $4)
                (expression.1 $4)
                ($5
                  (accept scanner 'tok-virgule))
                (tok-virgule.2 $5)
                ($6
                  (lse/parse-expression scanner))
                (expression.2 $6)
                ($7
                  (lse/parse-charger-1-1 scanner))
                ($0 (list $1 $2 $3 $4 $5 $6 $7)))
             (declare
              (ignorable
               $0
               expression.2
               tok-virgule.2
               expression.1
               expression
               tok-virgule.1
               tok-virgule
               identificateur.1
               identificateur
               tok-charger.1
               tok-charger
               $1
               $2
               $3
               $4
               $5
               $6
               $7))
             (list :charger $2 $4 $6 $7)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-charger)
            '(-->
              charger-1
              (alt
               (seq
                (tok-charger
                 identificateur
                 tok-virgule
                 expression
                 tok-virgule
                 expression
                 charger-1-1)
                ((list
                  :charger
                  $2
                  $4
                  $6
                  $7)))))))))))
  (progn
    (fmakunbound 'lse/parse-garer-1)
    (defun
        lse/parse-garer-1
        (scanner)
      "(--> garer-1 (seq (tok-garer identificateur tok-virgule expression tok-virgule expression) ((list :garer $2 $4 $6))))"
      (com.informatimago.rdp::with-non-terminal
          (garer-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-garer)
           (let*
               (($1 (accept scanner 'tok-garer))
                (tok-garer $1)
                (tok-garer.1 $1)
                ($2
                  (lse/parse-identificateur
                   scanner))
                (identificateur $2)
                (identificateur.1 $2)
                ($3
                  (accept scanner 'tok-virgule))
                (tok-virgule $3)
                (tok-virgule.1 $3)
                ($4
                  (lse/parse-expression scanner))
                (expression $4)
                (expression.1 $4)
                ($5
                  (accept scanner 'tok-virgule))
                (tok-virgule.2 $5)
                ($6
                  (lse/parse-expression scanner))
                (expression.2 $6)
                ($0 (list $1 $2 $3 $4 $5 $6)))
             (declare
              (ignorable
               $0
               expression.2
               tok-virgule.2
               expression.1
               expression
               tok-virgule.1
               tok-virgule
               identificateur.1
               identificateur
               tok-garer.1
               tok-garer
               $1
               $2
               $3
               $4
               $5
               $6))
             (list :garer $2 $4 $6)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-garer)
            '(-->
              garer-1
              (alt
               (seq
                (tok-garer
                 identificateur
                 tok-virgule
                 expression
                 tok-virgule
                 expression)
                ((list
                  :garer
                  $2
                  $4
                  $6)))))))))))
  (progn
    (fmakunbound 'lse/parse-resultat-1)
    (defun
        lse/parse-resultat-1
        (scanner)
      "(--> resultat-1 (seq (tok-resultat expression) ((list :resultat expression))))"
      (com.informatimago.rdp::with-non-terminal
          (resultat-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-resultat)
           (let*
               (($1
                  (accept scanner 'tok-resultat))
                (tok-resultat $1)
                (tok-resultat.1 $1)
                ($2
                  (lse/parse-expression scanner))
                (expression $2)
                (expression.1 $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               expression.1
               expression
               tok-resultat.1
               tok-resultat
               $1
               $2))
             (list :resultat expression)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-resultat)
            '(-->
              resultat-1
              (alt
               (seq
                (tok-resultat expression)
                ((list
                  :resultat
                  expression)))))))))))
  (progn
    (fmakunbound 'lse/parse-retour-1)
    (defun
        lse/parse-retour-1
        (scanner)
      "(--> retour-1 (seq (tok-retour retour-1-1) ((if $2 (list :retour-en $2) (list :retour)))))"
      (com.informatimago.rdp::with-non-terminal
          (retour-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-retour)
           (let*
               (($1 (accept scanner 'tok-retour))
                (tok-retour $1)
                (tok-retour.1 $1)
                ($2
                  (lse/parse-retour-1-1 scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               tok-retour.1
               tok-retour
               $1
               $2))
             (if
              $2
              (list :retour-en $2)
              (list :retour))))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-retour)
            '(-->
              retour-1
              (alt
               (seq
                (tok-retour retour-1-1)
                ((if
                  $2
                  (list :retour-en $2)
                  (list :retour))))))))))))
  (progn
    (fmakunbound 'lse/parse-faire-1)
    (defun
        lse/parse-faire-1
        (scanner)
      "(--> faire-1 (seq (tok-faire expression tok-pour identificateur tok-fleche expression faire-1-1 faire-1-2) ((list (first $8) $2 $4 $6 (or $7 (make-instance 'tok-numero :kind 'tok-numero :text \"1\" :column (token-column $6) :line (token-line $6))) (second $8)))))"
      (com.informatimago.rdp::with-non-terminal
          (faire-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-faire)
           (let*
               (($1 (accept scanner 'tok-faire))
                (tok-faire $1)
                (tok-faire.1 $1)
                ($2
                  (lse/parse-expression scanner))
                (expression $2)
                (expression.1 $2)
                ($3 (accept scanner 'tok-pour))
                (tok-pour $3)
                (tok-pour.1 $3)
                ($4
                  (lse/parse-identificateur
                   scanner))
                (identificateur $4)
                (identificateur.1 $4)
                ($5 (accept scanner 'tok-fleche))
                (tok-fleche $5)
                (tok-fleche.1 $5)
                ($6
                  (lse/parse-expression scanner))
                (expression.2 $6)
                ($7
                  (lse/parse-faire-1-1 scanner))
                ($8
                  (lse/parse-faire-1-2 scanner))
                ($0
                  (list $1 $2 $3 $4 $5 $6 $7 $8)))
             (declare
              (ignorable
               $0
               expression.2
               tok-fleche.1
               tok-fleche
               identificateur.1
               identificateur
               tok-pour.1
               tok-pour
               expression.1
               expression
               tok-faire.1
               tok-faire
               $1
               $2
               $3
               $4
               $5
               $6
               $7
               $8))
             (list
              (first $8)
              $2
              $4
              $6
              (or
               $7
               (make-instance
                'tok-numero
                :kind
                'tok-numero
                :text
                "1"
                :column
                (token-column $6)
                :line
                (token-line $6)))
              (second $8))))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-faire)
            '(-->
              faire-1
              (alt
               (seq
                (tok-faire
                 expression
                 tok-pour
                 identificateur
                 tok-fleche
                 expression
                 faire-1-1
                 faire-1-2)
                ((list
                  (first $8)
                  $2
                  $4
                  $6
                  (or
                   $7
                   (make-instance
                    'tok-numero
                    :kind
                    'tok-numero
                    :text
                    "1"
                    :column
                    (token-column $6)
                    :line
                    (token-line $6)))
                  (second $8))))))))))))
  (progn
    (fmakunbound 'lse/parse-debut-fin-1)
    (defun
        lse/parse-debut-fin-1
        (scanner)
      "(--> debut-fin-1 (seq (tok-debut liste-inst-ou-decl tok-fin) ((cons :debut $2))))"
      (com.informatimago.rdp::with-non-terminal
          (debut-fin-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-debut)
           (let*
               (($1 (accept scanner 'tok-debut))
                (tok-debut $1)
                (tok-debut.1 $1)
                ($2
                  (lse/parse-liste-inst-ou-decl
                   scanner))
                (liste-inst-ou-decl $2)
                (liste-inst-ou-decl.1 $2)
                ($3 (accept scanner 'tok-fin))
                (tok-fin $3)
                (tok-fin.1 $3)
                ($0 (list $1 $2 $3)))
             (declare
              (ignorable
               $0
               tok-fin.1
               tok-fin
               liste-inst-ou-decl.1
               liste-inst-ou-decl
               tok-debut.1
               tok-debut
               $1
               $2
               $3))
             (cons :debut $2)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-debut)
            '(-->
              debut-fin-1
              (alt
               (seq
                (tok-debut
                 liste-inst-ou-decl
                 tok-fin)
                ((cons :debut $2)))))))))))
  (progn
    (fmakunbound
     'lse/parse-si-alors-sinon-1)
    (defun
        lse/parse-si-alors-sinon-1
        (scanner)
      "(--> si-alors-sinon-1 (seq (tok-si disjonction tok-alors instruction si-alors-sinon-1-1) ((if $5 (list :si $2 $4 $5) (list :si $2 $4)))))"
      (com.informatimago.rdp::with-non-terminal
          (si-alors-sinon-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-si)
           (let*
               (($1 (accept scanner 'tok-si))
                (tok-si $1)
                (tok-si.1 $1)
                ($2
                  (lse/parse-disjonction scanner))
                (disjonction $2)
                (disjonction.1 $2)
                ($3 (accept scanner 'tok-alors))
                (tok-alors $3)
                (tok-alors.1 $3)
                ($4
                  (lse/parse-instruction scanner))
                (instruction $4)
                (instruction.1 $4)
                ($5
                  (lse/parse-si-alors-sinon-1-1
                   scanner))
                ($0 (list $1 $2 $3 $4 $5)))
             (declare
              (ignorable
               $0
               instruction.1
               instruction
               tok-alors.1
               tok-alors
               disjonction.1
               disjonction
               tok-si.1
               tok-si
               $1
               $2
               $3
               $4
               $5))
             (if
              $5
              (list :si $2 $4 $5)
              (list :si $2 $4))))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-si)
            '(-->
              si-alors-sinon-1
              (alt
               (seq
                (tok-si
                 disjonction
                 tok-alors
                 instruction
                 si-alors-sinon-1-1)
                ((if
                  $5
                  (list :si $2 $4 $5)
                  (list :si $2 $4))))))))))))
  (progn
    (fmakunbound 'lse/parse-aller-en-1)
    (defun
        lse/parse-aller-en-1
        (scanner)
      "(--> aller-en-1 (seq (tok-aller tok-en expression) ((list :aller-en $3))))"
      (com.informatimago.rdp::with-non-terminal
          (aller-en-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-aller)
           (let*
               (($1 (accept scanner 'tok-aller))
                (tok-aller $1)
                (tok-aller.1 $1)
                ($2 (accept scanner 'tok-en))
                (tok-en $2)
                (tok-en.1 $2)
                ($3
                  (lse/parse-expression scanner))
                (expression $3)
                (expression.1 $3)
                ($0 (list $1 $2 $3)))
             (declare
              (ignorable
               $0
               expression.1
               expression
               tok-en.1
               tok-en
               tok-aller.1
               tok-aller
               $1
               $2
               $3))
             (list :aller-en $3)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-aller)
            '(-->
              aller-en-1
              (alt
               (seq
                (tok-aller tok-en expression)
                ((list :aller-en $3)))))))))))
  (progn
    (fmakunbound 'lse/parse-afficher-1)
    (defun
        lse/parse-afficher-1
        (scanner)
      "(--> afficher-1 (seq (tok-afficher afficher-1-1) ($2)))"
      (com.informatimago.rdp::with-non-terminal
          (afficher-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-afficher)
           (let*
               (($1
                  (accept scanner 'tok-afficher))
                (tok-afficher $1)
                (tok-afficher.1 $1)
                ($2
                  (lse/parse-afficher-1-1
                   scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               tok-afficher.1
               tok-afficher
               $1
               $2))
             $2))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-afficher)
            '(-->
              afficher-1
              (alt
               (seq
                (tok-afficher afficher-1-1)
                ($2))))))))))
  (progn
    (fmakunbound 'lse/parse-lire-1)
    (defun
        lse/parse-lire-1
        (scanner)
      "(--> lire-1 (seq (tok-lire liste-reference) ((cons :lire $2))))"
      (com.informatimago.rdp::with-non-terminal
          (lire-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-lire)
           (let*
               (($1 (accept scanner 'tok-lire))
                (tok-lire $1)
                (tok-lire.1 $1)
                ($2
                  (lse/parse-liste-reference
                   scanner))
                (liste-reference $2)
                (liste-reference.1 $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               liste-reference.1
               liste-reference
               tok-lire.1
               tok-lire
               $1
               $2))
             (cons :lire $2)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-lire)
            '(-->
              lire-1
              (alt
               (seq
                (tok-lire liste-reference)
                ((cons :lire $2)))))))))))
  (progn
    (fmakunbound 'lse/parse-appel-1)
    (defun
        lse/parse-appel-1
        (scanner)
      "(--> appel-1 (seq (procident tok-pargauche appel-1-1 tok-pardroite) ((list* :appel $1 $3))))"
      (com.informatimago.rdp::with-non-terminal
          (appel-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-procident)
           (let*
               (($1
                  (lse/parse-procident scanner))
                (procident $1)
                (procident.1 $1)
                ($2
                  (accept scanner 'tok-pargauche))
                (tok-pargauche $2)
                (tok-pargauche.1 $2)
                ($3
                  (lse/parse-appel-1-1 scanner))
                ($4
                  (accept scanner 'tok-pardroite))
                (tok-pardroite $4)
                (tok-pardroite.1 $4)
                ($0 (list $1 $2 $3 $4)))
             (declare
              (ignorable
               $0
               tok-pardroite.1
               tok-pardroite
               tok-pargauche.1
               tok-pargauche
               procident.1
               procident
               $1
               $2
               $3
               $4))
             (list* :appel $1 $3)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-procident)
            '(-->
              appel-1
              (alt
               (seq
                (procident
                 tok-pargauche
                 appel-1-1
                 tok-pardroite)
                ((list*
                  :appel
                  $1
                  $3)))))))))))
  (progn
    (fmakunbound 'lse/parse-affectation-1)
    (defun
        lse/parse-affectation-1
        (scanner)
      "(--> affectation-1 (seq (reference tok-fleche expression) ((list :affectation reference expression))))"
      (com.informatimago.rdp::with-non-terminal
          (affectation-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-identificateur)
           (let*
               (($1
                  (lse/parse-reference scanner))
                (reference $1)
                (reference.1 $1)
                ($2 (accept scanner 'tok-fleche))
                (tok-fleche $2)
                (tok-fleche.1 $2)
                ($3
                  (lse/parse-expression scanner))
                (expression $3)
                (expression.1 $3)
                ($0 (list $1 $2 $3)))
             (declare
              (ignorable
               $0
               expression.1
               expression
               tok-fleche.1
               tok-fleche
               reference.1
               reference
               $1
               $2
               $3))
             (list
              :affectation
              reference
              expression)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-identificateur)
            '(-->
              affectation-1
              (alt
               (seq
                (reference
                 tok-fleche
                 expression)
                ((list
                  :affectation
                  reference
                  expression)))))))))))
  (progn
    (fmakunbound 'lse/parse-affic-1-1)
    (defun
        lse/parse-affic-1-1
        (scanner)
      "(--> affic-1-1 (alt ((seq (liste-expression) ((list* :afficher nil $1))) (seq (format affic-1-1-1-1) ((list* :afficher $1 $2))))))"
      (com.informatimago.rdp::with-non-terminal
          (affic-1-1 scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-identificateur
              tok-litchaine
              tok-nombre
              tok-numero
              tok-si
              tok-at
              tok-pargauche
              tok-procident
              tok-moins)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-liste-expression
                   scanner))
                (liste-expression $1)
                (liste-expression.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               liste-expression.1
               liste-expression
               $1))
             (list* :afficher nil $1)))
          ((word-equal
            (scanner-current-token scanner)
            'tok-crogauche)
           (let*
               (($1 (lse/parse-format scanner))
                (format $1)
                (format.1 $1)
                ($2
                  (lse/parse-affic-1-1-1-1
                   scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               format.1
               format
               $1
               $2))
             (list* :afficher $1 $2)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-identificateur
              tok-litchaine
              tok-nombre
              tok-numero
              tok-si
              tok-at
              tok-pargauche
              tok-procident
              tok-moins
              tok-crogauche)
            '(-->
              affic-1-1
              (alt
               (seq
                (liste-expression)
                ((list* :afficher nil $1)))
               (seq
                (format affic-1-1-1-1)
                ((list*
                  :afficher
                  $1
                  $2)))))))))))
  (progn
    (fmakunbound 'lse/parse-affic-1-1-1-1)
    (defun
        lse/parse-affic-1-1-1-1
        (scanner)
      "(--> affic-1-1-1-1 (alt ((seq nil ('nil)) (seq (liste-expression) ($1)))))"
      (com.informatimago.rdp::with-non-terminal
          (affic-1-1-1-1 scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-identificateur
              tok-litchaine
              tok-nombre
              tok-numero
              tok-si
              tok-at
              tok-pargauche
              tok-procident
              tok-moins)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-liste-expression
                   scanner))
                (liste-expression $1)
                (liste-expression.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               liste-expression.1
               liste-expression
               $1))
             $1))))))
  (progn
    (fmakunbound 'lse/parse-format)
    (defun
        lse/parse-format
        (scanner)
      "(--> format (seq (tok-crogauche liste-spec tok-crodroite) ($2)))"
      (com.informatimago.rdp::with-non-terminal
          (format scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-crogauche)
           (let*
               (($1
                  (accept scanner 'tok-crogauche))
                (tok-crogauche $1)
                (tok-crogauche.1 $1)
                ($2
                  (lse/parse-liste-spec scanner))
                (liste-spec $2)
                (liste-spec.1 $2)
                ($3
                  (accept scanner 'tok-crodroite))
                (tok-crodroite $3)
                (tok-crodroite.1 $3)
                ($0 (list $1 $2 $3)))
             (declare
              (ignorable
               $0
               tok-crodroite.1
               tok-crodroite
               liste-spec.1
               liste-spec
               tok-crogauche.1
               tok-crogauche
               $1
               $2
               $3))
             $2))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-crogauche)
            '(-->
              format
              (alt
               (seq
                (tok-crogauche
                 liste-spec
                 tok-crodroite)
                ($2))))))))))
  (progn
    (fmakunbound
     'lse/parse-afficher-1-1-1-1)
    (defun
        lse/parse-afficher-1-1-1-1
        (scanner)
      "(--> afficher-1-1-1-1 (alt ((seq nil ('nil)) (seq (liste-expression) ($1)))))"
      (com.informatimago.rdp::with-non-terminal
          (afficher-1-1-1-1 scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-identificateur
              tok-litchaine
              tok-nombre
              tok-numero
              tok-si
              tok-at
              tok-pargauche
              tok-procident
              tok-moins)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-liste-expression
                   scanner))
                (liste-expression $1)
                (liste-expression.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               liste-expression.1
               liste-expression
               $1))
             $1))))))
  (progn
    (fmakunbound 'lse/parse-liste-spec)
    (defun
        lse/parse-liste-spec
        (scanner)
      "(--> liste-spec (seq (specification liste-spec-1-1) ((cons $1 $2))))"
      (com.informatimago.rdp::with-non-terminal
          (liste-spec scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-divise
              tok-x
              tok-c
              tok-l
              tok-u
              tok-f
              tok-e
              tok-fois
              tok-numero
              tok-litchaine)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-specification
                   scanner))
                (specification $1)
                (specification.1 $1)
                ($2
                  (lse/parse-liste-spec-1-1
                   scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               specification.1
               specification
               $1
               $2))
             (cons $1 $2)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-divise
              tok-x
              tok-c
              tok-l
              tok-u
              tok-f
              tok-e
              tok-fois
              tok-numero
              tok-litchaine)
            '(-->
              liste-spec
              (alt
               (seq
                (specification liste-spec-1-1)
                ((cons $1 $2)))))))))))
  (progn
    (fmakunbound
     'lse/parse-specification-1-10-1)
    (defun
        lse/parse-specification-1-10-1
        (scanner)
      "(--> specification-1-10-1 (alt ((seq (tok-divise) (:spec-slash)) (seq (tok-x) (:spec-space)) (seq (tok-c) (:spec-cr)) (seq (tok-l) (:spec-nl)) (seq (tok-u) (:spec-u)) (seq (tok-f tok-numero tok-point tok-numero) ((list :spec-f tok-numero.1 tok-numero.2))) (seq (tok-e tok-numero tok-point tok-numero) ((list :spec-e tok-numero.1 tok-numero.2))) (seq (tok-litchaine) ((list :spec-chaine $1))))))"
      (com.informatimago.rdp::with-non-terminal
          (specification-1-10-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-divise)
           (let*
               (($1 (accept scanner 'tok-divise))
                (tok-divise $1)
                (tok-divise.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-divise.1
               tok-divise
               $1))
             :spec-slash))
          ((word-equal
            (scanner-current-token scanner)
            'tok-x)
           (let*
               (($1 (accept scanner 'tok-x))
                (tok-x $1)
                (tok-x.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable $0 tok-x.1 tok-x $1))
             :spec-space))
          ((word-equal
            (scanner-current-token scanner)
            'tok-c)
           (let*
               (($1 (accept scanner 'tok-c))
                (tok-c $1)
                (tok-c.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable $0 tok-c.1 tok-c $1))
             :spec-cr))
          ((word-equal
            (scanner-current-token scanner)
            'tok-l)
           (let*
               (($1 (accept scanner 'tok-l))
                (tok-l $1)
                (tok-l.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable $0 tok-l.1 tok-l $1))
             :spec-nl))
          ((word-equal
            (scanner-current-token scanner)
            'tok-u)
           (let*
               (($1 (accept scanner 'tok-u))
                (tok-u $1)
                (tok-u.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable $0 tok-u.1 tok-u $1))
             :spec-u))
          ((word-equal
            (scanner-current-token scanner)
            'tok-f)
           (let*
               (($1 (accept scanner 'tok-f))
                (tok-f $1)
                (tok-f.1 $1)
                ($2 (accept scanner 'tok-numero))
                (tok-numero $2)
                (tok-numero.1 $2)
                ($3 (accept scanner 'tok-point))
                (tok-point $3)
                (tok-point.1 $3)
                ($4 (accept scanner 'tok-numero))
                (tok-numero.2 $4)
                ($0 (list $1 $2 $3 $4)))
             (declare
              (ignorable
               $0
               tok-numero.2
               tok-point.1
               tok-point
               tok-numero.1
               tok-numero
               tok-f.1
               tok-f
               $1
               $2
               $3
               $4))
             (list
              :spec-f
              tok-numero.1
              tok-numero.2)))
          ((word-equal
            (scanner-current-token scanner)
            'tok-e)
           (let*
               (($1 (accept scanner 'tok-e))
                (tok-e $1)
                (tok-e.1 $1)
                ($2 (accept scanner 'tok-numero))
                (tok-numero $2)
                (tok-numero.1 $2)
                ($3 (accept scanner 'tok-point))
                (tok-point $3)
                (tok-point.1 $3)
                ($4 (accept scanner 'tok-numero))
                (tok-numero.2 $4)
                ($0 (list $1 $2 $3 $4)))
             (declare
              (ignorable
               $0
               tok-numero.2
               tok-point.1
               tok-point
               tok-numero.1
               tok-numero
               tok-e.1
               tok-e
               $1
               $2
               $3
               $4))
             (list
              :spec-e
              tok-numero.1
              tok-numero.2)))
          ((word-equal
            (scanner-current-token scanner)
            'tok-litchaine)
           (let*
               (($1
                  (accept scanner 'tok-litchaine))
                (tok-litchaine $1)
                (tok-litchaine.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-litchaine.1
               tok-litchaine
               $1))
             (list :spec-chaine $1)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-divise
              tok-x
              tok-c
              tok-l
              tok-u
              tok-f
              tok-e
              tok-litchaine)
            '(-->
              specification-1-10-1
              (alt
               (seq
                (tok-divise)
                (:spec-slash))
               (seq (tok-x) (:spec-space))
               (seq (tok-c) (:spec-cr))
               (seq (tok-l) (:spec-nl))
               (seq (tok-u) (:spec-u))
               (seq
                (tok-f
                 tok-numero
                 tok-point
                 tok-numero)
                ((list
                  :spec-f
                  tok-numero.1
                  tok-numero.2)))
               (seq
                (tok-e
                 tok-numero
                 tok-point
                 tok-numero)
                ((list
                  :spec-e
                  tok-numero.1
                  tok-numero.2)))
               (seq
                (tok-litchaine)
                ((list
                  :spec-chaine
                  $1)))))))))))
  (progn
    (fmakunbound 'lse/parse-spec-rep-num)
    (defun
        lse/parse-spec-rep-num
        (scanner)
      "(--> spec-rep-num (seq (tok-numero) ((list :rep $1))))"
      (com.informatimago.rdp::with-non-terminal
          (spec-rep-num scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-numero)
           (let*
               (($1 (accept scanner 'tok-numero))
                (tok-numero $1)
                (tok-numero.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-numero.1
               tok-numero
               $1))
             (list :rep $1)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-numero)
            '(-->
              spec-rep-num
              (alt
               (seq
                (tok-numero)
                ((list :rep $1)))))))))))
  (progn
    (fmakunbound
     'lse/parse-specification-1-9-1)
    (defun
        lse/parse-specification-1-9-1
        (scanner)
      "(--> specification-1-9-1 (alt ((seq (tok-divise) (:spec-slash)) (seq (tok-x) (:spec-space)) (seq (tok-c) (:spec-cr)) (seq (tok-l) (:spec-nl)) (seq (tok-litchaine) ((list :spec-chaine $1))))))"
      (com.informatimago.rdp::with-non-terminal
          (specification-1-9-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-divise)
           (let*
               (($1 (accept scanner 'tok-divise))
                (tok-divise $1)
                (tok-divise.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-divise.1
               tok-divise
               $1))
             :spec-slash))
          ((word-equal
            (scanner-current-token scanner)
            'tok-x)
           (let*
               (($1 (accept scanner 'tok-x))
                (tok-x $1)
                (tok-x.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable $0 tok-x.1 tok-x $1))
             :spec-space))
          ((word-equal
            (scanner-current-token scanner)
            'tok-c)
           (let*
               (($1 (accept scanner 'tok-c))
                (tok-c $1)
                (tok-c.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable $0 tok-c.1 tok-c $1))
             :spec-cr))
          ((word-equal
            (scanner-current-token scanner)
            'tok-l)
           (let*
               (($1 (accept scanner 'tok-l))
                (tok-l $1)
                (tok-l.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable $0 tok-l.1 tok-l $1))
             :spec-nl))
          ((word-equal
            (scanner-current-token scanner)
            'tok-litchaine)
           (let*
               (($1
                  (accept scanner 'tok-litchaine))
                (tok-litchaine $1)
                (tok-litchaine.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-litchaine.1
               tok-litchaine
               $1))
             (list :spec-chaine $1)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-divise
              tok-x
              tok-c
              tok-l
              tok-litchaine)
            '(-->
              specification-1-9-1
              (alt
               (seq
                (tok-divise)
                (:spec-slash))
               (seq (tok-x) (:spec-space))
               (seq (tok-c) (:spec-cr))
               (seq (tok-l) (:spec-nl))
               (seq
                (tok-litchaine)
                ((list
                  :spec-chaine
                  $1)))))))))))
  (progn
    (fmakunbound 'lse/parse-spec-rep-fois)
    (defun
        lse/parse-spec-rep-fois
        (scanner)
      "(--> spec-rep-fois (seq (tok-fois) ((list :rep-var))))"
      (com.informatimago.rdp::with-non-terminal
          (spec-rep-fois scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-fois)
           (let*
               (($1 (accept scanner 'tok-fois))
                (tok-fois $1)
                (tok-fois.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-fois.1
               tok-fois
               $1))
             (list :rep-var)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-fois)
            '(-->
              spec-rep-fois
              (alt
               (seq
                (tok-fois)
                ((list :rep-var)))))))))))
  (progn
    (fmakunbound 'lse/parse-terme-signe)
    (defun
        lse/parse-terme-signe
        (scanner)
      "(--> terme-signe (seq (terme-signe-1-1 terme) ((if $1 (list :neg terme) terme))))"
      (com.informatimago.rdp::with-non-terminal
          (terme-signe scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(nil
              tok-identificateur
              tok-litchaine
              tok-nombre
              tok-numero
              tok-si
              tok-at
              tok-pargauche
              tok-procident
              tok-divise
              tok-fois
              tok-puissance
              tok-virgule
              tok-crodroite
              tok-sinon
              tok-pour
              tok-jusqua
              tok-tant
              tok-pas
              tok-lt
              tok-le
              tok-ne
              tok-gt
              tok-ge
              tok-eq
              tok-fin
              eol
              tok-ptvirg
              tok-et
              tok-pardroite
              tok-alors
              tok-ou
              tok-plus
              tok-concat
              tok-moins)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-terme-signe-1-1
                   scanner))
                ($2 (lse/parse-terme scanner))
                (terme $2)
                (terme.1 $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               terme.1
               terme
               $1
               $2))
             (if $1 (list :neg terme) terme)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(nil
              tok-identificateur
              tok-litchaine
              tok-nombre
              tok-numero
              tok-si
              tok-at
              tok-pargauche
              tok-procident
              tok-divise
              tok-fois
              tok-puissance
              tok-virgule
              tok-crodroite
              tok-sinon
              tok-pour
              tok-jusqua
              tok-tant
              tok-pas
              tok-lt
              tok-le
              tok-ne
              tok-gt
              tok-ge
              tok-eq
              tok-fin
              eol
              tok-ptvirg
              tok-et
              tok-pardroite
              tok-alors
              tok-ou
              tok-plus
              tok-concat
              tok-moins)
            '(-->
              terme-signe
              (alt
               (seq
                (terme-signe-1-1 terme)
                ((if
                  $1
                  (list :neg terme)
                  terme)))))))))))
  (progn
    (fmakunbound 'lse/parse-terme)
    (defun
        lse/parse-terme
        (scanner)
      "(--> terme (seq (facteur terme-1-1) ((if $2 (uncomb (cons facteur $2)) facteur))))"
      (com.informatimago.rdp::with-non-terminal
          (terme scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-identificateur
              tok-litchaine
              tok-nombre
              tok-numero
              tok-si
              tok-at
              tok-pargauche
              tok-procident)
            :test
            #'word-equal)
           (let*
               (($1 (lse/parse-facteur scanner))
                (facteur $1)
                (facteur.1 $1)
                ($2
                  (lse/parse-terme-1-1 scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               facteur.1
               facteur
               $1
               $2))
             (if
              $2
              (uncomb (cons facteur $2))
              facteur)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-identificateur
              tok-litchaine
              tok-nombre
              tok-numero
              tok-si
              tok-at
              tok-pargauche
              tok-procident)
            '(-->
              terme
              (alt
               (seq
                (facteur terme-1-1)
                ((if
                  $2
                  (uncomb (cons facteur $2))
                  facteur)))))))))))
  (progn
    (fmakunbound
     'lse/parse-terme-signe-1-1)
    (defun
        lse/parse-terme-signe-1-1
        (scanner)
      "(--> terme-signe-1-1 (alt ((seq nil ('nil)) (seq (tok-moins) ($0)))))"
      (com.informatimago.rdp::with-non-terminal
          (terme-signe-1-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-moins)
           (let*
               (($1 (accept scanner 'tok-moins))
                (tok-moins $1)
                (tok-moins.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-moins.1
               tok-moins
               $1))
             $0))))))
  (progn
    (fmakunbound 'lse/parse-facteur)
    (defun
        lse/parse-facteur
        (scanner)
      "(--> facteur (seq (simple facteur-1-1) ((if $2 (uncomb (cons simple $2)) simple))))"
      (com.informatimago.rdp::with-non-terminal
          (facteur scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-procident
              tok-pargauche
              tok-at
              tok-si
              tok-numero
              tok-nombre
              tok-litchaine
              tok-identificateur)
            :test
            #'word-equal)
           (let*
               (($1 (lse/parse-simple scanner))
                (simple $1)
                (simple.1 $1)
                ($2
                  (lse/parse-facteur-1-1 scanner))
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               simple.1
               simple
               $1
               $2))
             (if
              $2
              (uncomb (cons simple $2))
              simple)))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-procident
              tok-pargauche
              tok-at
              tok-si
              tok-numero
              tok-nombre
              tok-litchaine
              tok-identificateur)
            '(-->
              facteur
              (alt
               (seq
                (simple facteur-1-1)
                ((if
                  $2
                  (uncomb (cons simple $2))
                  simple)))))))))))
  (progn
    (fmakunbound 'lse/parse-simple-1-2-1)
    (defun
        lse/parse-simple-1-2-1
        (scanner)
      "(--> simple-1-2-1 (alt ((seq (tok-pargauche simple-1-2-1-1-1 tok-pardroite) ((list $2))) (seq nil ('nil)))))"
      (com.informatimago.rdp::with-non-terminal
          (simple-1-2-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-pargauche)
           (let*
               (($1
                  (accept scanner 'tok-pargauche))
                (tok-pargauche $1)
                (tok-pargauche.1 $1)
                ($2
                  (lse/parse-simple-1-2-1-1-1
                   scanner))
                ($3
                  (accept scanner 'tok-pardroite))
                (tok-pardroite $3)
                (tok-pardroite.1 $3)
                ($0 (list $1 $2 $3)))
             (declare
              (ignorable
               $0
               tok-pardroite.1
               tok-pardroite
               tok-pargauche.1
               tok-pargauche
               $1
               $2
               $3))
             (list $2)))))))
  (progn
    (fmakunbound
     'lse/parse-simple-1-2-1-1-1)
    (defun
        lse/parse-simple-1-2-1-1-1
        (scanner)
      "(--> simple-1-2-1-1-1 (alt ((seq nil ('nil)) (seq (liste-argument) ($1)))))"
      (com.informatimago.rdp::with-non-terminal
          (simple-1-2-1-1-1 scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-identificateur
              tok-litchaine
              tok-nombre
              tok-numero
              tok-si
              tok-at
              tok-pargauche
              tok-procident
              tok-moins)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-liste-argument
                   scanner))
                (liste-argument $1)
                (liste-argument.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               liste-argument.1
               liste-argument
               $1))
             $1))))))
  (progn
    (fmakunbound
     'lse/parse-condition-1-2-1)
    (defun
        lse/parse-condition-1-2-1
        (scanner)
      "(--> condition-1-2-1 (alt ((seq (condition-1-2-1-1-1 expression) ((list $1 expression))) (seq nil ('nil)))))"
      (com.informatimago.rdp::with-non-terminal
          (condition-1-2-1 scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-eq
              tok-ge
              tok-gt
              tok-ne
              tok-le
              tok-lt)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-condition-1-2-1-1-1
                   scanner))
                ($2
                  (lse/parse-expression scanner))
                (expression $2)
                (expression.1 $2)
                ($0 (list $1 $2)))
             (declare
              (ignorable
               $0
               expression.1
               expression
               $1
               $2))
             (list $1 expression)))))))
  (progn
    (fmakunbound
     'lse/parse-condition-1-2-1-1-1)
    (defun
        lse/parse-condition-1-2-1-1-1
        (scanner)
      "(--> condition-1-2-1-1-1 (alt ((seq (tok-lt) (:lt)) (seq (tok-le) (:le)) (seq (tok-ne) (:ne)) (seq (tok-gt) (:gt)) (seq (tok-ge) (:ge)) (seq (tok-eq) (:eg)))))"
      (com.informatimago.rdp::with-non-terminal
          (condition-1-2-1-1-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-lt)
           (let*
               (($1 (accept scanner 'tok-lt))
                (tok-lt $1)
                (tok-lt.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-lt.1
               tok-lt
               $1))
             :lt))
          ((word-equal
            (scanner-current-token scanner)
            'tok-le)
           (let*
               (($1 (accept scanner 'tok-le))
                (tok-le $1)
                (tok-le.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-le.1
               tok-le
               $1))
             :le))
          ((word-equal
            (scanner-current-token scanner)
            'tok-ne)
           (let*
               (($1 (accept scanner 'tok-ne))
                (tok-ne $1)
                (tok-ne.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-ne.1
               tok-ne
               $1))
             :ne))
          ((word-equal
            (scanner-current-token scanner)
            'tok-gt)
           (let*
               (($1 (accept scanner 'tok-gt))
                (tok-gt $1)
                (tok-gt.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-gt.1
               tok-gt
               $1))
             :gt))
          ((word-equal
            (scanner-current-token scanner)
            'tok-ge)
           (let*
               (($1 (accept scanner 'tok-ge))
                (tok-ge $1)
                (tok-ge.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-ge.1
               tok-ge
               $1))
             :ge))
          ((word-equal
            (scanner-current-token scanner)
            'tok-eq)
           (let*
               (($1 (accept scanner 'tok-eq))
                (tok-eq $1)
                (tok-eq.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               tok-eq.1
               tok-eq
               $1))
             :eg))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(tok-lt
              tok-le
              tok-ne
              tok-gt
              tok-ge
              tok-eq)
            '(-->
              condition-1-2-1-1-1
              (alt
               (seq (tok-lt) (:lt))
               (seq (tok-le) (:le))
               (seq (tok-ne) (:ne))
               (seq (tok-gt) (:gt))
               (seq (tok-ge) (:ge))
               (seq (tok-eq) (:eg))))))))))
  (progn
    (fmakunbound 'lse/parse-reference-1-1)
    (defun
        lse/parse-reference-1-1
        (scanner)
      "(--> reference-1-1 (alt ((seq nil ('nil)) (seq (reference-1-1-1) ($1)))))"
      (com.informatimago.rdp::with-non-terminal
          (reference-1-1 scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-crogauche tok-pargauche)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-reference-1-1-1
                   scanner))
                ($0 (list $1)))
             (declare (ignorable $0 $1))
             $1))))))
  (progn
    (fmakunbound
     'lse/parse-reference-1-1-1-1-1)
    (defun
        lse/parse-reference-1-1-1-1-1
        (scanner)
      "(--> reference-1-1-1-1-1 (alt ((seq nil ('nil)) (seq (reference-1-1-1-1-1-1) ($0)))))"
      (com.informatimago.rdp::with-non-terminal
          (reference-1-1-1-1-1 scanner)
        (cond
          ((word-equal
            (scanner-current-token scanner)
            'tok-virgule)
           (let*
               (($1
                  (lse/parse-reference-1-1-1-1-1-1
                   scanner))
                ($0 (list $1)))
             (declare (ignorable $0 $1))
             $0))))))
  (progn
    (fmakunbound
     'lse/parse-reference-1-1-1-2-1)
    (defun
        lse/parse-reference-1-1-1-2-1
        (scanner)
      "(--> reference-1-1-1-2-1 (alt ((seq nil ('nil)) (seq (liste-argument) ($1)))))"
      (com.informatimago.rdp::with-non-terminal
          (reference-1-1-1-2-1 scanner)
        (cond
          ((member
            (scanner-current-token scanner)
            '(tok-identificateur
              tok-litchaine
              tok-nombre
              tok-numero
              tok-si
              tok-at
              tok-pargauche
              tok-procident
              tok-moins)
            :test
            #'word-equal)
           (let*
               (($1
                  (lse/parse-liste-argument
                   scanner))
                (liste-argument $1)
                (liste-argument.1 $1)
                ($0 (list $1)))
             (declare
              (ignorable
               $0
               liste-argument.1
               liste-argument
               $1))
             $1))))))
  (progn
    (fmakunbound 'lse/parse-debut)
    (defun
        lse/parse-debut
        (scanner)
      "(--> debut (seq (debut-1) ($1)))"
      (com.informatimago.rdp::with-non-terminal
          (debut scanner)
        (cond
          ((member
            (print (scanner-current-token scanner))
            '(nil
              tok-numero
              tok-affic
              tok-commentaire
              tok-chaine
              tok-tableau
              tok-liberer
              tok-executer
              tok-supprimer
              tok-charger
              tok-garer
              tok-resultat
              tok-retour
              tok-faire
              tok-debut
              tok-pause
              tok-terminer
              tok-si
              tok-aller
              tok-afficher
              tok-lire
              tok-procident
              tok-identificateur
              eol)
            :test
            #'word-equal)
           (let*
               (($1 (lse/parse-debut-1 scanner))
                ($0 (list $1)))
             (declare (ignorable $0 $1))
             $1))
          (t
           (com.informatimago.rdp::error-unexpected-token
            scanner
            '(nil
              tok-numero
              tok-affic
              tok-commentaire
              tok-chaine
              tok-tableau
              tok-liberer
              tok-executer
              tok-supprimer
              tok-charger
              tok-garer
              tok-resultat
              tok-retour
              tok-faire
              tok-debut
              tok-pause
              tok-terminer
              tok-si
              tok-aller
              tok-afficher
              tok-lire
              tok-procident
              tok-identificateur
              eol)
            '(-->
              debut
              (alt
               (seq (debut-1) ($1))))))))))
  (progn
    (fmakunbound 'parse-lse)
    (defun
        parse-lse
        (com.informatimago.rdp::source)
      "
SOURCE: When the grammar has a scanner generated, or a scanner class
        name, SOURCE can be either a string, or a stream that will be
        scanned with the generated scanner.  Otherwise, it should be a
        SCANNER instance.
"
      (let
          ((scanner
             com.informatimago.rdp::source))
        (advance-line scanner)
        (com.informatimago.rdp::with-non-terminal
            (lse scanner)
          (prog1
              (lse/parse-debut scanner)
            (unless
                (scanner-end-of-source-p scanner)
              (cerror
               "Continue"
               'parser-end-of-source-not-reached
               :file
               (scanner-file scanner)
               :line
               (scanner-line scanner)
               :column
               (scanner-column scanner)
               :grammar
               (grammar-named 'lse)
               :scanner
               scanner
               :non-terminal-stack
               (copy-list
                *non-terminal-stack*))))))))
  'lse)
