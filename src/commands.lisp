;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               commands.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    The commands of the LSE system.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-07 <PJB> Converted the grammars to rdp
;;;;                     (zebu uses too much files for the grammars).
;;;;    2005-08-26 <PJB> Distiled form C 'lse_cmd.c'.
;;;;    2002-02-12 <PJB> Extracted from lse_main.c
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Command line grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; :white-space = single space.
;; we don't keep newline because it's the eoln/end of statement
;; and we don't keep tabulation because it's not valid.


(defgrammar numero-de-ligne
    :terminals    ((tok-numero  "[0-9]+"))
    :start numero-de-ligne
    :rules ((--> numero-de-ligne
                 tok-numero :action (let ((lino (parse-integer (second $1))))
                                      (unless (valid-line-number-p lino)
                                        (error 'lse-error
                                               :format-control "NUMERO DE LIGNE INVALIDE: ~A"
                                               :format-arguments (list (second $1))))
                                      (list lino)))))

(defgrammar un-numero
    :terminals ((tok-numero  "[0-9]+"))
    :start un-numero
    :rules ((--> un-numero
                 tok-numero
                 :action (list (parse-integer (second $1))))))


(defgrammar deux-numeros
    :terminals ((tok-virgule ",")
                (tok-numero  "[0-9]+"))
    :start deux-numeros
    :rules ((--> deux-numeros
                 (seq tok-numero tok-virgule tok-numero
                      :action (list (parse-integer (second $1))
                                    (parse-integer (second $3))))
                 :action $1)))


(defgrammar deux-numeros-optionels
    :terminals ((tok-virgule ",")
                (tok-numero  "[0-9]+"))
    :start deux-numeros-optionels
    :rules ((--> deux-numeros-optionels
                 (opt (seq tok-numero
                           (opt (seq tok-virgule tok-numero :action (parse-integer (second $2)))
                                :action $1)
                           :action (list (parse-integer (second $1))
                                         $2))
                      :action $1)
                 :action (if $1
                             $1
                             (list 1 nil)))))



(defgrammar liste-de-numeros
    :terminals ((tok-virgule ",")
                (tok-fois    "\\*")
                (tok-a       #-LSE-CASE-INSENSITIVE "A" #+LSE-CASE-INSENSITIVE "[Aa]")
                (tok-numero  "[0-9]+"))
    :start liste-de-numeros
    :rules (
            (--> liste-de-numeros
                 (alt (seq tok-fois :action (progn :all))
                      (seq          :action (progn nil))
                      liste-d-intervales)
                 :action (list $1))

            (--> liste-d-intervales
                 (seq intervale-de-numeros (alt (seq tok-virgule liste-d-intervales
                                                     :action liste-d-intervales)
                                                (seq :action (list nil)))
                      :action (cons intervale-de-numeros $2))
                 :action $1)
            
            (--> intervale-de-numeros
                 (seq numero-de-ligne (alt (seq tok-a numero-de-ligne
                                                :action $2)
                                           (seq :action (progn nil)))
                      :action (if $2
                                  (cons numero-de-ligne $2)
                                  numero-de-ligne))
                 :action $1)

            (--> numero-de-ligne
                 tok-numero :action (let ((lino (parse-integer (second $1))))
                                      (unless (valid-line-number-p lino)
                                        (error 'lse-error
                                               :format-control "NUMERO DE LIGNE INVALIDE: ~A"
                                               :format-arguments (list (second $1))))
                                      lino))))


(defgrammar un-programme
    :terminals ((tok-identificateur "[A-Za-z][A-Za-z0-9]*"))
    :start un-programme
    :rules (
            (--> un-programme
                 ident
                 :action (list $1))

            (--> ident
                 (seq tok-identificateur
                      ;; :action (let ((text (second $1)))
                      ;;   (unless (<= (length text) 5)
                      ;;     (error 'lse-error
                      ;;            :format-control "IDENTIFICATEUR INVALIDE: ~A"
                      ;;            :format-arguments (list text)))
                      ;;   text)
                      :action (second $1))
                 :action $1)))


(defgrammar arguments-supprimer
    :terminals ((tok-star    "\\*")
                (tok-virgule ",")
                (tok-identificateur "[A-Za-z][A-Za-z0-9]*"))
    :start arguments-supprimer
    :rules ((--> arguments-supprimer
                 (alt
                  (seq tok-star :action '(*))
                  (seq ident tok-virgule mode
                       :action (list $1 $3)))
                 :action $1)
            (--> ident
                 (seq tok-identificateur
                      ;; :action  (let ((text (second $1)))
                      ;;           (unless (<= (length text) 5)
                      ;;             (error 'lse-error
                      ;;                    :format-control "IDENTIFICATEUR INVALIDE: ~A"
                      ;;                    :format-arguments (list text)))
                      ;;           text)
                      :action (second $1))
                 :action $1)
            (--> mode
                 (seq tok-identificateur
                      :action (let ((text (string-upcase (second $1))))
                                (unless (member text '("P" "D" "T") :test (function string=))
                                  (error 'lse-error
                                         :format-control "TYPE DE FICHIER INVALIDE: ~A; ATTENDU: P, D OU T."
                                         :format-arguments (list text)))
                                text))
                 :action $1)))


(defgrammar un-fichier
    :terminals ((tok-identificateur "[A-Za-z][A-Za-z0-9]*"))
    :start un-fichier
    :rules ((--> un-fichier
                 ident
                 :action (list $1))
            
            (--> ident
                 (seq tok-identificateur
                      ;; :action (let ((text (second $1)))
                      ;;           (unless (<= (length text) 5)
                      ;;             (error 'lse-error
                      ;;                    :format-control "IDENTIFICATEUR INVALIDE: ~A"
                      ;;                    :format-arguments (list text)))
                      ;;           text)
                      :action (second $1))
                 :action $1)))




(defgrammar deux-fichiers
    :terminals ((tok-virgule ",")
                (tok-identificateur "[A-Za-z][A-Za-z0-9]*"))
    :start deux-fichiers
    :rules ((--> deux-fichiers
                 (seq ident tok-virgule ident
                      :action (list $1 $3))
                 :action $1)
            
            (--> ident
                 (seq tok-identificateur
                      ;; :action (let ((text (second $1)))
                      ;;           (unless (<= (length text) 5)
                      ;;             (error 'lse-error
                      ;;                    :format-control "IDENTIFICATEUR INVALIDE: ~A"
                      ;;                    :format-arguments (list text)))
                      ;;           text)
                      :action (second $1))
                 :action $1)))



(defgrammar un-fichier-et-deux-numeros
    :terminals  ((tok-virgule ",")
                 (tok-numero  "[0-9]+")
                 (tok-identificateur "[A-Za-z][A-Za-z0-9]*"))
    :start un-fichier-et-deux-numeros
    :rules (
            (--> un-fichier-et-deux-numeros
                 (seq ident (opt (seq tok-virgule deux-numeros :action deux-numeros)
                                 :action $1)
                      :action (cons ident $2))
                 :action $1)

            (--> ident
                 (seq tok-identificateur
                      ;; :action (let ((text (second $1)))
                      ;;           (unless (<= (length text) 5)
                      ;;             (error 'lse-error
                      ;;                    :format-control "IDENTIFICATEUR INVALIDE: ~A"
                      ;;                    :format-arguments (list text)))
                      ;;           text)
                      :action (second $1))
                 :action $1)

            (--> deux-numeros
                 (seq tok-numero (opt (seq tok-virgule tok-numero
                                           :action  (parse-integer (second tok-numero))))
                      :action (cons (parse-integer (second tok-numero)) $2))
                 :action $1)))



;; (defgrammar un-chemin
;;     :terminals  ((chaine "'(('')?[^']*)*'"))
;;     :start un-chemin
;;     :rules ((--> un-chemin
;;                  chaine
;;                  :action (list (chaine-valeur (make-instance 'tok-chaine
;;                                                   :kind 'tok-chaine
;;                                                   :text (second chaine)))))))

(defun parse-une-ligne (ligne) (list ligne))

(defun valid-line-number-p (lino) (<= 1 lino 255))


(defun test/command-grammars ()
  (assert (equal '(  9) (parse-numero-de-ligne "9")))
  (assert (equal '(123) (parse-numero-de-ligne "123")))
  (assert (null (ignore-errors (parse-numero-de-ligne "999"))))

  (assert (equal '(  8) (parse-un-numero "8")))
  (assert (equal '(123) (parse-un-numero "123")))
  (assert (equal '(999) (parse-un-numero "999")))

  (assert (equal '(3 4) (parse-deux-numeros "3,4")))
  (assert (equal '(123 999) (parse-deux-numeros "123,999")))
  (assert (equal '(123 999) (parse-deux-numeros-optionels "123,999")))
  (assert (equal '(123 nil) (parse-deux-numeros-optionels "123")))

  (assert (equal '(nil)  (parse-liste-de-numeros "")))
  (assert (equal '(:all) (parse-liste-de-numeros "*")))
  (assert (equal '((7))    (parse-liste-de-numeros "7")))
  (assert (equal '((123))  (parse-liste-de-numeros "123")))
  (assert (equal '((10 20 30)) (parse-liste-de-numeros "10,20,30")))
  (assert (equal '((10 20 (30 . 40) 50)) (parse-liste-de-numeros "10, 20, 30 A 40,50")))

  (assert (equal '("A")     (parse-un-programme "A")))
  (assert (equal '("BOUR")  (parse-un-programme "BOUR")))
  (assert (equal '("C1564") (parse-un-programme "C1564")))
  
  (assert (equal '("ABCD") (parse-un-fichier-et-deux-numeros "ABCD")))
  (assert (equal '("ABCD" 100) (parse-un-fichier-et-deux-numeros "ABCD,100")))
  (assert (equal '("ABCD" 100 200) (parse-un-fichier-et-deux-numeros "ABCD,100,200")))
  (assert (equal '("A" 1 2) (parse-un-fichier-et-deux-numeros "A,1,2")))


  (assert (null (ignore-errors (parse-deux-fichiers "UN"))))
  (assert (equal '("A" "B") (parse-deux-fichiers "A , B")))
  (assert (equal '("UN" "DEUX") (parse-deux-fichiers "UN,DEUX")))


  (assert (null (ignore-errors (parse-arguments-supprimer "UN"))))
  (assert (null (ignore-errors (parse-arguments-supprimer "DEUX,Z"))))
  (assert (equal '("A" "P") (parse-arguments-supprimer "A , P")))
  (assert (equal '("FICHI" "D") (parse-deux-fichiers "FICHI,D")))

  :success)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LSE Documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *chapters* (make-hash-table :test (function equalp)))
(defun find-chapter (title) (gethash title *chapters*))

(defstruct chapter
  title
  text)

(defmacro defchapter (title &rest text)
  `(setf (gethash ',title *chapters*)
         (make-chapter :title ',title
                       :text ',(unsplit-string text #\Newline))))



(defchapter "GARANTIE"
"
Système L.S.E
Copyright (C) 2012 Pascal Bourguignon

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
"
    )

(defchapter "COPIE"
    #.*license*
    )

(defchapter "SOURCES"
    "
Les sources de ce programme sont disponibles sous license AGPLv3
à l'adresse suivante:
http://www.ogamita.com/lse/
")


(defchapter "INSTRUCTIONS"
    "Voici la liste des instructions disponibles.  

Taper DO)CUMENTATION <instruction> pour avoir la documentation de
chaque instruction:

affectation            ref ← expression
appel                  &procid(arguments…)
liberer                LIBERER id1,…,idn      
lire                   LIRE id1,…,idn            
afficher               AFFICHER [format]expr…
alleren                ALLER EN lino
si                     SI condition ALORS instruction [SINON instruction]
debut                  DEBUG instruction;… FIN
fairejusqua            FAIRE lino POUR var←expri [PAS exprp] JUSQUA exprf
fairetantque           FAIRE lino POUR var←expri [PAS exprp] TANT QUE exprb
retour                 RETOUR
retouren               RETOUR EN lino
resultat               RESULTAT expr
garer                  GARER 
charger                CHARGER
supprimer              SUPPRIMER
executer               EXCUTER
pause                  PAUSE
terminer               TERMINER
")

(defchapter "FONCTIONS"
    "Voici la liste des fonctions disponibles.  

Taper DO)CUMENTATION <fonction> pour avoir la documentation de
chaque fonction.

PLUS                   a+b, somme de deux nombres;
MOINS                  a-b, différence de deux nombres;
NEGATION               -a, négation unaire;
FOIS                   a*b, produit de deux nombres;
DIVISE                 a/b, quotient de deux nombres;
PUISSANCE              a↑b, puissance;
CONCATENATION          a!b, concatenation de deux chaînes;
SITERNAIRE             SI condition ALORS expr1 SINON expr2;
COMPARAISONS           a<b, a<=b, a>b, a>=b, a=b, a#b;
CONJONCTION            comparaison1 ET comparaison2;
DISJONCTION            conjonction OU conjonection;
NEGATION               NON condition;

ENT                    ENT(expr), partie entière;
ABS                    ABS(expr), valeur absolue;
EXP                    EXP(expr), exponentiation: e↑expr;
SIN                    SIN(expr), sinus;
COS                    COS(expr), cosinus;
ATG                    ATG(expr), arc tangente;
RAC                    RAC(expr), racine carrée;
LGN                    LGN(expr), logarithme népérien;
ALE                    ALE(expr), valeur aléatoire;
ATT                    ATT(), signal d'attention utilisateur;
ETL                    ETL(expr1,expr2), ET Logique;
OUL                    OUL(expr1,expr2), OU Logique;
OXL                    OXL(expr1,expr2), OU exclusif Logique;
TEM                    TEM(), temps;
DAT                    DAT(), chaîne date;
LGR                    LGR(ch), longueur chaine;
POS                    POS(ch,de,sc), position sous-chaîne;
EQN                    EQN(ch), équivalent numérique;
EQC                    EQC(ch), équivalent caractère;
CCA                    CCA(ca), conversion en caractères;
CNB                    CNB(ch,de), conversion en nombre;
SCH                    SCH(ch,de,fi), sous-chaîne;
SKP                    SKP(ch,de[,ev]), saut;
PTR                    PTR(ch,de[,ev]), pointeur;
GRL                    GRL(ch,de), groupe de lettres;

")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LSE Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *command-scanner*
  (let ((scanner (make-instance 'scanner :source (MAKE-STRING-INPUT-STREAM ""))))
    ;; (advance-token scanner)
    scanner))


(defstruct command
  initials
  name
  grammar 
  arguments
  oneliner
  documentation
  function)


(defmacro defcommand (name grammar arguments &body body)
  (let ((oneliner      (if (stringp (first body))
                           (pop body)
                           nil))
        (documentation (if (stringp (first body))
                           (pop body)
                           nil)))
    `(make-command
      :initials       (subseq ,name 0 2)
      :name          ,name
      :grammar       ',grammar ;; (when grammar (find-grammar (string grammar)))
      :arguments     ',arguments
      :oneliner      ,oneliner
      :documentation ,(or documentation oneliner)
      :Function       (lambda ,arguments
                        (block ,(intern name  "COM.INFORMATIMAGO.LSE")
                          ,@body)))))


(defvar *command-group* nil
  "The command-group the command being called belongs to.")

(defmethod command-call ((command command))
  (let ((gn (command-grammar command)))
    (if gn
        (apply (command-function command)
               (funcall (intern (with-standard-io-syntax (format nil "PARSE-~A" gn))
                                 "COM.INFORMATIMAGO.LSE")
                        (io-read-line *task* :beep nil)))
        (funcall (command-function command)))))



(defstruct command-group
  name
  supergroups
  commands)


(defmacro define-command-group (name supergroups &body commands)
  `(defparameter ,name
     (make-command-group
      :name ',name
      :supergroups ',supergroups
      :commands (let* ((commands (list ,@commands))
                       (dups (duplicates commands
                                         :test 'equalp
                                         :key (function command-initials))))
                  (when dups
                    (error "There are commands with duplicate initials: ~{~{~A~^, ~}~^; ~}."
                           (mapcar (lambda (dup)
                                     (mapcar (function command-name)
                                             (remove (command-initials dup) commands
                                                     :test-not (function string-equal)
                                                     :key (function command-initials))))
                                   dups)))
                  commands))))


(defmethod find-command (command (group command-group))
  (or (find command (command-group-commands group)
            :test
            #-LSE-CASE-INSENSITIVE (function string=)
            #+LSE-CASE-INSENSITIVE (function string-equal)
            :key (function command-initials))
      (some (lambda (supergroup) (find-command command (symbol-value supergroup)))
            (command-group-supergroups group))))


(defmethod all-commands ((group command-group))
  (append (command-group-commands group)
          (mapcan (lambda (supergroup) (all-commands (symbol-value supergroup)))
                  (command-group-supergroups group))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ---------------------
;;   Fichier {abstrait}
;; ---------------------
;;  nom : CHAR(5)      ---[ Les espaces de nom pour les fichiers permanents ]
;;  secteurs : entier     [ et pour les fichiers temporaires sont distincts ]
;; ---------------------
;;              ---------------------
;;               Fichier Permanent
;;              ---------------------
;;               num_compte : entier
;;               date_modif : date
;;              ---------------------
;;                             ---------------------
;;                              Fichier Programme
;;                             ---------------------
;;                              taille_mot : entier
;;                             ---------------------
;; 
;;              ----------------------
;;               Fichier Temporaire
;;              ----------------------
;;               num_console : entier
;;              ----------------------
;; 
;; 
;; 
;; -----------------
;;  Console
;; -----------------      [ il y a un espace de nom de temporaire par console ]
;;  numero : entier
;; -----------------
;; 
;; ---------------------
;;  Compte
;; ---------------------
;;  numero : entier  -----[ numero = XXXNN : XXX est caché ; NN est affiché ]
;;  droit_pgm : bool  ----[a:creation, modification, suppression de programmes]
;;  droit_per : bool  ----[b:creation, modification, suppression de permanents]
;;  droit_tau : bool    ----[c:augmentation dynamique de l'espace temporaire]
;;  droit_tfi : bool  ----[d:allocation d'un espace temporaire fixe superieur]
;; --------------------
;; 
;; 
;;  [a: creation, modification, suppression de programmes]
;;  [b: creation, modification, suppression de permanents]
;;  [c: augmentation dynamique de l'espace temporaire]
;;  [d: allocation d'un espace temporaire fixe superieur]
;;  Les combinaisons possibles sont :
;;   Combinaison de droits :  a    b    ab   c    ac   bc   abc  d    bd   abd
;;   Codes à   utiliser       1    2    3    4    5    6    7    8    :     ;
;;    en quatrième colone :   A    B    C    D    E    F    G    H    J     K
;;                                           d    ad   bd   adb






(define-command-group common ()

  (defcommand "AIDER" nil ()
    "Affiche la liste des commandes."
    (io-format *task* "~%LES COMMANDES DISPONIBLES SONT:~2%")
    (let ((*print-right-margin* 80))
      (dolist (command (all-commands *command-group*))
        (io-format *task* "~A)~20A ~{~{~<~%                        ~1,80:;~A~> ~}~^~%                        ~}~%"
                   (command-initials command)
                   (subseq (command-name command) 2)
                   (mapcar (lambda (line) (split-sequence #\Space line))
                           (split-sequence #\Newline (command-oneliner command))))))
    (io-format *task* "~&POUR ENTRER UNE COMMANDE, TAPEZ SES DEUX ~
                         PREMIERES LETTRES, SUIVIES DE CTRL-S.~
                       ~&POUR ANNULER UN CARACTERE, TAPEZ \\.~%"))

  
  (defcommand "DOCUMENTATION" une-ligne (what)
    "Affiche la documentation d'une commande ou d'une instruction."
    (cond
      ((let ((command (find-command what *command-group*)))
         (when command
           (io-format *task* "~%~A)~20A ~{~{~<~%                        ~1,80:;~A~> ~}~^~%                        ~}~%"
                      (command-initials command)
                      (subseq (command-name command) 2)
                      (mapcar (lambda (line) (split-sequence #\Space line))
                              (split-sequence #\Newline (command-documentation command))))
           t)))
      ((let ((chapter (find-chapter what)))
         (when chapter
           (io-format *task* "~%~A~%~V,,,'-A~2%~{~{~<~%~1,80:;~A~> ~}~%~}~%"
                      (chapter-title chapter)
                      (length (chapter-title chapter))
                      ""
                      (mapcar (lambda (line) (split-sequence #\Space line))
                              (split-sequence #\Newline (chapter-text chapter))))
           t)))
      (t
       (io-format *task*
                  "~%Il n'y a pas de documentation pour ~S~
                   ~%Essayez les commandes suivantes:~
                   ~%DO)CUMENTATION GARANTIE       indique qu'aucune garantie n'est assurée;~
                   ~%DO)CUMENTATION COPIE          donne la license et votre liberté de copier;~
                   ~%DO)CUMENTATION SOURCES        où trouver les sources de ce programme;~
                   ~%DO)CUMENTATION INSTRUCTIONS   donne la liste des instructions;~
                   ~%DO)CUMENTATION FONCTIONS      donne la liste des fonctions;~
                   ~%AI)DE                         donne la liste des commandes.~2%"
                  what))))
  

  (defcommand "IDENTIFICATION" un-numero (identification)
    "Permet à l'utilisateur de s'identifier."
    ;;     void lse_cmd_identification(lse_travail_t* travail)
    ;;     {
    ;;         lse_cmd_parser_t parser;
    ;;         lse_erreur_t erreur=lse_ok;
    ;;         lse_chaine_t ligne;
    ;;         int identification;
    ;;         int disponible=lse_fichiers_temporaire_disponible();
    ;;         int requis=9999; /* SEE Ajouter requis au fichier compt */
    ;; 
    ;;         lse_chaine_initialiser(&ligne);
    ;;         lse_ecrire_format(travail," \a");
    ;;         lse_lire_ligne(travail,&ligne,0,mXOFF); 
    ;;         lse_cmd_arg_initialiser(&parser,&ligne); 
    ;;         erreur=lse_cmd_analyser_un_numero(&parser,&identification);
    ;;         if(erreur!=lse_ok){
    ;;             /* mais que dit il ? on ne change pas de compte */
    ;;             lse_ecrire_format(travail,"\r\n???");
    ;;             return;
    ;;         }
    ;;         
    ;;         /*
    ;;             SEE: check that we're not wargamed.
    ;;             
    ;; 
    ;;             if(id_locked_for_address(identification,remote_address)){
    ;;                 (* on ne change pas de compte *)
    ;;                 lse_ecrire_format(travail,"\r\n???");
    ;;                 return;
    ;;             }
    ;; 
    ;;             we need to keep for each id the list 
    ;;             of (remote_address,number of invalid try)
    ;; 
    ;;             number of invalid try is reset to 0 once a good id is given.
    ;; 
    ;; 
    ;;             maxtry=3
    ;;             once maxtry unsuccessfull ID has been issued for a given 
    ;;             account from a given remote_address, lock the account for 
    ;;             this address.
    ;;             
    ;;         */
    ;; 
    ;;         if(!lse_compte_verifier_identification(identification)){
    ;;             /* on ne change pas de compte */
    ;;             lse_ecrire_format(travail,"\r\n???");
    ;;             return;
    ;;         }
    ;; 
    ;;         lse_fichier_terminer(travail->compte,-1);
    ;;         travail->compte=identification%100;
    ;;         /* SEE requis=lse_compte_espace_temporaire_requis(identification); */
    ;;         if(lse_compte_verifier_droit(identification,
    ;;                                      lse_compte_droit_tempofixe)){
    ;;             int desire;
    ;;             lse_ecrire_format(travail,"\r\nESPACE TEMPORAIRE DISPONIBLE : %d",
    ;;                               disponible);
    ;;             lse_ecrire_format(travail,"\r\nESPACE TEMPORAIRE DESIRE     :");
    ;;             lse_cmd_lire_ligne(travail,&ligne);
    ;;             lse_cmd_arg_initialiser(&parser,&ligne); 
    ;;             erreur=lse_cmd_analyser_un_numero(&parser,&desire);
    ;;             if((erreur==lse_ok)&&(0<desire)&&(desire<=disponible)){
    ;;                 erreur=lse_fichier_allouer_temporaire(travail->console,desire);
    ;;             }else{
    ;;                 lse_ecrire_format(travail,"\r\n???");
    ;;             }
    ;;         }else if(disponible<requis
    ;;                  /*cas ou  l'identification demandée correspondait a
    ;;                    un espace temporaire supérieur à celui restant*/){
    ;;             lse_ecrire_format(travail,
    ;;                               "\r\nESPACE TEMPORAIRE REDUIT A %d SECTEURS.",
    ;;                               disponible);
    ;;             erreur=lse_fichier_allouer_temporaire(travail->console,disponible);
    ;;         }
    ;;         if(erreur!=lse_ok){
    ;;             lse_erreur_rapporter(travail,erreur);
    ;;         }
    ;;     }/*lse_cmd_identification*/
    )

  )



(defun account-check-right (account fictype)
  (values))
(defun account-set-right (account fictype right)
  (values))



(define-command-group sleeping (common)

 
  
  (defcommand "DROITS"  nil ()
    "Gestion des droits d'accès des comptes."
    (io-format *task* " ")
    (unless (string= (io-read-line *task* :echo nil) *PASSWORD*)
      (lse-error "IDENTIFIANT INVALIDE")
      (return-from droits))
    (io-new-line *task*) (io-format *task* "            ANCIEN  NOUVEAU  ") ;
    (io-new-line *task*) (io-format *task* "NO.COMPTE    PDAF     PDAF   ") ;
    (loop
      :with account = -1
      :for line = (progn (io-new-line *task*)
                         (io-format *task* "COMPTE NO. : ") ;
                         (io-read-line *task*))
      :until (string= line "FIN")
      :do
      (setf account (if (zerop (length line))
                        (mod (1+ account) 100)
                        (parse-integer line :junk-allowed nil)))
      (when (<= 0 account 99)
        (let ((op (account-check-right account :programme))
              (od (account-check-right account :permanent))
              (oa (account-check-right account :tempoauto))
              (of (account-check-right account :tempofixe))
              np nd na nf)
          (io-carriage-return *task*)
          (io-format *task* 
                     "   ~2,'0D       ~{~[0~;1~]~}     "
                     account (list op od oa of))
          (setf line (io-read-line *task*))
          (if (and (= 4 (length line))
                   (every (lambda (ch) (position ch "01")) line))
              (progn
                (setf np (char= (character "1") (aref line 0))
                      nd (char= (character "1") (aref line 1))
                      na (char= (character "1") (aref line 2))
                      nf (and (not na)
                              (char= (character "1") (aref line 3))))
                (account-set-right account :programme np)
                (account-set-right account :permanent nd)
                (account-set-right account :tempoauto na)
                (account-set-right account :tempofixe nf))
              (setf np op  nd od  na oa  nf of))
          (io-carriage-return *task*)
          (io-format *task*
                     "   ~2,'0D       ~{~[0~;1~]~}      ~{~[0~;1~]~}    "
                     account (list op od oa of) (list np nd na nf)) )))
    (io-new-line *task*))


  (defcommand "BONJOUR" nil ()
    "Activation du mode de travail."
    (io-new-line *task* 2)
    (io-format *task* "LSE-M15  CONSOLE NO.~2D  ~A" 
               (task-console *task*) (dat))
    (setf (task-state *task*) :active))


  (defcommand "ADIEUX" nil ()
    "Déconnexion."
    (io-new-line *task*)
    (task-disconnect *task*))

  ) ;; sleeping





(define-condition au-revoir (condition) ; not an error.
  ())

(defun au-revoir ()
  (io-format *task* "  ~8A" (subseq (dat) 9))
  (io-new-line *task* 3)
  (io-finish-output *task*)
  (task-close-all-files *task*)
  (setf (task-state *task*) :sleeping)
  (catalog-delete-temporaries (task-console *task*))
  (signal 'au-revoir))


(defun abreger ()
  (io-new-line *task*)
  (setf (task-abreger   *task*) t))

(defun in-extenso ()
  (io-new-line *task*)
  (setf (task-abreger   *task*) nil))

(defun pas-a-pas ()
  (io-new-line *task*)
  (setf (vm-pas-a-pas (task-vm *task*)) t))

(defun normal ()
  (io-new-line *task*)
  (setf (vm-pas-a-pas (task-vm *task*)) nil))



(defun lister-a-partir-de (from to)
  (io-new-line *task*)
  (io-format *task* "~{~A~%~}"
             (mapcar (function code-source)
                     (get-program (task-vm *task*) from to))))


(defun numero-a-partir-de (from to)
  (io-new-line *task*)
  (io-format *task* "~{~A~%~}"
             (mapcar (function code-line)
                     (get-program (task-vm *task*) from to))))


(defun desassembler-a-partir-de (from to)
  (io-new-line *task*)
  (let ((lines '()))
    (maphash (lambda (lino code)
               (when (and (<= from lino) (or (null to) (<= lino to)))
                 (push (list lino
                             (code-source code)
                             (with-output-to-string (*standard-output*)
                               (with-standard-io-syntax
                                 (disassemble-lse (code-vector code)))))
                       lines)))
             (vm-code-vectors (task-vm *task*)))
    (io-format *task* "~:{~*~A~%~A~%~}" (sort lines '< :key (function first)))))


(defun ruban ()
  (io-new-line *task*)
  (silence)
  (io-start-tape-reader *task*))


(defun punch (string)
  (loop
    :for ch :across string
    :for code = (logand (char-code ch) #xff)
    :for bin = (loop
                 :repeat 8
                 :for i = 128 :then (ash i -1)
                 :collect (if (zerop (logand i code))
                              #\space #\o)
                 :when (= i 8) :collect #\.)
    :do (io-format *task* "~{~A~}~%" bin)))

(defun perforer-a-partir-de (from to)
  (io-new-line *task*)
  (io-start-tape-puncher *task*)
  (unwind-protect (lister-a-partir-de from to)
    (io-stop-tape-puncher *task*))
  (punch "PERFORATION EFFECTUEE")
  (io-format *task* "~&PERFORATION EFFECTUEE.~%"))


(defun selectionner-ruban (ruban)
  (io-new-line *task*)
  (let ((path (catalog-pathname ruban "R")))
    (when (task-tape-input *task*)
      (close (task-tape-input *task*)))
    (setf (task-tape-input *task*) (open path :if-does-not-exist :error))
    (io-format *task* "~&LE RUBAN ~:@(~A~) (~A) EST MIS EN PLACE.~%"
               ruban (read-line (task-tape-input *task*)))))


(defun archiver-ruban (ruban)
  (io-new-line *task*)
  (let ((dst-path (catalog-pathname ruban "R"))
        (src (task-tape-output *task*)))
    (file-position src 0)
    (with-open-file (dst dst-path
                         :direction :output
                         :if-exists nil
                         :if-does-not-exist :create)
      (if dst
          (progn
            (io-format *task* "~&COMMENTAIRE A ECRIRE SUR LE RUBAN AU FEUTRE (UNE LIGNE) :~%")
            (write-line (io-read-line *task*) dst)
            (copy-stream src dst)
            (close src)
            (setf (task-tape-output *task*) nil))
          (error 'lse-file-error
                 :pathname dst-path
                 :format-control "IL Y A DEJA UN RUBAN NOMME '~:@(~A~)'."
                 :format-arguments (list ruban))))))


(defun etagere-de-rubans (chemin)
  (io-new-line *task*)
  (when (and (stringp chemin)
             (< 1 (length chemin))
             (char/= #\/ (aref chemin (1- (length chemin)))))
    (setf chemin (concatenate 'string chemin "/")))
  (setf *current-shelf*
        (truename (make-pathname
                   :name nil :type nil :version nil
                   :defaults (merge-pathnames chemin *current-directory*))))
  (io-format *task* "~&L'ETAGERE DE RUBAN COURANTE EST: ~A~%" *current-shelf*)
  (lister-rubans))




(defun lister-rubans ()
  (let* ((files  (directory (make-pathname :name :wild
                                           :type (cdr (assoc :tape *file-types*))
                                           :version nil
                                           :defaults *current-shelf*)))
         (width  (reduce (function max) files
                         :key (lambda (x) (length (pathname-name x)))
                         :initial-value 5))
         (comment ""))
    (io-format *task* "~%RUBANS PERFORES SUR L'ETAGERERE~
                     ~%*********************************~
                     ~2% NOM      DATE    TAILLE  COMMENTAIRE~
                     ~2%")
    (dolist (file files)
      (io-format *task* "~VA  ~8A  ~7D  ~A~%"
                 width
                 (string-upcase (pathname-name file))
                 (subseq (formate-date (file-write-date file)) 0 8)
                 (or (ignore-errors
                       (progn
                         (setf comment "")
                         (with-open-file (stream file :if-does-not-exist nil)
                           (setf comment (read-line stream))
                           (- (file-length stream) (file-position stream)))))
                     0)
                 comment))
    (io-new-line *task*)))




(defun silence ()
  (io-new-line *task*)
  (setf (io-echo      *task*) nil)
  (setf (task-silence *task*) t))


(defun unsilence ()
  (io-new-line *task*)
  (setf (io-echo      *task*) t)
  (setf (task-silence *task*) nil))



(defun executer-a-partir-de (from to)
  (io-new-line *task*)
  (let ((vm (task-vm *task*)))
    (unless (or (null to) (vm-line-exist-p vm to))
      (error-bad-line to))
    (when (vm-line-exist-p vm from)
      (vm-reset-variables vm))
    (setf (vm-trap-line vm) to)
    (catch 'run-step-done (vm-goto vm from))
    (vm-run vm)))


(defun continuer ()
  (io-new-line *task*)
  (let ((vm (task-vm *task*)))
    (if (vm-pausedp vm)
        (progn
          (setf (vm-trap-line vm) nil)
          (vm-unpause vm)
          (vm-run vm))
        (error 'lse-error
               :format-control "ON NE PEUT PAS CONTINUER UN PROGRAMME QUI N'EST PAS EN PAUSE."))))


(defun reprendre-a-partir-de (from to)
  (io-new-line *task*)
  (let ((vm (task-vm *task*)))
    (unless (or (null to) (vm-line-exist-p vm to))
      (error-bad-line to))
    (when (vm-line-exist-p vm from)
      (vm-reset-stacks vm))
    (setf (vm-trap-line vm) to)
    (catch 'run-step-done (vm-goto vm from))
    (vm-run vm)))


(defun poursuivre-jusqu-en (linum)
  (io-new-line *task*)
  (let ((vm (task-vm *task*)))
    (unless (or (null linum) (vm-line-exist-p vm linum))
      (error-bad-line linum))
    (if (vm-pausedp vm)
        (progn
          (setf (vm-trap-line vm) linum)
          (vm-unpause vm)
          (vm-run vm))
        (error 'lse-error
               :format-control "ON NE PEUT PAS POURSUIVRE UN PROGRAMME QUI N'EST PAS EN PAUSE."))))


(defun prendre-etat-console (console-no)
  (declare (ignore console-no))
  (io-new-line *task*)
  ;;           verifier le numero correspond a une console existante.
  ;;           Si la zone temporaire locale < taille des fichiers dans la zone
  ;;           temporaire de la console a prendre :
  ;;           "TRANSFERT FICHIERS TEMPORAIRES IMPOSSIBLE"
  (lse-error "CETTE COMMANDE N'EST PAS IMPLEMENTEE DANS LA VERSION UNIX DU SYSTEME L.S.E."))




(defparameter *file-types*
  '((:program        . "lse")
    (:data           . "don")
    (:temporary      . "don")
    (:tape           . "rub")
    (:temporary-tape . "rub")))


(defparameter *current-directory* (truename #P"./"))
(defparameter *current-shelf*     (truename #P"./"))


(defun make-temporaries-directory ()
  (let ((dir (pathname (format nil "/tmp/lse~D/" (getuid)))))
    (ensure-directories-exist (make-pathname :name "test" :type "test" :defaults dir))
    dir))

(defun normalize-fictype (fictype)
  (cond
    ((string-equal fictype "P") :program)
    ((string-equal fictype "D") :data)
    ((string-equal fictype "T") :temporary)
    ((string-equal fictype "R") :tape)
    ((string-equal fictype "S") :temporary-tape)
    ((member fictype '(:program :data :temporary :tape :temporary-tape)) fictype)
    (t (error 'lse-error
              :format-control "INDICATEUR DE TYPE DE FICHIER INVALIDE: ~A; ATTENDU: P, D ou T."
              :format-arguments (list fictype)))))

(defun catalog-pathname (name fictype)
  (let ((fictype (normalize-fictype fictype))
        (name (string-downcase name)))
    (make-pathname :name name
                   :type (cdr (assoc fictype *file-types*))
                   :case :local
                   :defaults (case fictype
                               ((:temporary :temporary-tape) (make-temporaries-directory))
                               (:tape                        *current-shelf*)
                               (otherwise                    *current-directory*)))))

;; (catalog-pathname "BOUR" "P") --> #P"./bour.lse"
;; (catalog-pathname "BOUR" "D") --> #P"./bour.don"
;; (catalog-pathname "BOUR" "T") --> #P"/tmp/lse1000/bour.don"


(defun appeler (pgm)
  (io-new-line *task*)
  (let* ((path      (catalog-pathname pgm :p))
         (vm        (task-vm *task*)))
    (vm-terminer vm)
    (replace-program vm (compile-lse-file path pgm))
    (values)))


(defun ranger (pgm)
  (io-new-line *task*)
  (let* ((path      (catalog-pathname pgm :p))
         (vm        (task-vm *task*))
         (source    (get-program vm 1 nil)))
    (if source
        (with-open-file (stream path
                                :direction :output
                                :if-exists nil
                                :if-does-not-exist :create)
          (if stream
              (loop
                :for line :in source
                :do (write-line (code-source line) stream))
              (error 'lse-file-error
                     :pathname path
                     :format-control "UN PROGRAMME NOMME '~A' EXISTE DEJA; UTILISEZ LA COMMANDE MODIFIER."
                     :format-arguments (list pgm))))
        (error 'lse-error
               :format-control "IL N'Y A PAS DE PROGRAMME A RANGER."
               :format-arguments '()))
    (values)))


(defun modifier (pgm)
 (io-new-line *task*)
   (let* ((path      (catalog-pathname pgm :p))
         (vm        (task-vm *task*))
         (source    (get-program vm 1 nil)))
    (if source
        (with-open-file (stream path
                                :direction :output
                                :if-exists :supersede
                                :if-does-not-exist :create)
          (loop
            :for line :in source
            :do (write-line (code-source line) stream)))
        (error 'lse-error
               :format-control "IL N'Y A PAS DE PROGRAMME A MODIFIER."
               :format-arguments '()))
    (values)))



(eval-when (:compile-toplevel :load-toplevel :execute)
 (defparameter *xoff* (code-char xoff))
 (defparameter *nul*  (code-char nul)))

(defun split-size (string size)
  (loop
    :with len = (length string)
    :for start :below len :by size
    :collect (subseq string start (min (+ start size) len))))

(defun decoder (fichier from to)
  (io-new-line *task*)
  (let* ((path      (catalog-pathname fichier :T))
         (vm        (task-vm *task*))
         (source    (get-program vm from to)))
    (if source
        (let ((buffer (unsplit-string (mapcar (function code-source) source)
                                      *xoff*
                                      :fill-pointer t :size-increment 2)))
          (vector-push *xoff* buffer)
          (vector-push *nul*  buffer)
          (let ((file (lse-data-file-open path
                                          :if-exists :supersede
                                          :if-does-not-exist :create)))
            (unwind-protect
                 (loop
                   :for rn :from 1
                   :for chunk :in (split-size buffer *max-record-chaine-size*)
                   :do (write-record file rn chunk))
              (lse-data-file-close file))))
        (error 'lse-error
               :format-control "IL N'Y A PAS DE PROGRAMME A DECODER."
               :format-arguments '()))
    (values)))


(defun encoder (fichier from to)
  (io-new-line *task*)
  (let* ((path      (catalog-pathname fichier :T))
         (vm        (task-vm *task*)))
    (let ((file (lse-data-file-open path :if-does-not-exist nil)))
      (if file
          (let ((source (nsubstitute-if #\Newline
                                        (lambda (ch) (or (char= ch *xoff*)
                                                         (char= ch *nul*)))
                                        (unsplit-string
                                         (unwind-protect
                                              (loop
                                                :for rn :from 1
                                                :for chunk = (read-record file rn)
                                                :collect chunk
                                                :until (char= (aref chunk (1- (length chunk))) *nul*))
                                           (lse-data-file-close file))
                                         ""))))
            (io-format *task* "~2&~A~2&" source)
            (with-input-from-string (stream source)
              (dolist (line (compile-lse-stream stream))
                ;; ENCODER merges the lines.
                (when (and (<= from (first line))
                           (or (null to)
                               (<= (first line) to)))
                  (put-line vm (first line) line)))))
          (error 'lse-file-error
                 :pathname path
                 :format-control "IL N'Y A PAS DE FICHIER TEMPORAIRE '~A' A ENCODER."
                 :format-arguments (list fichier))))))




;; (directory "./*.lse")
;; (appeler "BOURG")
;; (appeler "testcomp")
;; (modifier "SAVE1")
;; (lister-a-partir-de 1 nil)



(defun effacer-lignes (liste-de-numeros)
  (io-new-line *task*)
  (let ((vm (task-vm *task*)))
    (if (eql :all liste-de-numeros)
        (when (minimum-line-number vm)
          (loop
            :for lino :from (minimum-line-number vm)
            :to (maximum-line-number vm)
            :do (erase-line-number vm lino)))
        (dolist (item liste-de-numeros)
          (if (consp item)
              (loop
                :for lino :from (min (car item) (cdr item)) :to (max (car item) (cdr item))
                :do (erase-line-number vm lino))
              (erase-line-number vm item))))))

(defun eliminer-commentaires ()
  (io-new-line *task*)
  (error 'pas-implemente
         :what 'eliminer-commentaires))


(defun cataloguer (temporaire permanent)
  (io-new-line *task*)
  (let ((src-path (catalog-pathname temporaire :t))
        (dst-path (catalog-pathname permanent  :d)))
    (with-open-file (src src-path
                         :direction :input
                         :element-type '(unsigned-byte 8)
                         :if-does-not-exist nil)
      (if src
          (with-open-file (dst dst-path
                               :direction :output
                               :element-type '(unsigned-byte 8)
                               :if-exists nil
                               :if-does-not-exist :create)
            (if dst
                (copy-stream src dst)
                (error 'lse-file-error
                       :pathname dst-path
                       :format-control "IL Y A DEJA UN FICHIER PERMANENT '~A'."
                       :format-arguments (list permanent))))
          (error 'lse-file-error
                 :pathname src-path
                 :format-control "IL N'Y A PAS DE FICHIER TEMPORAIRE '~A' A CATALOGUER."
                 :format-arguments (list temporaire))))))


(defun supprimer-tous-les-fichiers-temporaires ()
  (io-new-line *task*)
  (dolist (path (directory (make-pathname :name :wild
                                          :type (cdr (assoc :temporary *file-types*))
                                          :case :local
                                          :defaults (make-temporaries-directory))))
    (delete-file path))
  (values))


(defun supprimer (fichier stype)
  (io-new-line *task*)
  (let ((path (catalog-pathname fichier stype)))
    (delete-file path))
  (values))




(defun changer-repertoire (chemin)
  (io-new-line *task*)
  (when (and (stringp chemin)
             (< 1 (length chemin))
             (char/= #\/ (aref chemin (1- (length chemin)))))
    (setf chemin (concatenate 'string chemin "/")))
  (setf *current-directory*
        (truename (make-pathname
                   :name nil :type nil :version nil
                   :defaults (merge-pathnames chemin *current-directory*))))
  (task-close-all-files *task*))


(defun afficher-repertoire-courant ()
  (io-new-line *task*)
  (io-format *task* "REPERTOIRE COURANT: ~A~%" *current-directory*))



(defun table-des-fichiers ()
  (io-format *task* "  ~A~%" (dat))
  (flet ((list-files (type directory control-string modulo)
           (let* ((files  (directory (make-pathname :name :wild
                                                    :type (cdr (assoc type *file-types*))
                                                    :version nil
                                                    :defaults directory)))
                  (width  (reduce (function max) files
                                  :key (lambda (x) (length (pathname-name x)))
                                  :initial-value 5)))
             (dolist (file files)
               (io-format *task* control-string
                          width
                          (string-upcase (pathname-name file))
                          (task-account *task*)
                          (subseq (formate-date (file-write-date file)) 0 8)
                          (truncate (or (ignore-errors
                                          (with-open-file (stream file
                                                                  :direction :input
                                                                  :element-type '(unsigned-byte 8)
                                                                  :if-does-not-exist nil)
                                            (file-length stream)))
                                        0)
                                    modulo))))))
    
    (io-format *task* "~%FICHIERS-PROGRAMMES~
                     ~%*******************~
                     ~2% NOM NO.COMPTE  DATE  NB.MOTS~
                     ~2%")
    (list-files :program *current-directory* "~VA  ~2,'0D    ~8A ~5D~%" 2)

    (io-format *task* "~%FICHIERS-PERMANENTS~
                     ~%*******************~
                     ~2% NOM NO.COMPTE  DATE  NB.SECTEURS~
                     ~2%")
    (list-files :data *current-directory* "~VA  ~2,'0D    ~8A ~5D~%" +block-size+)

    (io-format *task* "~%FICHIERS-DONNEE TEMPORAIRES~
                     ~%*****************************~
                     ~2% NOM CONSOLE NB.SECTEURS~
                     ~2%")
    (list-files :temporary (make-temporaries-directory) "~VA  ~2,'0D~*     ~5D~%" +block-size+)
    (io-new-line *task*)
    (values)))


(defun free-sectors ()
  88485460)

(defun allocated-temporary-space ()
  '((1 20)
    (2 20)
    (3 3)
    (4 3)
    (5 3)
    (6 3)
    (7 3)
    (8 3)
    (16 20)))

(defun utilisation-disque ()
  (table-des-fichiers)
  (io-format *task* "~%NOMBRE DE SECTEUR LIBRES:~D~
                     ~%*************************~
                     ~2%" (free-sectors))
  (io-format *task* "~%ESPACE TEMPORAIRE ALLOUE~
                     ~%************************~
                     ~2%CONSOLE NB.SECTEURS~
                     ~2%")
  (io-format *task* "~:{ ~2,'0D       ~5D~%~}" (allocated-temporary-space))
  (io-new-line *task*)
  (values))




;;  We provide a REPL for LE (repl) command for debugging.

(defmacro handling-errors (&body body)
  `(HANDLER-CASE (progn ,@body)
     (simple-condition 
         (ERR) 
       (io-format *task* "~&~A: ~%" (class-name (class-of err)))
       (apply (function io-format) *task*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (io-format *task* "~&"))
     (condition 
         (ERR) 
       (io-format *task* "~&~A: ~%  ~S~%"
                  (class-name (class-of err)) err))))


(defun repl ()
  (do ((+eof+ (gensym))
       (hist 1 (1+ hist)))
      (nil)
    (io-format *task* "~%~A[~D]> " (package-name *package*) hist)
    (handling-errors
     (setf +++ ++   ++ +   + -   - (read *standard-input* nil +eof+))
     (when (or (eq - +eof+)
               (member - '((quit)(exit)(continue)) :test (function equalp)))
       (return-from repl))
     (setf /// //   // /   / (multiple-value-list (eval -)))
     (setf *** **   ** *   * (first /))
     (io-format *task* "~& --> ~{~S~^ ;~%     ~}~%" /))))




(define-command-group awake (common)

  (defcommand "AU REVOIR" nil ()
    "Efface les fichiers temporaires, et passe à l'état dormant."
    #+lse-unix "La commande AU REVOIR annonce au système qu'il
peut effacer les fichiers temporaires, et quitter le système L.S.E."
    (au-revoir))

  (defcommand "ABREGER"     nil ()
    "Ne complète pas l'affichage des commandes."
    (abreger))

  (defcommand "IN EXTENSO"  nil ()
    "Annule la commande ABREGER: complète l'affichage des commandes."
    (in-extenso))

  (defcommand "PAS A PAS"   nil ()
    "Exécution du programme pas-à-pas."
    #+lse-unix "Peut être utilisée avant EXECUTER, CONTINUER, REPRENDRE.

Fait arrêter l'exécution au début de chaque ligne.  La console repasse
alors dans l'état «moniteur» et affiche le numéro de la ligne
atteinte.

Pour faire continuer l'exécution il suffit de frapper RET mais on peut
aussi utiliser toute autre commande ou le mode «machine de bureau»;
pour revenir à l'exécution du programme, il faudra alors utiliser la
commande CONTINUER.
"
    (pas-a-pas))
  
  (defcommand "NORMAL"      nil ()
    "Annule la commande PAS A PAS."
    (normal))

  (defcommand "LISTER A PARTIR DE"   deux-numeros-optionels (from to)
    "Affiche le programme courant."
    (lister-a-partir-de from to))

  (defcommand "ETAGERE DE RUBANS"  une-ligne (chemin) 
    "Selectionne une étagère de rubans perforés."
    (etagere-de-rubans chemin))

  (defcommand "SELECTIONNER RUBAN" un-fichier (ruban)
    "Selectionne un ruban de l'étagère et le place dans le lecteur de ruban."
    (selectionner-ruban ruban))

  (defcommand "RUBAN" nil ()
    "Active la lecture du ruban perforé."
    (ruban))

  (defcommand "PERFORER A PARTIR DE" deux-numeros-optionels (from to)
    "Perfore le programme courant sur ruban perforé."
    (perforer-a-partir-de from to))
  
  (defcommand "ARCHIVER RUBAN" un-fichier (ruban)
    "Nomme le ruban qui vient d'être perforé et l'archive sur l'étagère."
    (archiver-ruban ruban))


  #+developing
  (defcommand "LE EVALUER UNE EXPRESSION LISP " une-ligne (ligne)
    (io-new-line *task*)
    (let* ((*vm* (task-vm *task*))
           (results)
           (output (with-output-to-string (*standard-output*)
                     (handler-case
                         (setf results (multiple-value-list
                                        (eval (let ((*package* (find-package "COM.INFORMATIMAGO.LSE"))
                                                    (*print-right-margin* 80))
                                                (read-from-string ligne)))))
                       (error (err)
                         (format t "~%ERROR: ~A~%" err)
                         (setf results nil))))))
      (io-format *task* "~%~A~%~@[=> ~{~S~%~^   ~}~]" output results)))
  
  #+developing
  (defcommand "LD DEASSEMBLER A PARTIR DE "     deux-numeros-optionels (from to)
    "Commande de deboguage: Désassemble les lignes de programme."
    (io-new-line *task*)
    (desassembler-a-partir-de from to))

  #+developing
  (defcommand "LP DEASSEMBLER/PERFORER A PARTIR DE "  deux-numeros-optionels (from to)
    "Commande de deboguage: Désassemble les lignes de programme."
    (io-new-line *task*)
    (io-start-tape-puncher *task*)
    (unwind-protect (desassembler-a-partir-de from to)
      (io-stop-tape-puncher *task*))
    (punch "PERFORATION EFFECTUEE")
    (io-format *task* "~%PERFORATION EFFECTUEE.~%"))


  (defcommand "NUMERO A PARTIR DE"   deux-numeros-optionels (from to)
    "Affiche les numéros de lignes utilisés."
    (numero-a-partir-de from to))


  (defcommand "SILENCE"  nil ()
    "Supprime l'affichage de tout ce que l'utilisateur tape au clavier.
L'effet de cette commande est annulé par la touche ESC."
    (silence))

  (defcommand "UNSILENCE"  nil ()
    "Active l'affichage de tout ce que l'utilisateur tape au clavier."
    (unsilence))


  (defcommand "EXECUTER A PARTIR DE" deux-numeros-optionels (from to)
    "Exécute le programme."
    (executer-a-partir-de from to))


  (defcommand "CONTINUER"             nil ()
    "Continue l'exécution du programme après une pause."
    (continuer))


  (defcommand "REPRENDRE A PARTIR DE" deux-numeros-optionels (from to)
    "Reprend l'exécution du programme après une pause."
    (reprendre-a-partir-de from to))


  (defcommand "POURSUIVRE JUSQU'EN"   numero-de-ligne (linum)
    "Continue l'exécution du programme jusqu'à la ligne indiquée."
    (poursuivre-jusqu-en linum))


  #-lse-unix
  (defcommand "PRENDRE ETAT CONSOLE"  un-numero (consnum)
    "Copie le programme courant et les fichiers temporaires de la console indiquée."
    "Cette commande a pour effet de transférer à l'utilisateur le
programme d'un autre utilisateur travaillant sur la console numéro
N. Le programme ainsi transféré se trouve dans l'état où l'utilisateur
l'avait en mémoire.

Sur T1600 cette commande ne transfère que le programme, les variables étant dans l'état non défini.
Sur MITRA 15, l'état des variables est également transféré."
    (prendre-etat-console consnum))


  (defcommand "APPELER"  un-programme (pgm)
    "Charge un fichier programme en mémoire."
    (appeler pgm))

  (defcommand "RANGER" un-programme (pgm)
    "Enregistre le programme courant dans un nouveau fichier programme."
    (ranger pgm))
  
  (defcommand "MODIFIER"  un-programme (pgm)
    "Enregistre le programme courant dans un fichier programme existant."
    (modifier pgm))

  
  (defcommand "ENCODER" un-fichier-et-deux-numeros (fichier &optional (from 1) (to nil))
    "Charge un programme à partir d'un fichier donnée."
    (encoder fichier from to))


  (defcommand "DECODER" un-fichier-et-deux-numeros (fichier &optional (from 1) (to nil))
    "Enregistre le programme dans un fichier donnée."
    (decoder fichier from to))
  

  (defcommand "EFFACER LIGNES" liste-de-numeros (liste-de-numeros)
    "Efface les lignes indiquées."
    (effacer-lignes liste-de-numeros))
  

  (defcommand "ELIMINER COMMENTAIRES"  nil ()
    "Elimine les commentaires."
    (eliminer-commentaires))

  
  (defcommand "CATALOGUER"  deux-fichiers (temporaire permanent)
    "Copie un fichier temporaire dans un fichier permanent."
    (cataloguer temporaire permanent))

  
  (defcommand "SUPPRIMER" arguments-supprimer (fichier &optional fictype)
    "Supprime un fichier de type indiqué, ou supprime tous les fichiers temporaires (*)."
    (if (equal fichier '*)
        (supprimer-tous-les-fichiers-temporaires)
        (supprimer fichier fictype)))


  (defcommand "TABLE DES FICHIERS" nil ()
    "Liste la table des fichiers (répertoire courant, et fichiers temporaires)."
    (table-des-fichiers))


  (defcommand "UTILISATION DISQUE" nil ()
    "Liste la table des fichiers (répertoire courant, et fichiers temporaires)."
    (utilisation-disque))


  (defcommand "CHANGER REPERTOIRE" une-ligne (nouveau-repertoire)
    "Change le répertoire courant."
    (changer-repertoire nouveau-repertoire))

  (defcommand "AFFICHER REPERTOIRE COURANT" nil ()
    "Affiche le répertoire courant."
    (afficher-repertoire-courant))



  
  ) ;; awake



(defun sleeping-commands ()
  (mapcar (function command-name) (command-group-commands sleeping)))

(defun awake-commands ()
  (mapcar (function command-name) (command-group-commands awake)))




(defun lse-compile-and-execute (task line)
  (let* ((*task* task)
         (code (compile-lse-line line)))
    (if (zerop (code-line code))
        ;; instruction line
        (let ((vm (task-vm task)))
          (setf (vm-state vm) :running
                (vm-pc.line vm) 0
                (vm-pc.offset vm) 0
                (vm-code vm) (code-vector code))
          (vm-run vm)
          (io-new-line *task*))
        ;; program line
        (if (equalp #(!next-line) (code-vector code))
            (erase-line-number (task-vm task) (code-line code))
            (put-line (task-vm task) (code-line code) code)))))


(defun command-eval-line (task line)
  (let ((*task* task))
    (cond

      ((and (= 2 (length line))
            (alpha-char-p (aref line 0))
            (alpha-char-p (aref line 1)))
       ;; looks like a command
       (let* ((command-group (ecase (task-state task)
                               (:sleeping sleeping)
                               (:active   awake)))
              (*command-group* command-group)
              (command (find-command line command-group)))
         (if (null command)
             (error 'lse-error
                    :format-control "COMMANDE INCONNUE ~S"
                    :format-arguments (list line))
             (progn
               (unless (task-silence task)
                 (io-carriage-return task)
                 (io-format task "~A " (if (task-abreger task)
                                           (subseq (command-name command) 0 2)
                                           (command-name command))))
               (command-call command)))))
      
      ((< 0 (length line))
       ;; soit une instruction, soit une ligne de programme...
       (io-new-line task)
       (if (task-state-awake-p task)
           (lse-compile-and-execute task line)
           (error "COMMANDE INVALIDE EN L'ETAT ~A~%ESSAYER LA COMMANDE AI(DE).~%"
                  (task-state-label (task-state task)))))

      #|else empty line, just ignore it.|#)))


(defvar *debug-repl* nil)
;; (setf *debug-repl* nil)
;; (setf *debug-repl* t)

(defun command-repl (task)
  (let ((*task* task)
        (*print-case* :upcase))
    (io-format task "~&PRET~%")
    (io-finish-output task)
    (handler-case
        (loop
          :while (task-state-awake-p task)
          :do (restart-case
                  (handler-case
                      (flet ((do-it ()
                               (setf (task-interruption task) nil)
                               ;; (unless (task-silence task)
                               ;;   (io-new-line task))
                               (io-finish-output *task*)
                               (handler-case
                                   (let ((line (io-read-line task :beep (not (task-silence task)))))
                                     (if (task-interruption task)
                                         (io-format *task* "~%PRET~%")
                                         (command-eval-line task line)))
                                 (end-of-file (err)
                                   (if (and (io-tape-input-p *task*)
                                            (eql (stream-error-stream err) (task-input *task*)))
                                       (io-stop-tape-reader *task*)
                                       (error err))))))
                        (if *debug-repl*
                            (handler-bind ((lse-error     (function signal))
                                           (scanner-error (function signal))
                                           (parser-error  (function signal))
                                           (file-error    (function signal))
                                           (error         (function invoke-debugger)))
                              (do-it))
                            (do-it)))
                    (scanner-error-invalid-character (err)
                      (io-format *task* "~%ERREUR: ~?~%"
                                 "CARACTERE INVALIDE '~a' EN POSITION ~D"
                                 (scanner-error-format-arguments err))
                      (io-format *task* "~%PRET~%")
                      (io-finish-output *task*))
                    (error (err)
                      (error-format *task* err)
                      (io-format *task* "~%PRET~%")
                      (io-finish-output *task*))
                    (user-interrupt (condition)
                      (io-format *task* "~%Condition: ~A~%" condition)
                      (io-format *task* "~%PRET~%")
                      (io-finish-output *task*)))
                (continue ()
                  :report "CONTINUER")))
      (au-revoir () (values)))))



;; (test/command-grammars)

;; Local Variables:
;; eval: (cl-indent 'defcommand 3)
;; End:
;;;; THE END ;;;;
