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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;****************************************************************************

(in-package "COM.INFORMATIMAGO.LSE")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Command line grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; :white-space = single space.
;; we don't keep newline because it's the eoln/end of statement
;; and we don't keep tabulation because it's not valid.


(defgrammar donnee-lse
    ;; Note: this is used to read numbers by LIRE (terminal-read).
    :terminals ((nombre " *[-+]?[0-9]+(.[0-9]+([Ee][-+]?[0-9]+?)?)? *($| .*)"))
    :skip-spaces nil
    :start donnee-lse
    :rules ((--> donnee-lse
                 nombre :action (multiple-value-list
                                 (read-from-string (second $1))))))

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


(eval-when (:compile-toplevel :load-toplevel :execute)

  (defparameter *syntaxes*
    '((numero-de-ligne              . "<N>")
      (un-numero                    . "<N>")
      (deux-numeros                 . "<N1>,<N2>")
      (deux-numeros-optionels       . "[<N1>[,<N2>]]")
      (liste-de-numeros             . "* | <N1>[,<N2>[...,<Nn>]...] | <N1> A <N2>")
      (un-programme                 . "<NOMPG>")
      (un-fichier                   . "<NOMFI>")
      (deux-fichiers                . "<NOMF1>,<NOMF2>")
      (un-fichier-et-deux-numeros   . "<NOMFI>[,<N1>[,<N2>]]")
      (une-ligne                    . "<TEXTE>")
      (arguments-supprimer          . "* | <NOMFI>,P|D|T")))

  (defun syntax (grammar-name)
    (cdr (assoc grammar-name *syntaxes*)))

  );;eval-when


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
;;; LSE Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defstruct command-group
  name
  supergroups
  commands)


(defstruct command
  initials
  name
  grammar
  parser
  arguments
  oneliner
  documentation
  function)

(defvar *command-groups* '()
  "An a-list mapping names to command-group structures.")

(defun command-group-named (name)
  (cdr (assoc name *command-groups*)))

(defun (setf command-group-named) (command-group name)
  (let ((entry (assoc name *command-groups*)))
    (if entry
        (setf (cdr entry) command-group)
        (push (cons name command-group) *command-groups*))))

(defgeneric find-command (command group &key in-extenso) 
  (:method ((command command) (group command-group) &key (in-extenso nil))
    (find-command (if in-extenso
                      (command-name command)
                      (command-initials command))
                  group :in-extenso in-extenso))
  (:method ((command string) (group command-group) &key (in-extenso nil))
    (or (find command (command-group-commands group)
              :test (function string-equal)
              :key (if in-extenso
                       (function command-name)
                       (function command-initials)))
        (some (lambda (supergroup) (find-command command (symbol-value supergroup)
                                                 :in-extenso in-extenso))
              (command-group-supergroups group)))))


(defmacro define-command-group (name supergroups)
  `(progn
     (defparameter ,name
       (make-command-group
        :name ',name
        :supergroups ',supergroups
        :commands '()))
     (setf (command-group-named ',name) ,name)
     ',name))



(defgeneric add-command-to-group (command group)
  (:documentation "Add the command to the group. Return COMMAND.")
  (:method ((command command) (group symbol))
    (let ((the-group  (command-group-named group)))
      (if the-group
          (add-command-to-group command the-group)
          (error "There's no command group named ~S" group))))
  (:method ((command command) (group command-group))
    (let* ((duplicate (find-command command group)))
      (when (and duplicate
                 (string/= (command-name command) (command-name duplicate)))
        (error "There is already a command with same initials as ~S: ~S."
               (command-name command)
               (command-name duplicate))))
    (setf (command-group-commands group)
          (nconc (command-group-commands group) (list command)))
    command))



(defmacro defcommand (name group grammar arguments &body body)
  (let ((oneliner      (if (stringp (first body))
                           (pop body)
                           nil))
        (docstring     (if (stringp (first body))
                           (pop body)
                           nil))
        (fname         (intern (substitute #\- #\space name)
                               "COM.INFORMATIMAGO.LSE"))
        (initials      (subseq name 0 2)))
    `(progn
       (defun ,fname ,arguments
         ,@body)
       (add-chapter ',initials
                    (defchapter (,name "COMMANDES" ,oneliner)
                        , (with-output-to-string (out)
                            (format out "~2%~A)~A ~@[~A~]~2%"
                                    initials
                                    (subseq name 2)
                                    (syntax grammar))
                            (write-documentation out (or docstring oneliner)))))
       (add-command-to-group
        (make-command
         :initials      ,initials
         :name          ,name
         :grammar       ',grammar ;; (when grammar (find-grammar (string grammar)))
         :parser        ,(when grammar
                               `(function ,(intern (with-standard-io-syntax
                                                     (format nil "PARSE-~A" grammar))
                                                   "COM.INFORMATIMAGO.LSE")))
         :arguments     ',arguments
         :oneliner      ,oneliner
         :documentation ,(or docstring oneliner)
         :function      (function ,fname))
        ',group)
       ',fname)))



(defvar *command-group* nil
  "The command-group the command being called belongs to.
Bound by COMMAND-EVAL-LINE.")


(defmethod command-call ((command command))
  (let ((gn (command-grammar command)))
    (if gn
        (apply (command-function command)
               (converting-parser-errors
                (funcall (command-parser command)
                         (io-read-line *task* :beep t))))
        (funcall (command-function command)))))


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





(define-command-group common   ())
(define-command-group sleeping (common))
(define-command-group awake    (common))


;;;---------------------------------------------------------------------
;;; Commands common to all states.
;;;---------------------------------------------------------------------


(defcommand "AIDER" common nil ()
  "Affiche la liste des commandes disponibles."
  "Affiche la liste des commandes disponibles.

Voir la commande: DOCUMENTATION."
  (io-format *task* "~%LES COMMANDES DISPONIBLES SONT:~2%")
  (let ((*print-right-margin* 80))
    (dolist (command (all-commands *command-group*))
      (io-format *task* "~A)~20A ~{~{~<~%~24<~>~1,80:;~A~> ~}~^~%~24<~>~}~%"
                 (command-initials command)
                 (subseq (command-name command) 2)
                 (mapcar (lambda (line) (split-sequence #\Space line))
                         (split-sequence #\Newline (process-doc (command-oneliner command)))))))
  (io-format *task* (process-doc "~%POUR ENTRER UNE COMMANDE, TAPEZ SES DEUX ~
                                    PREMIERES LETTRES, SUIVIES DE [XOFF].~
                                  ~%POUR ANNULER UN CARACTERE, TAPEZ [DEL].~%")))



(defcommand "DOCUMENTATION" common une-ligne (what)
  "Affiche la documentation d'une commande ou d'une instruction."
  "Affiche la documentation d'une commande ou d'une instruction.

Voir la commande: AIDER"
  (cond
    ((string-equal what "COMMANDES")
     (aider))
    #-(and)
    ((let ((command (or (find-command what *command-group* :in-extenso nil)
                        (find-command what *command-group* :in-extenso t))))
       (when command
         (io-format *task* "~2%~A)~A ~@[~A~]~2%"
                    (command-initials command)
                    (subseq (command-name command) 2)
                    (syntax (command-grammar command)))
         (write-documentation *task* (command-documentation command))
         t)))
    ((let ((chapters (find-chapter what)))
       (flet ((print-chapter (chapter)
                (io-format *task* "~2%~A ~A~%~V,,,'-<~>~2%"
                           (chapter-category chapter)
                           (chapter-title chapter)
                           (+ (length (chapter-title chapter))
                              1
                              (length (chapter-category chapter))))
                (if (stringp (chapter-text chapter))
                    (write-documentation *task* (chapter-text chapter))
                    (funcall (chapter-text chapter) chapter))
                t))
         (if (listp chapters)
             (dolist (chapter chapters t)
               (print-chapter chapter))
             (print-chapter chapters)))))
    (t
     (io-format *task*
                "~A"
                (process-doc
                 (format nil "~2%Il n'y a pas de documentation pour ~S~%~A~%"
                         what
                         "Essayez les commandes suivantes:
DO)CUMENTATION GARANTIE       indique qu'aucune garantie n'est assurée;
DO)CUMENTATION COPIE          donne la license et votre liberté de copier;
DO)CUMENTATION SOURCES        où trouver les sources de ce programme;
DO)CUMENTATION INSTRUCTIONS   donne la liste des instructions;
DO)CUMENTATION FONCTIONS      donne la liste des fonctions;
DO)CUMENTATION COMMANDES      donne la liste des commandes;
AI)DE                         donne la liste des commandes."))))))



#-lse-unix
(defcommand "IDENTIFICATION" common un-numero (identification)
  "Permet à l'utilisateur de s'identifier."
  (declare (ignore identification))
  (error 'pas-implemente :what 'identification)
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





(defun account-check-right (account fictype)
  (declare (ignore account fictype))
  (error 'pas-implemente :what "ACCOUNT-CHECK-RIGHT")
  (values))

(defun account-set-right (account fictype right)
  (declare (ignore account fictype right))
  (error 'pas-implemente :what "ACCOUNT-SET-RIGHT")
  (values))


#-lse-unix
(defcommand "DROITS" sleeping nil ()
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
    :until (string-equal line "FIN")
    :do
    (setf account (if (zerop (length line))
                      (mod (1+ account) 100)
                      (parse-integer line :junk-allowed nil)))
    (if (<= 0 account 99)
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
                     account (list op od oa of) (list np nd na nf)) )
        (io-format *task* "NO. DE COMPTE INVALIDE (DOIT ETRE UN NOMBRE ENTRE 00 et 99).~%")))
  (io-new-line *task*))


#-lse-unix
(defcommand "BONJOUR" sleeping nil ()
  "Activation du mode de travail."
  (io-new-line *task* 2)
  (io-format *task* "LSE-M15  CONSOLE NO.~2D  ~A" 
             (task-console *task*) (dat))
  (setf (task-state *task*) :active))

#-lse-unix
(defcommand "ADIEUX" sleeping nil ()
  "Déconnexion."
  (io-new-line *task*)
  (task-disconnect *task*))








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

;; (directory "./*.lse")
;; (appeler "BOURG")
;; (appeler "testcomp")
;; (modifier "SAVE1")
;; (lister-a-partir-de 1 nil)



(define-condition au-revoir (condition) ; not an error.
  ())

(defcommand "AU REVOIR" awake nil ()
  #+lse-unix "Efface les fichiers temporaires, et quite le système L.S.E."
  #-lse-unix "Efface les fichiers temporaires, et passe à l'état dormant."
  #+lse-unix "La commande AU REVOIR annonce au système qu'il
peut effacer les fichiers temporaires, et quitter le système L.S.E."
  #-lse-unix "La commande AU REVOIR efface les fichiers temporaires et
retourne à l'état dormant, en attente d'une nouvelle connexion (via la
commande BONJOUR)."
  (io-format *task* "  ~8A" (subseq (dat) 9))
  (io-new-line *task* 3)
  (io-finish-output *task*)
  (task-close-all-files *task*)
  (setf (task-state *task*) :sleeping)
  (catalog-delete-temporaries (task-console *task*))
  (signal 'au-revoir))


(defcommand "ABREGER" awake     nil ()
  "Ne complète pas l'affichage des commandes."
  "Ne complète pas l'affichage des commandes.

Voir la commande IN EXTENSO."
  (io-new-line *task*)
  (setf (task-abreger   *task*) t))

(defcommand "IN EXTENSO" awake  nil ()
  "Annule la commande ABREGER: complète l'affichage des commandes."
  "Annule la commande ABREGER: complète l'affichage des commandes.

Voir la commande ABREGER."
  (io-new-line *task*)
  (setf (task-abreger   *task*) nil))


;; Edition des programmes.

(defcommand "LISTER A PARTIR DE" awake   deux-numeros-optionels (from to)
  "Affiche le programme courant."
  "Fait afficher le programme courant de l'utilisateur, à partir de la
ligne N1 jusqu'à la ligne N2, si N2 est présent, sinon jusqu'à la fin.
En cours de listage, la touche d'interruption [ESC] peut être utilisée
pour l'arrêter.

Voir les commandes NUMERO A PARTIR DE, EFFACER LIGNES, PERFORER A PARTIR DE."
  (io-new-line *task*)
  (io-format *task* "~{~A~%~}~%"
             (mapcar (function code-source)
                     (get-program (task-vm *task*) from to))))

(defcommand "NUMERO A PARTIR DE" awake   deux-numeros-optionels (from to)
  "Affiche les numéros de lignes utilisés."
  "Fait afficher les numéros de lignes utilisés à partir du numéro N1
jusqu'au numéro N2, si N2 est présent, sinon jusqu'à la fin.

Voir les commandes LISTER A PARTIR DE, EFFACER LIGNES, PERFORER A PARTIR DE."
  (io-new-line *task*)
  (io-format *task* "~{~A~%~}"
             (mapcar (function code-line)
                     (get-program (task-vm *task*) from to))))


(defcommand "EFFACER LIGNES" awake liste-de-numeros (liste-de-numeros)
  "Efface les lignes indiquées."
  "Si * est donné, efface tout le programme courant de l'utilisateur.
Sinon supprime les lignes dont les numéros sont indiqués par N1, N2,
... Nn, ou les lignes de numéro N1 à N2 inclus, dans le programme
courant de l'utilisateur.

Voir les commandes LISTER A PARTIR DE, NUMERO A PARTIR DE, PERFORER A PARTIR DE."
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

#-(and)
(defcommand "ELIMINER COMMENTAIRES" awake  nil ()
  "Elimine les commentaires."
  (io-new-line *task*)
  (error 'pas-implemente
         :what 'eliminer-commentaires))


;; Commandes rubans perforés:

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


(defcommand "ETAGERE DE RUBANS" awake  une-ligne (chemin) 
  "Selectionne une étagère de rubans perforés."
  "Les rubans perforés sont simulés par des fichiers.  Ils sont
conservés sur des \"étagère\", c'est à dire, enregistrés dans des répertoires.

Cette commande permet de sélectionner le répertoire où les rubans
perforés sont enregistrés.

Voir les commandes SELECTIONNER RUBAN, RUBAN, PERFORER A PARTIR DE, ARCHIVER RUBAN.
"
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


(defcommand "SELECTIONNER RUBAN" awake un-fichier (ruban)
  "Selectionne un ruban de l'étagère et le place dans le lecteur de ruban."
  "Selectionne un ruban de l'étagère et le place dans le lecteur de ruban.

Voir les commandes ETAGERE DE RUBAN, RUBAN, PERFORER A PARTIR DE, ARCHIVER RUBAN."
  (io-new-line *task*)
  (let ((path (catalog-pathname ruban "R")))
    (when (task-tape-input *task*)
      (close (task-tape-input *task*)))
    (setf (task-tape-input *task*) (open path :if-does-not-exist :error))
    (io-format *task* "~&LE RUBAN ~:@(~A~) (~A) EST MIS EN PLACE.~%"
               ruban (read-line (task-tape-input *task*)))))


(defcommand "RUBAN" awake nil ()
  "Active la lecture du ruban perforé."
  "Cette commande déclenche le lecteur de ruban de la télétype.

Voir les commandes ETAGERE DE RUBAN, SELECTIONNER RUBAN, PERFORER A PARTIR DE, ARCHIVER RUBAN."
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
    :initially (io-format *task* "+~~~~~~~~~~~~~~~~~~+~%")
    :do        (io-format *task* "|~{~A~}|~%" bin)
    :finally   (io-format *task* "+~~~~~~~~~~~~~~~~~~+~%")))


(defcommand "PERFORER A PARTIR DE" awake deux-numeros-optionels (from to)
  "Perfore le programme courant sur ruban perforé."
  "Permet d'obtenir un ruban perforé comme support du programme
L.S.E. rentré à la console.

Cette commande simule le ruban perforé en écrivant sur un fichier
temporaire.  Le ruban ainsi perforé peut alors être archivé sur une
étagère avec la commande ARCHIVER RUBAN.

Le programme est perforé à partir de la ligne N1 jusqu'à la ligne N2,
si N2 est présent sinon jusqu'à la fin.

Voir les commandes ETAGERE DE RUBAN, SELECTIONNER RUBAN, RUBAN, ARCHIVER RUBAN."
  #-(and) "Permet d'obtenir un ruban perforé comme support du programme
L.S.E. rentré à la console.

Cette commande n'est effective que pour l'utilisateur travaillant sur
la télétype (TTY).

Le programme est perforé à partir de la ligne N1 jusqu'à la ligne N2,
si N2 est présent sinon jusqu'à la fin."
  (io-new-line *task*)
  (io-start-tape-puncher *task*)
  (unwind-protect (lister-a-partir-de from to)
    (io-stop-tape-puncher *task*))
  (punch "PERFORATION EFFECTUEE")
  (io-format *task* "~&PERFORATION EFFECTUEE.~%"))



(defcommand "ARCHIVER RUBAN" awake un-fichier (ruban)
  "Nomme le ruban qui vient d'être perforé et l'archive sur l'étagère."
  "Nomme le ruban qui vient d'être perforé et l'archive sur l'étagère.

Voir les commandes ETAGERE DE RUBAN, SELECTIONNER RUBAN, RUBAN, PERFORER A PARTIR DE."
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




(defcommand "SILENCE" awake  nil ()
  "Supprime l'affichage de tout ce que l'utilisateur tape au clavier."
  "Supprime l'affichage de tout ce que l'utilisateur tape au clavier.
L'effet de cette commande est annulé par la touche [ESC] ou par
commande ECHO.

Voir les commandes ECHO, RUBAN."
  (io-new-line *task*)
  (setf (task-silence *task*) t))


(defcommand "ECHO" awake  nil ()
  "Active l'affichage de tout ce que l'utilisateur tape au clavier."
  "Active l'affichage de tout ce que l'utilisateur tape au clavier.

Voir les commandes SILENCE, RUBAN."
  (io-new-line *task*)
  (setf (task-silence *task*) nil))



;; Execution:

(defcommand "PAS A PAS" awake   nil ()
  "Exécution du programme pas-à-pas."
  "Peut être utilisée avant EXECUTER, CONTINUER, REPRENDRE.

Fait arrêter l'exécution au début de chaque ligne.  La console repasse
alors dans l'état «moniteur» et affiche le numéro de la ligne
atteinte.

Pour faire continuer l'exécution il suffit de frapper RET mais on peut
aussi utiliser toute autre commande ou le mode «machine de bureau»;
pour revenir à l'exécution du programme, il faudra alors utiliser la
commande CONTINUER.

Voir les commandes NORMAL, CONTINUER, REPRENDRE A PARTIR DE, POURSUIVRE JUSQU'EN."
  (io-new-line *task*)
  (setf (vm-pas-a-pas (task-vm *task*)) t))


(defcommand "NORMAL" awake      nil ()
  "Annule la commande PAS A PAS."
  "Annule la commande PAS A PAS.

Voir les commandes PAS A PAS, CONTINUER, REPRENDRE A PARTIR DE, POURSUIVRE JUSQU'EN."
  (io-new-line *task*)
  (setf (vm-pas-a-pas (task-vm *task*)) nil))


(defcommand "EXECUTER A PARTIR DE" awake deux-numeros-optionels (from to)
  "Exécute le programme."
  "Fait passer la console de l'état «moniteur» à l'état «exécution».

Fait exécuter le programme courant de l'utilisateur à partir de la
ligne de numéro N1 (si aucune ligne de numéro N1 existe, une erreur
sera détectée).

L'exécution se poursuivra jusqu'à ce qu'on arrive :

- soit à la ligne de numéro N2 ; cette ligne ne sera pas exécutée, la
  console repassera dans l'état «mooniteur» et affichera N2.

- soit à l'une des instructions LSE : PAUSE ou TERMINER ou à une
  erreur.  La console repassera dans l'état «moniteur» et affichera un
  message indiquant la cause de l'arrêt et le numéro de la ligne où il
  s'est produit.  Éventuellement, en cas d'arrêt dans une procédure,
  le numéro de la ligne où l'on avait appelé cette procédure.

- L'utilisation de la touche d'interruption [ESC] arrête également
  l'exécution.

Voir les commandes PAS A PAS, NORMAL, CONTINUER, REPRENDRE A PARTIR DE, POURSUIVRE JUSQU'EN."
  (io-new-line *task*)
  (let ((vm (task-vm *task*)))
    (unless (or (null to) (vm-line-exist-p vm to))
      (error-bad-line to))
    (when (vm-line-exist-p vm from)
      (vm-reset-variables vm))
    (setf (vm-trap-line vm) to)
    (catch 'run-step-done (vm-goto vm from))
    (vm-run vm)
    (setf (task-pas-a-pas-first *task*) (task-pas-a-pas *task*))))


(defcommand "CONTINUER" awake             nil ()
  "Continue l'exécution du programme après une pause."
  "Permet de relancer l'exécution d'un programme momentanément
interroompu par l'instruction PAUSE ou la touche d'interruption
[ESC].  L'exécution reprend à l'endroit où elle fut arrêtée.

Voir les commandes PAS A PAS, NORMAL, EXECUTER A PARTIR DE, REPRENDRE A PARTIR DE, POURSUIVRE JUSQU'EN."
  (io-new-line *task*)
  (let ((vm (task-vm *task*)))
    (if (vm-pausedp vm)
        (progn
          (setf (vm-trap-line vm) nil)
          (vm-unpause vm)
          (vm-run vm)
          (setf (task-pas-a-pas-first *task*) (task-pas-a-pas *task*)))
        (error 'lse-error
               :format-control "ON NE PEUT PAS CONTINUER UN PROGRAMME QUI N'EST PAS EN PAUSE."))))


(defcommand "REPRENDRE A PARTIR DE" awake deux-numeros-optionels (from to)
  "Reprend l'exécution du programme après une pause."
  "Permet de reprendre l'exécution sur une ligne différente de celle
où elle fut interrompue.

- Les parametres N1 et N2 ont la même signification que ceux de la
  commande EXECUTER, mais la commande REPRENDRE ne change pas
  l'affectatioon des indentificateurs.  Toutes les valeurs antérieures
  à l'interruption sont conservées.

L'exécution du programme est toujours reprise au niveau principal
\(même si le programme avait été interrompu dans une procédure).

Voir les commandes PAS A PAS, NORMAL, EXECUTER A PARTIR DE, CONTINUER, POURSUIVRE JUSQU'EN."
  (io-new-line *task*)
  (let ((vm (task-vm *task*)))
    (unless (or (null to) (vm-line-exist-p vm to))
      (error-bad-line to))
    (when (vm-line-exist-p vm from)
      (vm-reset-stacks vm))
    (setf (vm-trap-line vm) to)
    (catch 'run-step-done (vm-goto vm from))
    (vm-run vm)
    (setf (task-pas-a-pas-first *task*) (task-pas-a-pas *task*))))


(defcommand "POURSUIVRE JUSQU'EN" awake   numero-de-ligne (linum)
  "Continue l'exécution du programme jusqu'à la ligne indiquée."
  "Relance l'exécution comme CONTINUER, mais avec un arrêt en ligne N.

Voir les commandes PAS A PAS, NORMAL, EXECUTER A PARTIR DE, CONTINUER, REPRENDRE A PARTIR DE."
  (io-new-line *task*)
  (let ((vm (task-vm *task*)))
    (unless (or (null linum) (vm-line-exist-p vm linum))
      (error-bad-line linum))
    (if (vm-pausedp vm)
        (progn
          (setf (vm-trap-line vm) linum)
          (vm-unpause vm)
          (vm-run vm)
          (setf (task-pas-a-pas-first *task*) (task-pas-a-pas *task*)))
        (error 'lse-error
               :format-control "ON NE PEUT PAS POURSUIVRE UN PROGRAMME QUI N'EST PAS EN PAUSE."))))


#-lse-unix
(defcommand "PRENDRE ETAT CONSOLE" awake  un-numero (consnum)
  "Copie le programme courant et les fichiers temporaires de la console indiquée."
  "Cette commande a pour effet de transférer à l'utilisateur le
programme d'un autre utilisateur travaillant sur la console numéro
N. Le programme ainsi transféré se trouve dans l'état où l'utilisateur
l'avait en mémoire.

Sur T1600 cette commande ne transfère que le programme, les variables étant dans l'état non défini.
Sur MITRA 15, l'état des variables est également transféré."
  (io-new-line *task*)
  ;;           verifier le numero correspond a une console existante.
  ;;           Si la zone temporaire locale < taille des fichiers dans la zone
  ;;           temporaire de la console a prendre :
  ;;           "TRANSFERT FICHIERS TEMPORAIRES IMPOSSIBLE"
  (lse-error "CETTE COMMANDE N'EST PAS IMPLEMENTEE DANS LA VERSION UNIX DU SYSTEME L.S.E."))


;; Gestion des fichiers programmes

(defcommand "APPELER" awake  un-programme (pgm)
  "Charge un fichier programme en mémoire."
  "Commande d'appel d'un programme déjà sur le disque.

Le fichier programme dont le nom NOMPG est indiqué dans le répertoire
courant est chargé en mémoire, remplaçant le programme courant.
L'utilisateur peut alors l'exécuter, le lister, le modifier, etc.

Voir les commandes AFFICHER REPERTOIRE, CHANGER REPERTOIRE, RANGER,
MODIFIER."
  (io-new-line *task*)
  (let* ((path      (catalog-pathname pgm :p))
         (vm        (task-vm *task*))
         (new-pgm   (compile-lse-file path pgm)))
    (vm-terminer vm)
    (replace-program vm new-pgm)
    (values)))

(defcommand "RANGER" awake un-programme (pgm)
  "Enregistre le programme courant dans un nouveau fichier programme."
  "Permet de ranger sur le disque un programme utilisateur.

Le programme courant est enregistré dans un fichier de nom NOMPG
indiqué, dans le répertoire courant.  Le fichier doit préalablement ne
pas exister, et sera créé.

Voir les commandes AFFICHER REPERTOIRE, CHANGER REPERTOIRE, TABLE DES
FICHIERS, APPELER, MODIFIER."
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


(defcommand "MODIFIER" awake  un-programme (pgm)
  "Enregistre le programme courant dans un fichier programme existant."
  "Permet de ranger sur le disque un programme utilisateur.

Le programme courant est enregistré dans un fichier de nom NOMPG
indiqué, dans le répertoire courant.  Le fichier doit exister
préalablement, et il est mis à jour par le nouveau programme.

Voir les commandes AFFICHER REPERTOIRE, CHANGER REPERTOIRE, TABLE DES
FICHIERS, APPELER, RANGER."
  
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


(defcommand "DECODER" awake un-fichier-et-deux-numeros (fichier &optional (from 1) (to nil))
  "Enregistre le programme dans un fichier donnée."

  "Le programme courant est garé en fichier temporaire sous le nom
NOMFI, depuis la ligne N1 jusqu'à la ligne N2.

Si N2 est omis, on rangera de la ligne N1 jusqu'à la dernière ligne du
programme.

Si N1 et N2 sont omis, on rangera tout le programme de l'utilisateur.

Le programme est rangé sous forme de chaîne de caractères, chaque
ligne étant terminé par un code X-OFF (19) et avec un code NUL (0) à
la fin, dans des enregistrements consécutifs partant du numéro 1.

Voir les commandes ENCODER, APPELER, RANGER, MODIFIER."
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


(defcommand "ENCODER" awake un-fichier-et-deux-numeros (fichier &optional (from 1) (to nil))
  "Charge un programme à partir d'un fichier donnée."

  "Cette commande est la commande symétrique de DECODER, les
parametres NOMFI, N1 et N2 on la même signfication.

Cette commande consiste à lire les instructions contenues dans le
fichier à décoder et à les compiler.

Voir les commandes DECODER, APPELER, RANGER, MODIFIER."
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


(defcommand "CATALOGUER" awake  deux-fichiers (temporaire permanent)
  "Copie un fichier temporaire dans un fichier permanent."

  "NOMF1 est un fichier temporaire existant.  NOMF2 est un fichier permanent.

Cette commande permet de mettre en bibliothèque sous le nom NOMF2 le
fichier données NOMF1 qui devient donc un fichier permanent.

Les fichiers permanents sont garés dans le répertoire courant.

Voir les commandes AFFICHER REPERTOIRE, CHANGER REPERTOIRE, TABLE DES
FICHIERS."
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



(defcommand "SUPPRIMER" awake arguments-supprimer (fichier &optional fictype)
  "Supprime un fichier de type indiqué, ou supprime tous les fichiers temporaires (*)."
  "
SUPPRIMER * supprime tous les fichiers temporaires.

SUPPRIMER NOMFI,P|D|T supprime le fichier Programme, Donnée permanent,
ou Temporaire indiqué.

Voir les commandes TABLE DES FICHIERS, UTILISATION DISQUE."
  (if (equal fichier '*)
      (supprimer-tous-les-fichiers-temporaires)
      (let ((path (catalog-pathname fichier fictype)))
        (io-new-line *task*)
        (delete-file path)
        (values))))


(defcommand "AFFICHER REPERTOIRE" awake nil ()
  "Affiche le répertoire courant.

Voir les commandes CHANGER REPERTOIRE, TABLE DES FICHIER, UTILISATION DISQUE."
  (io-new-line *task*)
  (io-format *task* "REPERTOIRE COURANT: ~A~%" *current-directory*))


(defcommand "CHANGER REPERTOIRE" awake une-ligne (nouveau-repertoire)
  "Change le répertoire courant."
  "Change le répertoire courant.  Le TEXTE donné désigne un chemin unix
absolu, ou relatif à l'ancien répertoire courant.

Voir les commandes AFFICHER REPERTOIRE, TABLE DES FICHIER, UTILISATION DISQUE."
  (io-new-line *task*)
  (when (and (stringp nouveau-repertoire)
             (< 1 (length nouveau-repertoire))
             (char/= #\/ (aref nouveau-repertoire (1- (length nouveau-repertoire)))))
    (setf nouveau-repertoire (concatenate 'string nouveau-repertoire "/")))
  (setf *current-directory*
        (truename (make-pathname
                   :name nil :type nil :version nil
                   :defaults (merge-pathnames nouveau-repertoire *current-directory*))))
  (task-close-all-files *task*))


(defcommand "TABLE DES FICHIERS" awake nil ()
  "Liste la table des fichiers (répertoire courant, et fichiers temporaires)."
  "Cette comamnde liste tous les fichiers programmes et donnée du
répertoire courant, et les fichiers temporaires.

Voir les commandes UTILISATION DISQUE, SUPPRIMER."
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


(defcommand "UTILISATION DISQUE" awake nil ()
  "Liste la table des fichiers (répertoire courant, et fichiers temporaires)."
  "Cette comamnde liste tous les fichiers programmes et donnée du
répertoire courant, et les fichiers temporaires.

Voir les commandes TABLE DES FICHIERS, SUPPRIMER."
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







;; Deboguage

;;  We provide a REPL for LE (repl) command for debugging.

#+developing
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

#+developing
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


#+developing
(defcommand "LE EVALUER UNE EXPRESSION LISP" awake une-ligne (ligne)
  "Commande de déboguage: évaluation d'une expression LISP."
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
(defcommand "LD DESASSEMBLER A PARTIR DE" awake     deux-numeros-optionels (from to)
  "Commande de deboguage: Désassemble les lignes de programme."
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

#+developing
(defcommand "LP DEASSEMBLER/PERFORER A PARTIR DE" awake  deux-numeros-optionels (from to)
  "Commande de deboguage: Désassemble les lignes de programme en perforant un ruban."
  (io-new-line *task*)
  (io-start-tape-puncher *task*)
  (unwind-protect (ld-desassembler-a-partir-de from to)
    (io-stop-tape-puncher *task*))
  (punch "PERFORATION EFFECTUEE")
  (io-format *task* "~%PERFORATION EFFECTUEE.~%"))


#+developing
(defcommand "SHOW BINDINGS" awake nil ()
  "Montre les touches."
  (let ((terminal (task-terminal *task*)))
   (io-format *task* "
~@[~16A pour entrer les données.~%~]~
~@[~16A pour effacer le caractère précédent.~%~]~
~@[~16A pour interrompre.~%~]~
~@[~16A pour envoyer le signal d'attention (fonction ATT()).~%~]~
~@[~16A pour entrer les données, mais ajoute le code RETOUR aux chaînes.~%~]~
"
              (terminal-key terminal :xoff)
              (terminal-key terminal :delete)
              (terminal-key terminal :escape)
              (terminal-key terminal :attention)
              (terminal-key terminal :return))))

#+developing
(defun initialize-debugging-task ()
  (setf *command-group* awake
        *task* (make-instance 'task
                   :state :active
                   :case-insensitive t
                   :upcase-output nil
                   :dectech nil
                   :unicode nil
                   :terminal (make-instance 'standard-terminal
                                 :input-stream  (stream-input-stream  *terminal-io*)
                                 :output-stream (stream-output-stream *terminal-io*)))))







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
          (vm-run vm))
        ;; program line
        (if (equalp #(!next-line) (code-vector code))
            (erase-line-number (task-vm task) (code-line code))
            (put-line (task-vm task) (code-line code) code)))
    (io-new-line *task*)))


(defun command-eval-line (task line)
  (let ((*task* task))
    (cond
      
      ((and (= 2 (length line))
            (alpha-char-p (aref line 0))
            (alpha-char-p (aref line 1)))
       ;; looks like a command
       (setf (task-pas-a-pas-first *task*) nil)
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
       (setf (task-pas-a-pas-first *task*) nil)
       (if (task-state-awake-p task)
           (lse-compile-and-execute task line)
           (error "COMMANDE INVALIDE EN L'ETAT ~A~%ESSAYER LA COMMANDE AI(DE).~%"
                  (task-state-label (task-state task)))))

      ((task-pas-a-pas-first *task*)       
       (continuer))
      
      #|else empty line, just ignore it.|#)))


(defvar *debug-repl* nil)
;; (setf *debug-repl* nil)
;; (setf *debug-repl* t)

(defun command-repl (task)
  (let ((*task* task)
        (*print-case* :upcase))
    (io-format task "~&PRET~%")
    (io-finish-output task)
    (handler-case ; catch au-revoir condition.
        (loop
          :while (task-state-awake-p task)
          :do (restart-case
                  (handler-case
                      (flet ((do-it ()
                               (setf (task-interruption task) nil)
                               ;; (unless (task-silence task)
                               ;;   (io-new-line task))
                               (io-finish-output task)
                               (handler-case
                                   (let ((line (io-read-line task :beep (not (task-silence task)))))
                                     (if (task-interruption task)
                                         (progn
                                           (io-standard-redirection task)
                                           (echo)
                                           (io-format task "~%PRET~%"))
                                         (command-eval-line task line)))
                                 (end-of-file (err)
                                   (if (and (io-tape-input-p task)
                                            (eql (stream-error-stream err) (task-input task)))
                                       (io-stop-tape-reader task)
                                       (progn
                                         ;; end-of-file on input, let's exit.
                                         ;; (error err)
                                         (signal 'au-revoir)))))))
                        (if *debug-repl*
                            (handler-bind ((lse-error     (function signal))
                                           (scanner-error (function signal))
                                           (parser-error  (function signal))
                                           (file-error    (function signal))
                                           (error         (function invoke-debugger)))
                              (do-it))
                            (do-it)))
                    (scanner-error-invalid-character (err)
                      (io-format task "~%ERREUR: ~?~%"
                                 "CARACTÈRE INVALIDE ~A~:[~*~; (~D~)~] EN POSITION ~D"
                                 (destructuring-bind (ch pos) (scanner-error-format-arguments err)
                                   (list
                                    (if (<= 32 (char-code ch))
                                        (format nil "'~A'" ch)
                                        (format nil ".~A." (char-code ch)))
                                    (<= 32 (char-code ch))
                                    (char-code ch)
                                    pos)))
                      (io-format task "~%PRET~%")
                      (io-finish-output task))
                    (error (err)
                      (error-format task err)
                      (io-format task "~%PRET~%")
                      (io-finish-output task))
                    (user-interrupt (condition)
                      #+developing
                      (io-format task "~%Condition: ~A~%" condition)
                      (io-standard-redirection task)
                      (echo)
                      (io-format task "~%PRET~%")
                      (io-finish-output task)))
                (continue ()
                  :report "CONTINUER")))
      (au-revoir () (values)))))



;; (test/command-grammars)

;; Local Variables:
;; eval: (cl-indent 'defcommand 3)
;; End:
;;;; THE END ;;;;
