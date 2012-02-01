;;****************************************************************************
;;FILE:               commands.lisp
;;LANGUAGE:           Common-Lisp
;;SYSTEM:             Common-Lisp
;;USER-INTERFACE:     NONE
;;DESCRIPTION
;;    
;;    The commands of the LSE system.
;;    
;;AUTHORS
;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;MODIFICATIONS
;;    2005-08-26 <PJB> Distiled form C 'lse_cmd.c'.
;;    2002-02-12 <PJB> Extracted from lse_main.c
;;BUGS
;;LEGAL
;;    GPL
;;    
;;     Copyright Pascal J. Bourguignon 2000 - 2005
;; 
;;     This file is part of EMULSE :  L.S.E.  [ EMULATION MITRA-15 ]
;;    
;;    This program is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU General Public License
;;    as published by the Free Software Foundation; either version
;;    2 of the License, or (at your option) any later version.
;;    
;;    This program is distributed in the hope that it will be
;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;    PURPOSE.  See the GNU General Public License for more details.
;;    
;;    You should have received a copy of the GNU General Public
;;    License along with this program; if not, write to the Free
;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;    Boston, MA 02111-1307 USA
;;****************************************************************************


#||
(delete-duplicates
(delete-if-not
(lambda (x) (and (symbolp x)
(<= 4 (length (string x)))
(string= "TOK-" x :end2 4)))
(flatten (with-open-file (in "commands.lisp")
(loop :for s = (read in nil in)
:until (eq s in)
:collect s)))))
||#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEFINE-GRAMMAR & DEFRULE
;;;
;;;  Allows the definition of gramamr in lisp sources
;;;  instead of .zb files.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *rules* (make-hash-table)))


(defmacro defrule (&whole rule   name &rest args )
  (declare (ignore args))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *rules*) ',rule)
     ',name))

(defmacro define-grammar (name
                          (&key (grammar-file nil grammar-file-p)
                                (output-file  nil output-file-p)
                                (package nil packagep)
                                (identifier-start-chars nil)
                                (identifier-continue-chars nil)
                                (intern-identifier nil)
                                (string-delimiter nil)
                                (symbol-delimiter nil)
                                (domain ())
                                (domain-file nil domain-file-p)
                                (grammar "null-grammar")
                                (white-space nil white-space-p)
                                (case-sensitive t)
                                (lex-cats nil lex-cats-p))
                          &body rules)
  (unless lex-cats-p
    (error ":LEX-CATS keyword is mandatory in the options of DEFINE-GRAMMAR."))
  (let ((grammar-file (if grammar-file-p
                          grammar-file
                          (make-pathname :name (string name)
                                         :type "ZB"
                                         :defaults *grammar-directory*
                                         :case :common)))
        (output-file (if output-file-p
                         output-file
                         (make-pathname :name (string name)
                                        :type "TAB"
                                        :defaults *grammar-directory*
                                        :case :common))))
    (with-open-file (*standard-output*
                     grammar-file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (print
       `(
         :name     ,(string name)
                   :package  ,(string (if packagep package (package-name *package*)))
                   :identifier-start-chars     ,identifier-start-chars    
                   :identifier-continue-chars  ,identifier-continue-chars 
                   :intern-identifier          ,intern-identifier         
                   :string-delimiter           ,string-delimiter          
                   :symbol-delimiter           ,symbol-delimiter          
                   :case-sensitive             ,case-sensitive            
                   :grammar                    ,grammar                   
                   :domain                     ,domain                    
                   :domain-file ,(if domain-file-p
                                     domain-file
                                     (format nil "~(~A~)-domain" name))
                   ,@(when white-space-p `(:white-space ,white-space))
                   :lex-cats ,lex-cats))
      (dolist (rule rules)
        (if (and (consp rule) (eq (first rule) 'insert-rule))
            (if (gethash (second rule) *rules*)
                (print (gethash (second rule) *rules*))
                (error "Unknown rule name ~S" (second rule)))
            (print rule))))
    (LET ((COM.HP.ZEBU::*WARN-CONFLICTS*  T)
          (COM.HP.ZEBU::*ALLOW-CONFLICTS* T))
      (zebu-compile-file grammar-file :output-file output-file))
    `(eval-when (:compile-topleve :load-toplevel :execute)
       (zebu:delete-grammar ,(string name))
       (zebu-load-file ',output-file)
       (defparameter ,name (find-grammar ,(string name))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Command line grammars
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; :white-space = single space.
;; we don't keep newline because it's the eoln/end of statement
;; and we don't keep tabulation because it's not valid.


(define-grammar numero-de-ligne
    (
     :white-space (#.(character " "))
                  :lex-cats    ((tok-numero  "{NUMERO}")))

  (defrule numero-de-ligne
      := (tok-numero)
      :build (progn (unless (valid-line-number (numero-valeur tok-numero))
                      (error "NUMERO DE LIGNE INVALIDE: ~A"
                             (token-text tok-numero)))
                    tok-numero)))


(define-grammar un-numero
    (
     :white-space (#.(character " "))
                  :lex-cats    ((tok-numero  "{NUMERO}")))

  (defrule un-numero
      := (tok-numero)))


(defrule deux-numeros
    := (tok-numero.1 tok-virgule tok-numero.2)
    :build (progn (list tok-numero.1 tok-numero.2)))


(define-grammar deux-numeros
    (
     :white-space (#.(character " "))
                  :lex-cats    ((tok-virgule ",")(tok-numero  "{NUMERO}")))

  (insert-rule deux-numeros))


(define-grammar deux-numeros-optionels
    (
     :white-space (#.(character " "))
                  :lex-cats    ((tok-virgule ",")(tok-numero  "{NUMERO}")))

  (defrule deux-numeros-optionels
      := (tok-numero.1 tok-virgule tok-numero.2)
      :build (progn (list tok-numero.1 tok-numero.2))
      := ()
      :build (progn (list 1 nil))))


(define-grammar liste-de-numeros
    (
     :white-space (#.(character " "))
                  :lex-cats    ((tok-virgule ",")
                                (tok-fois    "\\*")
                                (tok-a       "A")
                                (tok-numero  "{NUMERO}")))

  (defrule liste-de-numeros
      := (tok-fois) :build (progn :all)
      := () :build (progn nil)
      := (liste-d-intervales))

  (defrule liste-d-intervales
      := (intervale-de-numeros tok-virgule liste-d-intervales)
      :build (cons intervale-de-numeros liste-d-intervales)
      := (intervale-de-numeros)
      :build (list intervale-de-numeros))
  
  (defrule intervale-de-numeros
      := (numero-de-ligne.1 tok-a numero-de-ligne.2)
      :build (progn  `(interval ,numero-de-ligne.1 ,numero-de-ligne.2))
      := (numero-de-ligne)
      :build (progn  numero-de-ligne)))


(defrule ident
    := (tok-identificateur)
    :build (progn
             (unless (and (char= (character "&")
                                 (aref (token-text tok-identificateur) 0))
                          (<= (length  (token-text tok-identificateur) 5)))
               (error "IDENTIFICATEUR INVALIDE: ~A"
                      (token-text tok-identificateur)))
             tok-identificateur))


(define-grammar un-programme
    (
     :white-space (#.(character " "))
                  :lex-cats    ((tok-identificateur "{IDENTIFICATEUR}")))

  (defrule un-programme
      := (ident))

  (insert-rule ident))


(define-grammar deux-fichiers
    (
     :white-space (#.(character " "))
                  :lex-cats    ((tok-virgule ",")(tok-identificateur "{IDENTIFICATEUR}")))

  (defrule deux-fichiers
      := (ident.1 tok-virgule ident.2)
      :build (list ident.1 ident.2))
  
  (insert-rule ident))


(define-grammar un-fichier-et-deux-numeros
    (
     :white-space (#.(character " "))
                  :lex-cats    ((tok-virgule ",")
                                (tok-numero  "{NUMERO}")
                                (tok-identificateur "{IDENTIFICATEUR}")))

  (defrule un-fichier-et-deux-numeros
      := (ident tok-virgule deux-numeros)
      :build (cons ident deux-numeros))

  (insert-rule ident)
  (insert-rule deux-numeros))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; LSE Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *command-scanner*
  (let ((scanner (make-instance 'scanner :source (MAKE-STRING-INPUT-STREAM ""))))
    (advance-token scanner)
    scanner))


(defstruct command
  initials
  name
  grammar 
  arguments
  documentation
  function)


(defmacro defcommand (name grammar arguments &body body)
  `(make-command
    :initials       (subseq ,name 0 2)
    :name          ,name
    :grammar       ,(when grammar (find-grammar (string grammar)))
    :arguments    ',arguments
    :documentation ,(if (stringp (first body)) (first body) "")
    :function       (lambda ,(flatten arguments) ,@body)))


(defstruct command-group
  name
  supergroups
  commands)


(defmacro define-command-group (name supergroups &body commands)
  `(defparameter ,name
     (make-command-group
      :name ',name
      :supergroups ',supergroups
      :commands (list ,@commands))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



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
              (io-format *task* "~&LES COMMANDES DISPONIBLES SONT:~2%")
              (dolist (command *command-group*)
                (io-format *task* "~A)~20A ~A~%"
                           (command-initials command)
                           (subseq (command-name command) 2)
                           (command-documentation command)))
              (io-format *task*    "~&POUR ENTRER UNE COMMANDE, TAPEZ SES DEUX ~
                            PREMIERES LETTRES, SUIVIES DE CTRL-S.~
                          ~&POUR ANNULER UN CARACTERE, TAPEZ \\.~%"))
  

  (defcommand "IDENTIFICATION" un-numero (identification)
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


(define-command-group sleeping (common)

  
  (defcommand "DROITS"  nil ()
              "Allow the administrator to set the access rights of the accounts."
              (io-format *task* " ")
              (unless (string= (io-read-line *task* :echo nil :xoff t) +PASSWORD+)
                (error-report *task* +error-bad-identifier+)
                (return-from cmd-droits))
              (io-newline *task*) (io-format *task* "            ANCIEN  NOUVEAU  ") ;
              (io-newline *task*) (io-format *task* "NO.COMPTE    PDAF     PDAF   ") ;
              (loop with account = -1
                 for line = (progn (io-newline *task*)
                                   (io-format *task* "COMPTE NO. : ") ;
                                   (io-read-line *task* :xoff t))
                 until (string= line "FIN")
                 do
                   (setf account (if (zerop (length line))
                                     (mod (1+ account) 100)
                                     (parse-integer line :junk-allowed nil)))
                   (when (<= 0 account 99)
                     (let ((op (account-check-right account :programme))
                           (od (account-check-right account :permanent))
                           (oa (account-check-right account :tempoauto))
                           (of (account-check-right account :tempofixe))
                           np nd na nf)
                       (io-beginning-of-line *task*)
                       (io-format *task* 
                                  "   ~2,'0D       ~{~[0~;1~]~}     "
                                  account (list op od oa of))
                       (setf line (io-read-line *task* :xoff t))
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
                       (io-beginning-of-line *task*)
                       (io-format *task*
                                  "   ~2,'0D       ~{~[0~;1~]~}      ~{~[0~;1~]~}    "
                                  account (list op od oa of) (list np nd na nf)) )))
              (io-new-line *task*))



  (defcommand "BONJOUR" nil ()
              (io-new-line *task* 2)
              (io-format *task* "LSE-M15  CONSOLE NO.~2D  ~A" 
                         (*task*-console *task*) (lse-dat))
              (*task*-state-change *task* :active))


  (defcommand "ADIEUX" nil ()
              (io-new-line *task*)
              (*task*-disconnect *task*))


  ) ;; sleeping



(define-command-group awake (common)

  (defcommand "AU REVOIR" nil ()
              (io-format *task* "  ~8A" (subseq (lse-dat) 9))
              (*task*-state-change *task* :sleeping)
              (catalog-delete-temporaries (*task*-console *task*)))


  (defcommand "ABREGER"     nil ()  (setf (abreger   *task*) t))
  (defcommand "IN EXTENSO"  nil ()  (setf (abreger   *task*) nil))

  (defcommand "PAS A PAS"   nil ()  (setf (pas-a-pas *task*) t))
  (defcommand "NORMAL"      nil ()  (setf (pas-a-pas *task*) nil))


  (defcommand "LISTER A PARTIR DE"   deux-numeros-optionels ((from to))
              
              )


  (defcommand "PERFORER A PARTIR DE" deux-numeros-optionels ((from to))
              (lse_es_activer_perforateur_de_ruban travail)
              (lse_langage_perforer travail (first from-to) (second from-to))
              (lse_es_arreter_perforateur_de_ruban travail)
              (lse_ecrire_format travail "\r\nPERFORATION EFFECTUEE."))


  (defcommand "LP LISTER   LISP"     deux-numeros-optionels ((from to))
              
              )


  (defcommand "PP PERFORER LISP"     deux-numeros-optionels ((from to))
              (lse_es_activer_perforateur_de_ruban travail)
              (lse_langage_lispeur travail (first from-to) (second from-to))
              (lse_es_arreter_perforateur_de_ruban travail)
              (lse_ecrire_format travail "\r\nPERFORATION EFFECTUEE."))


  (defcommand "NUMERO A PARTIR DE"   deux-numeros-optionels ((from to))
              
              )


  (defcommand "EXECUTER A PARTIR DE" deux-numeros-optionels ((from to))
              
              )


  (defcommand "CONTINUER"             nil ()
              )


  (defcommand "REPRENDRE A PARTIR DE" deux-numeros-optionels ((from to))
              
              )


  (defcommand "POURSUIVRE JUSQU'EN"   numero-de-ligne (linum)
              
              )


  (defcommand "PRENDRE ETAT CONSOLE"  un-numero (consnum)
              ;;           verifier le numero correspond a une console existante.
              ;;           Si la zone temporaire locale < taille des fichiers dans la zone
              ;;           temporaire de la console a prendre :
              ;;           "TRANSFERT FICHIERS TEMPORAIRES IMPOSSIBLE"       
              )

  (defcommand "APPELER"  un-programme (pgm)
              ;;         lse_cmd_parser_t parser;
              ;;         lse_erreur_t erreur;
              ;;         lse_chaine_t ligne;
              ;;         lse_ident_t ident;
              ;;         lse_cmd_lire_ligne(travail,&ligne);
              ;;         lse_cmd_arg_initialiser(&parser,&ligne); 
              ;;         erreur=lse_cmd_analyser_un_programme(&parser,&ident);
              ;;         if(erreur==lse_ok){
              ;;             lse_langage_appeler(travail,&ident);
              ;;         }else{
              ;;             lse_erreur_rapporter(travail,erreur);
              ;;         }
              )
  
  (defcommand "CATALOGUER"  deux-fichiers ((temporaire permanent))
              ;;         lse_cmd_parser_t parser;
              ;;         lse_erreur_t erreur;
              ;;         lse_chaine_t ligne;
              ;;         lse_ident_t temporaire;
              ;;         lse_ident_t permanent;
              ;; 
              ;;         if(!lse_compte_verifier_droit(travail->compte,
              ;;                                       lse_compte_droit_permanent)){
              ;;             lse_erreur_rapporter(travail,lse_erreur_non_autorise);
              ;;             return;
              ;;         }
              ;; 
              ;;         lse_cmd_lire_ligne(travail,&ligne);
              ;;         lse_cmd_arg_initialiser(&parser,&ligne); 
              ;;         erreur=lse_cmd_analyser_deux_fichiers(&parser,&temporaire,&permanent);
              ;;         if(erreur==lse_ok){
              ;;             erreur=lse_catalogue_cataloguer(travail->console,temporaire.nom,
              ;;                                             travail->compte,permanent.nom);
              ;;         }
              ;;         if(erreur!=lse_ok){
              ;;             lse_erreur_rapporter(travail,erreur);
              ;;         }
              )
  

  (defcommand "ENCODER" un-ficher-et-deux-numeros ((fichier de a))
              ;;         lse_cmd_parser_t parser;
              ;;         lse_erreur_t erreur;
              ;;         lse_chaine_t ligne;
              ;;         lse_ident_t fichier;
              ;;         int de;
              ;;         int a;
              ;;         lse_cmd_lire_ligne(travail,&ligne);
              ;;         lse_cmd_arg_initialiser(&parser,&ligne); 
              ;;         erreur=lse_cmd_analyser_un_fichier_et_deux_numeros(&parser,
              ;;                                                            &fichier,&de,&a);
              ;;         if(erreur==lse_ok){
              ;;             lse_langage_encoder(travail,&fichier,de,a);
              ;;         }else{
              ;;             lse_erreur_rapporter(travail,lse_erreur_nom_de_fichier_attendu);
              ;;         }
              )


  (defcommand "DECODER" un-ficher-et-deux-numeros ((fichier de a))
              ;;         lse_cmd_parser_t parser;
              ;;         lse_erreur_t erreur;
              ;;         lse_chaine_t ligne;
              ;;         lse_ident_t fichier;
              ;;         int de;
              ;;         int a;
              ;;         lse_cmd_lire_ligne(travail,&ligne);
              ;;         lse_cmd_arg_initialiser(&parser,&ligne); 
              ;;         erreur=lse_cmd_analyser_un_fichier_et_deux_numeros(&parser,
              ;;                                                            &fichier,&de,&a);
              ;;         if(erreur==lse_ok){
              ;;             lse_langage_decoder(travail,&fichier,de,a);
              ;;         }else{
              ;;             lse_erreur_rapporter(travail,lse_erreur_nom_de_fichier_attendu);
              ;;         }
              )
  

  (defcommand "EFFACER LIGNES" deux-numeros-optionels ((from to))
              ;;     void lse_cmd_effacer_lignes(lse_travail_t* travail)
              ;;     {
              ;;         lse_cmd_parser_t parser;
              ;;         lse_erreur_t erreur;
              ;;         lse_chaine_t ligne;
              ;;         int max=lse_programme_ligne_max();
              ;;         char* indicateurs=(char*)malloc((unsigned)(max+1));
              ;;         lse_cmd_lire_ligne(travail,&ligne);
              ;;         lse_cmd_arg_initialiser(&parser,&ligne); 
              ;;         erreur=lse_cmd_analyser_liste_de_numeros(&parser,indicateurs);
              ;;         if(erreur==lse_ok){
              ;;             int compte=0;
              ;;             int i;
              ;;             for(i=1;i<=max;i++){
              ;;                 if(indicateurs[i]){
              ;;                     compte++;
              ;;                     lse_langage_effacer(travail,i);
              ;;                 }
              ;;             }
              ;;             if(compte==0){
              ;;                 lse_ecrire_format(travail,"\r\nANNULE");
              ;;             }
              ;;         }else{
              ;;             lse_erreur_rapporter(travail,erreur);
              ;;         }
              ;;         free(indicateurs);
              ;;     }/*lse_cmd_effacer_lignes*/
              )
  

  (defcommand "ELIMINER COMMENTAIRES"  nil ()
              ;;         lse_language_eliminer_commentaires(travail);
              )



  (defcommand "MODIFIER"  un-programme (pgm)
              ;;         lse_cmd_parser_t parser;
              ;;         lse_erreur_t erreur;
              ;;         lse_chaine_t ligne;
              ;;         lse_ident_t ident;
              ;;         lse_cmd_lire_ligne(travail,&ligne);
              ;;         lse_cmd_arg_initialiser(&parser,&ligne); 
              ;;         erreur=lse_cmd_analyser_un_programme(&parser,&ident);
              ;;         if(erreur==lse_ok){
              ;;             lse_langage_modifier(travail,&ident);
              ;;         }else{
              ;;             lse_erreur_rapporter(travail,erreur);
              ;;         }
              )


  (defcommand "RANGER" un-programme (pgm)
              ;;         lse_cmd_parser_t parser;
              ;;         lse_erreur_t erreur;
              ;;         lse_chaine_t ligne;
              ;;         lse_ident_t ident;
              ;;         lse_cmd_lire_ligne(travail,&ligne);
              ;;         lse_cmd_arg_initialiser(&parser,&ligne); 
              ;;         erreur=lse_cmd_analyser_un_programme(&parser,&ident);
              ;;         if(erreur==lse_ok){
              ;;             lse_langage_ranger(travail,&ident);
              ;;         }else{
              ;;             lse_erreur_rapporter(travail,erreur);
              ;;         }
              )

  
  (defcommand "RUBAN" nil ()
              ;;         if(lse_es_activer_lecteur_de_ruban(travail)){
              ;;             travail->silence=1;
              ;;         }
              )

  
  (defcommand "SILENCE"  nil ()
              ;;         travail->silence=1;
              )

  
  (defcommand "SUPPRIMER" deux-fichiers ((fichier stype))
              ;;         lse_cmd_parser_t parser;
              ;;         lse_erreur_t erreur;
              ;;         lse_chaine_t ligne;
              ;;         lse_ident_t fichier;
              ;;         lse_ident_t stype;
              ;;         lse_catalogue_type_t type=lse_catalogue_type_fin;
              ;; 
              ;;         lse_cmd_lire_ligne(travail,&ligne);
              ;;         lse_cmd_arg_initialiser(&parser,&ligne); 
              ;;         erreur=lse_cmd_analyser_deux_fichiers(&parser,&fichier,&stype);
              ;;         if(erreur==lse_ok){
              ;;             if(strcmp(stype.nom,"P")==0){
              ;;                 type=lse_catalogue_type_programme;
              ;;             }else if(strcmp(stype.nom,"D")==0){
              ;;                 type=lse_catalogue_type_permanent;
              ;;             }else if(strcmp(stype.nom,"T")==0){
              ;;                 type=lse_catalogue_type_temporaire;
              ;;             }else{
              ;;                 lse_erreur_rapporter(travail,
              ;;                                      lse_erreur_type_de_fichier_invalide);
              ;;                 return;
              ;;             }
              ;;             erreur=lse_catalogue_supprimer_fichier(
              ;;                 type,
              ;;                 type==lse_catalogue_type_temporaire?
              ;;                 travail->console:travail->compte,
              ;;                 fichier.nom);
              ;;             lse_erreur_rapporter(travail,erreur);
              ;;         }else{
              ;;             lse_cmd_arg_initialiser(&parser,&ligne);
              ;;             if(!lse_cmd_etoile(&parser)){
              ;;                 lse_erreur_rapporter(travail,erreur);
              ;;                 return;
              ;;             }
              ;;             if(!lse_cmd_rien_de_plus(&parser)){
              ;;                 lse_erreur_rapporter(travail,lse_erreur_rien_de_plus_attendu);
              ;;             }
              ;;             erreur=lse_catalogue_supprimer_temporaires(travail->console);
              ;;             if(erreur!=lse_ok){
              ;;                 lse_erreur_rapporter(travail,erreur);
              ;;             }
              ;;         }
              )


  (defcommand "TABLE DES FICHIERS" nil ()
              ;;         lse_catalogue_t* programmes;
              ;;         lse_catalogue_t* permanents;
              ;;         lse_catalogue_t* temporaires;
              ;; 
              ;;         if(travail->compte==lse_compte_anonyme){
              ;;             programmes=0;
              ;;             permanents=0;
              ;;         }else{
              ;;             programmes=lse_catalogue_programmes(travail->compte);
              ;;             permanents=lse_catalogue_permanents(travail->compte);
              ;;         }
              ;; 
              ;;         temporaires=lse_catalogue_temporaires(travail->console);
              ;; 
              ;;         lse_lister_fichiers(travail,programmes,permanents,temporaires,0,0);
              )


  (defcommand "UTILISATION DISQUE" nil ()
              ;;         lse_catalogue_t* programmes;
              ;;         lse_catalogue_t* permanents;
              ;;         lse_catalogue_t* temporaires;
              ;;         int secteurs_libres;
              ;;         lse_temporaire_alloue_t* alloues;
              ;;         lse_travail_t* console;
              ;;         int i;
              ;; 
              ;;         if(travail->compte==lse_compte_anonyme){
              ;;             return;
              ;;         }
              ;; 
              ;;         programmes=lse_catalogue_programmes(lse_compte_tous);
              ;;         permanents=lse_catalogue_permanents(lse_compte_tous);
              ;;         temporaires=lse_catalogue_temporaires(lse_compte_tous);
              ;; 
              ;;         secteurs_libres=lse_catalogue_secteurs_libres();
              ;;         alloues=(lse_temporaire_alloue_t*)malloc(sizeof(lse_temporaire_alloue_t)
              ;;                                                  *(lse_travail_nombre()+1));
              ;;         i=0;
              ;;         while(0!=(console=lse_travail_console(i))){
              ;;             alloues[i].console=console->console;
              ;;             alloues[i].secteurs=lse_fichier_temporaire_alloue(console->console);
              ;;             i++;
              ;;         }
              ;;         alloues[i].console=-1;
              ;; 
              ;; 
              ;;         lse_lister_fichiers(travail,programmes,permanents,temporaires,
              ;;                             secteurs_libres,alloues);
              ;; 
              ;;         free(alloues);
              )

  
  ;;     void lse_catalogue_lister(lse_travail_t* travail,
  ;;                               lse_catalogue_t* catalogue,
  ;;                               lse_catalogue_champ_t champs)
  ;;     {
  ;;         int i;
  ;;         const char* format_numero;
  ;;         if(champs==(lse_catalogue_champ_nom
  ;;                     |lse_catalogue_champ_numero
  ;;                     |lse_catalogue_champ_secteurs)){
  ;;             format_numero="  %02d   ";
  ;;         }else{
  ;;             format_numero="   %02d    ";
  ;;         }
  ;; 
  ;;         for(i=0;i<catalogue->nombre;i++){
  ;;             lse_catalogue_entree_t* entree;
  ;;             entree=pjb_tableau_element_a(catalogue->entrees,i);
  ;;             
  ;;             lse_ecrire_format(travail,"\r\n");
  ;; 
  ;;             if(champs&lse_catalogue_champ_nom){
  ;;                 lse_ecrire_format(travail,"%-5s",entree->nom);
  ;;             }
  ;;             if(champs&lse_catalogue_champ_numero){
  ;;                 lse_ecrire_format(travail,format_numero,entree->numero);
  ;;             }
  ;;             if(champs&lse_catalogue_champ_date){
  ;;                 lse_ecrire_format(travail,"%02d/%02d/%02d",
  ;;                                   entree->jour,entree->mois,entree->annee);
  ;;             }
  ;;             if(champs&lse_catalogue_champ_mots){
  ;;                 lse_ecrire_format(travail,"  %-5d",entree->mots);
  ;;             }
  ;;             if(champs&lse_catalogue_champ_secteurs){
  ;;                 lse_ecrire_format(travail,"     %d",entree->secteurs);
  ;;             }
  ;;         }
  ;;     }/*lse_catalogue_lister*/
  ;; 
  ;; 
  ;;  
  ;; 
  ;;     typedef struct {
  ;;         int console;
  ;;         int secteurs;
  ;;     }       lse_temporaire_alloue_t;
  ;; 
  ;;     static void lse_lister_fichiers(lse_travail_t* travail,
  ;;                                     lse_catalogue_t* programmes,
  ;;                                     lse_catalogue_t* permanents,
  ;;                                     lse_catalogue_t* temporaires,
  ;;                                     int secteurs_libres,
  ;;                                     lse_temporaire_alloue_t* alloues)
  ;;     {
  ;;         int i;
  ;;         lse_valeur_t* arguments[16];
  ;;         lse_chaine_t* date;
  ;; 
  ;;         lse_dat(arguments);
  ;;         date=(lse_chaine_t*)(arguments[0]);
  ;;         lse_ecrire_format(travail,"  %s\r\n",date->caracteres);
  ;; 
  ;;         if(programmes!=0){
  ;;             lse_ecrire_format(travail,
  ;;                               "\r\nFICHIERS-PROGRAMME"
  ;;                               "\r\n******************"
  ;;                               "\r\n"
  ;;                               "\r\n NOM NO.COMPTE  DATE  NB.MOTS"
  ;;                               "\r\n");
  ;;             lse_catalogue_lister(travail,programmes,
  ;;                                  lse_catalogue_champ_nom
  ;;                                  |lse_catalogue_champ_numero
  ;;                                  |lse_catalogue_champ_date
  ;;                                  |lse_catalogue_champ_mots);
  ;;             lse_ecrire_format(travail,"\r\n");
  ;;         }
  ;; 
  ;;         if(permanents!=0){
  ;;             lse_ecrire_format(travail,
  ;;                               "\r\nFICHIERS-DONNEE PERMANENTS"
  ;;                               "\r\n**************************"
  ;;                               "\r\n"
  ;;                               "\r\n NOM NO.COMPTE  DATE  NB.SECTEURS"
  ;;                               "\r\n");
  ;; 
  ;;             lse_catalogue_lister(travail,permanents,
  ;;                                  lse_catalogue_champ_nom
  ;;                                  |lse_catalogue_champ_numero
  ;;                                  |lse_catalogue_champ_date
  ;;                                  |lse_catalogue_champ_secteurs);
  ;;             lse_ecrire_format(travail,"\r\n");
  ;;         }
  ;; 
  ;;         if(temporaires!=0){
  ;;             lse_ecrire_format(travail,
  ;;                               "\r\nFICHIERS-DONNEE TEMPORAIRES"
  ;;                               "\r\n***************************"
  ;;                               "\r\n"
  ;;                               "\r\n NOM CONSOLE NB.SECTEURS"
  ;;                               "\r\n");
  ;; 
  ;;             lse_catalogue_lister(travail,temporaires,
  ;;                                  lse_catalogue_champ_nom
  ;;                                  |lse_catalogue_champ_numero
  ;;                                  |lse_catalogue_champ_secteurs);
  ;;             lse_ecrire_format(travail,"\r\n");
  ;;         }
  ;; 
  ;;         if(alloues!=0){
  ;;             lse_ecrire_format(travail,
  ;;                               "\r\nNOMBRE DE SECTEURS LIBRES: %d"
  ;;                               "\r\n**************************"
  ;;                               "\r\n"
  ;;                               "\r\nESPACE TEMPORAIRE ALLOUE"
  ;;                               "\r\n************************"
  ;;                               "\r\n"
  ;;                               "\r\nCONSOLE NB.SECTEURS"
  ;;                               "\r\n",secteurs_libres);
  ;;             i=0;
  ;;             while(alloues[i].console>=0){
  ;;                 lse_ecrire_format(travail,"\r\n  %02d         %d",
  ;;                                   alloues[i].console,alloues[i].secteurs);
  ;;                 i++;
  ;;             }
  ;;         }
  ;; 
  ;;         lse_ecrire_format(travail,"\r\n");
  ;; 
  ;;     }/*lse_lister_fichiers*/

  ) ;; awake



(defun cmd-interprete (task)
  (loop while (task-state-awake-p (task-state task))
     do
       (setf (task-interruption task) nil)
       (unless (task-silence task) (io-new-line task))
       (let ((line (io-read-line task
                                 :beep (not (task-silence task))
                                 :xoff t)))
         (unless (task-interruption task)
           (cond
             ((and (= 2 (length line)) (alpha-char-p (aref line 0)))
              ;; looks like a command
              (let ((entry (assoc line 
                                  (if (task-state-sleeping-p task)
                                      +sleeping-commands+ 
                                      +awake-commands+)
                                  :test (lambda (x y) (string= x y :end2 2)))))
                (if (null entry)
                    (error-format task "COMMAND INCONNUE")
                    (progn
                      (unless (task-abreger task)
                        (io-beginning-of-line task)
                        (io-format task "~A" (first entry)))
                      (funcall (second entry) task)))))
             ((< 0 (length line))
              ;; soit une instruction, soit une ligne de programme... 
              (if (task-state-awake-p task)
                  (lse-compile-and-execute task line)
                  (io-format task "ETAT DORMANT"))))))))


;;----------------------------------------------------------------------

;; Local Variables:
;; eval: (progn (cl-indent 'defcommand 3) (cl-indent 'define-command-group 2))
