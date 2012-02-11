;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               catalog.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    EMULSE : L.S.E. [ EMULATION MITRA-15 ]
;;;;    
;;;;    An emultator of the CII MITRA-15 L.S.E. System 
;;;;    and programming language interpreter.
;;;;    
;;;;    This is the L.S.E. Librarian (Cataloger).
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-08-02 <PJB> Begun convertion to Common-Lisp.
;;;;    2000-12-09 <PJB> Added this header comment.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    This file is part of EMULSE :  L.S.E.  [ EMULATION MITRA-15 ]
;;;;
;;;;    Copyright Pascal J. Bourguignon 2000 - 2004
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

;; A catalog is a list of files in a given category.
;; There are the following catalog categories:
;; :program :permanent :temporary :tape :shelf

(defvar *current-console-number* 0)
(defvar *current-user-number*    0)



(defclass program-file (file)
  ())

(defclass data-file (file)
  ())

(defclass permanent-data-file (data-file)
  ())

(defclass temporary-data-file (data-file)
  ())


(deftype catalog-type () 
  '(member :program :permanent :temporary :tape :shelf))


(defclass catalog-entry ()
  ((name    :initarg :name
            :accessor catalog-entry-name
            :type string         
            :documentation "Catalog name, 1-5 alphanum.")
   (day     :initarg :day
            :accessor catalog-entry-day
            :initform 1
            :type (integer 1 31))
   (month   :initarg :month
            :accessor catalog-entry-month
            :initform 1
            :type (integer 1 12))
   (year    :initarg :year
            :accessor catalog-entry-year
            :initform 0
            :type (integer 0 99))
   (words   :reader catalog-entry-words
            :type (integer 0)
            :documentation "File size in words.")
   (sectors :reader catalog-entry-sectors
            :type (integer 0)
            :documentation "File size in sectors.")
   (path    :initarg :path
            :reader catalog-entry-path
            :documentation "The pathname to the actual file.")))


(defclass temporary-entry (catalog-entry)
  ((console :initarg :console
            :accessor catalog-entry-console
            :initform 0
            :type (integer 0 99)
            :documentation "Number of console of the temporary file.")))


(defclass permanent-entry (catalog-entry)
  ((owner   :initarg :owner
            :accessor catalog-entry-owner
            :initform 0
            :type (integer 0 99) 
            :documentation "Owner of the permanent file.")))

(defclass tape-entry (catalog-entry)
  ((shelf   :initform nil
            :documentation "for tapes, a relative logical path (\"x\" \"y\" \"z\")")))



;; (io-valid-tape-name-p name)
;;         name ::= part | part '/' name .
;;         part ::= [A-Za-z][A-Za-z0-9]{0,4} /

(defclass catalog ()
  ((type    :initform :program :accessor catalog-type)
   (count   :initform 0 :accessor catalog-count)
   (entries :initarg :entries :initform '()  :accessor catalog-entries
            :documentation "list of catalog-entry"))) 



(defstruct (catalog-data (:type list))
  "Structure of the following *CATALOG-NAMES* list."
  type name catpath dirpath)

(defun catalog-data (type)  (assoc type *catalog-names*))

(defparameter *catalog-names*
  `((:program   "PROGRAMME"   ,*lse-fic-progr*  ,*lse-rep-progr*)
    (:permanent "PERMANENT"   ,*lse-fic-perma*  ,*lse-rep-perma*)
    (:temporary "TEMPORAIRE"  nil               ,*lse-rep-tempo*)
    (:tape      "RUBAN"       nil               ,*lse-rep-ruban*)
    (:shelf     "ETAGERE"     nil               ,*lse-rep-ruban*)))


(defparameter *catalog-permanent* (make-instance 'catalog))
(defparameter *catalog-programme* (make-instance 'catalog))




;; (defun catalog-entry-path (entry)
;;   "Returns the path of the catalog entry ENTRY."
;;   (case (catalog-entry-type entry)
;;     ((:program :permanent) 
;;      (format nil "~A;~A"
;;              (catalog-data-dirpath (catalog-data (catalog-entry-type entry)))
;;              (catalog-entry-name entry)))
;;     ((:tape :shelf)
;;      (format nil "~A;~{~A;~}~A"
;;              (catalog-data-dirpath (catalog-data (catalog-entry-type entry)))
;;              (catalog-entry-shelf entry)
;;              (catalog-entry-name entry)))
;;     ((:temporary)
;;      (format nil "~A;~2,'0D;~A"
;;              (catalog-data-dirpath (catalog-data (catalog-entry-type entry)))
;;              (catalog-entry-console entry)
;;              (catalog-entry-name  entry)))
;;     (otherwise
;;      (error-panic "COMPLETER UNE ENTREE DE CATALOGUE ~@
;;                  DE TYPE ~S IMPOSSIBLE." (catalog-entry-type entry)))))


(defun catalog-entry-fill (entry)
  (let* ((path (catalog-entry-path entry))
         (result nil)
         (flen (with-open-file (f path
                                  :direction :input 
                                  :if-does-not-exist nil)
                 (if f (progn (setf result t) (file-length f)) 0))))
    (multiple-value-bind (s m h day month year)
        (decode-universal-time (or (file-write-date path) (get-universal-time)))
      (declare (ignore h m s))
      (setf (catalog-entry-day     entry) day
            (catalog-entry-month   entry) month
            (catalog-entry-year    entry) (mod year 100)
            (catalog-entry-words   entry) (truncate flen 2)
            (catalog-entry-sectors entry) (truncate flen 256)))
    result))


(defun catalog-update (catalog)
  (map nil (function catalog-entry-fill) (catalog-entries catalog))
  (values))


#|
(defun catalog-find-name (catalog name)    
  (find name (catalog-entries catalog) 
        :test (lambda (name entry)
                (if (member (catalog-type catalog) '(:tape :shelf))
                  (and (string-equal (catalog-entry-name entry) name)
                       (catalog-entry-rpath entry)))
                       (function catalog-entry-name))))));;catalog-find-name
|#

(defun catalog-find-name-console (catalog name &optional (console *current-console-number*))
  "Finds in the CATALOG the entry belonging to the CONSOLE that has the NAME." 
  (find (cons name console) (catalog-entries catalog)
        :test (lambda (no entry)
                (and (= (cdr no) (catalog-entry-console entry))
                     (string-equal (car no) (catalog-entry-name entry))))))
    
    
(defun catalog-valid-name-p (name)
  (and (<= 1 (length name) 5)
       (alpha-char-p (char name 0))
       (every (function alphanumericp) name)))


(defun catalog-add-entry-type (catalog type name &key (console *current-console-number*) (owner *current-user-number*))
  (let ((entry (make-instance 'catalog-entry 
                :console (or console 0)
                :owner   (or owner 0)
                :type type
                :name  (if (member type '(:tape :shelf)) ""   name)
                :shelf (if (member type '(:tape :shelf)) name nil))))
    (when (catalog-entry-fill entry)
      (push entry (catalog-entries catalog))
      (incf (catalog-count catalog))
      entry)))


(defun catalog-add-entry (catalog name &key (console *current-console-number*) (owner *current-user-number*))
  (catalog-add-entry-type catalog (catalog-type catalog) name
                          :console console :owner owner))


(defun catalog-add-entries (catalog names &key (console *current-console-number*) (owner *current-user-number*))   
  (dolist (name names)
    (catalog-add-entry catalog name :console console :owner owner)))


(defun catalog-delete (catalog entry)
  (setf (catalog-entries catalog) (delete  entry (catalog-entries catalog))))

    
#||
;; this is used with scandir.  We may do otherwise.

(defvar *selectionner-numero*    0)
(defvar *selectionner-catalogue* nil)


(defun selectionner-nouveau (entry))
    static int selectionner_nouveaux(const struct dirent* entree_rep)
    {
        return(
            lse_catalogue_nom_valide(entree_rep->d_name)
            &&(0>lse_catalogue_chercher_nom(selectionner_catalogue,
                                            entree_rep->d_name)));
    }/*selectionner_nouveaux*/


    static int selectionner_nouveaux_temporaire(const struct dirent* entree_rep)
    {
        return(
            lse_catalogue_nom_valide(entree_rep->d_name)
            &&(0>lse_catalogue_chercher_nom_numero(selectionner_catalogue,
                                                   entree_rep->d_name,
                                                   selectionner_numero)));
    }/*selectionner_nouveaux_temporaire*/


#define TAILLE_CHEMIN (4096)
    static char* selectionner_repertoire_courant;
    static int   selectionner_repertoire_courant_longueur;


    static int selectionner_fichier(const struct dirent* entree_rep)
        /*
            RETURN: selectionner_repertoire "/" entree_rep est un fichier.
        */
    {
        char*       chemin=selectionner_repertoire_courant;
        int         longueur=selectionner_repertoire_courant_longueur;
        struct stat buffer;
        if(lse_catalogue_nom_valide(entree_rep->d_name)){
            if(longueur+1+strlen(entree_rep->d_name)<=TAILLE_CHEMIN){
                sprintf(chemin+longueur,"/%s",entree_rep->d_name);
                if(0==stat(chemin,&buffer)){
                    chemin[longueur]='\0';
                    return(S_ISREG(buffer.st_mode));
                }
            }
        }
        chemin[longueur]='\0';
        return(0);
    }/*selectionner_fichier*/


    static int selectionner_repertoire(const struct dirent* entree_rep)
        /*
            RETURN: selectionner_repertoire "/" entree_rep est un répertoire.
        */
    {
        char*       chemin=selectionner_repertoire_courant;
        int         longueur=selectionner_repertoire_courant_longueur;
        struct stat buffer;
        if(lse_catalogue_nom_valide(entree_rep->d_name)){
            if(longueur+1+strlen(entree_rep->d_name)<=TAILLE_CHEMIN){
                sprintf(chemin+longueur,"/%s",entree_rep->d_name);
                if(0==stat(chemin,&buffer)){
                    chemin[longueur]='\0';
                    return(S_ISDIR(buffer.st_mode));
                }
            }
        }
        chemin[longueur]='\0';
        return(0);
    }/*selectionner_repertoire*/

||#


(defun catalog-fetch-tapes (catalog base-path &optional (max-depth 1))
  (when (eq (catalog-type catalog) :tape)
    (let* ((base-path (truename base-path))
           (pdir      (pathname-directory base-path))
           (plen      (length pdir)))
     (dotimes (depth max-depth catalog)
       (let* ((pmat  (make-pathname 
                      :directory (append pdir (make-list depth :initial-element :wild))
                      :name :wild :type nil :version :newest :defaults base-path))
              (files (mapcan
                      (lambda (path)
                        (let ((d (pathname-directory path)))
                          (when (equal (subseq d 0 plen) pdir)
                            ;; no truename skipping, let's assume type and version NIL.
                            (list (make-pathname 
                                   :directory (cons :relative (subseq d plen))
                                   :name (pathname-name path) :type nil :version :newest
                                   :defaults pmat)))))
                      (directory pmat))))
         (catalog-add-entries catalog files))))))


;; (let ((tapes (make-catalog :type :tape)))
;;   (catalog-fetch-tapes tapes *LSE-REP-RUBAN* 3))

;; (catalog-entry-path #S(catalog-entry :type :tape :name "" :owner 0 :console 0 :day 1 :month 1 :year 0 :words 0 :sectors 0 :shelf #P"BOURG/BOUR"))





;; (let ((path #P"/home/pjb/src/lse-repository/RUBAN/BOURG/BOUR"))
;;   (let ((d (pathname-directory path)))
;;     (when (equal (subseq d 0 plen) pdir)
;;       ;; no truename skipping, let's assume type and version NIL.
;;       (list (make-pathname 
;;              :directory (list :relative (subseq d plen))
;;              :name (pathname-name path) :type nil :version :newest
;;              :defaults pmat)))))

;; (catalog-type (make-instance 'catalog :type :tape))
;; (truename  *LSE-REP-RUBAN*)#P"/home/pjb/src/lse-repository/RUBAN/"




#||

(pathname-name (first (catalog-fetch-tapes nil "LSE:RUBAN;" "EXEMP;" 0 0)))



(catalog-fetch-tapes nil "LSE:RUBAN;" "*" 0 0)

    static int lse_catalogue_chercher_ruban(lse_catalogue_t* ceci,
                                            const char* chemin_racine,
                                            const char* chemin_relatif,
                                            int profondeur,
                                            int profondeur_max)
    {
        struct dirent** entrees=0;
        int             nombre_entrees;
        int             i;
        char            chemin[TAILLE_CHMIN];
        char*           chemin_entree;
        char*           nom_entree;
        int             total=0;

        chemin_entree=chemin+sprintf(chemin,"%s/",chemin_racine);
        if(chemin_relatif[0]=='\0'){
            nom_entree=chemin_entree;
        }else{
            nom_entree=chemin_entree
                +sprintf(chemin_entree,"%s/",chemin_relatif);
        }

        if(ceci->type==lse_catalogue_type_ruban){
            selectionner_repertoire_courant=chemin;
            selectionner_repertoire_courant_longueur=strlen(chemin);
            nombre_entrees=scandir(chemin,
                                   &entrees,
                                   selectionner_fichier,
                                   alphasort);
            total+=nombre_entrees;
            if(0<nombre_entrees){
                if(pjb_tableau_taille(ceci->entrees)
                   <1+nombre_entrees+ceci->nombre){
                    lse_catalogue_etendre(ceci,
                                          1+nombre_entrees+ceci->nombre);
                }
                for(i=0;i<nombre_entrees;i++){
                    sprintf(nom_entree,"%s",entrees[i]->d_name);
                    lse_catalogue_ajouter(ceci,LSE_COMPTE_PAR_DEFAUT,
                                          chemin_entree);
                }
                nom_entree[0]='\0';
            }
            if(entrees!=0){ 
                free_dirent_pp(entrees,nombre_entrees);
                entrees=0;
            }
        }

        if(profondeur<profondeur_max){
            selectionner_repertoire_courant=chemin;
            selectionner_repertoire_courant_longueur=strlen(chemin);
            nombre_entrees=scandir(chemin,
                                   &entrees,
                                   selectionner_repertoire,
                                   alphasort);
            if(0<nombre_entrees){
                if(ceci->type==lse_catalogue_type_etagere){
                    if(pjb_tableau_taille(ceci->entrees)
                       <1+nombre_entrees+ceci->nombre){
                        lse_catalogue_etendre(ceci,
                                              1+nombre_entrees+ceci->nombre);
                    }
                    for(i=0;i<nombre_entrees;i++){
                        sprintf(nom_entree,"%s",entrees[i]->d_name);
                        lse_catalogue_ajouter(ceci,LSE_COMPTE_PAR_DEFAUT,
                                              chemin_entree);
                    }
                    nom_entree[0]='\0';
                    total+=nombre_entrees;
                }
                for(i=0;i<nombre_entrees;i++){  
                    sprintf(nom_entree,"%s",entrees[i]->d_name);
                    total+=lse_catalogue_chercher_ruban(ceci,
                                                        chemin_racine,
                                                        chemin_entree,
                                                        profondeur+1,
                                                        profondeur_max);
                }
            }
            if(entrees!=0){ 
                free_dirent_pp(entrees,nombre_entrees);
                entrees=0;
            }
        }
        return(total);
    }/*lse_catalogue_chercher_ruban*/



    static int lse_catalogue_obtenir_nouvelles_entrees(lse_catalogue_t* ceci)
    {
        struct dirent** namelist;
        int nombre_entrees;

        switch(ceci->type){
        case lse_catalogue_type_temporaire: {
            int total=0;
            int console;
            for(console=0;console<100;console++){
                char chemin[4096];  
                int taille=sprintf(chemin,"%s/%02d",
                                   chemins_des_repertoires[ceci->type],
                                   console);
                if(taille>=sizeof(chemin)){
                    lse_paniquer("UN TAMPON A DEBORDE DANS LSE_CATALOGUE ! "
                                 "J'ARRETE TOUT !");
                }
                
                selectionner_catalogue=ceci;
                selectionner_numero=console;
                nombre_entrees=scandir(chemin,
                                       &namelist,
                                       selectionner_nouveaux_temporaire,
                                       alphasort);

                if(nombre_entrees>0){
                    lse_catalogue_ajouter_nouvelles_entrees(ceci,
                                                            console,
                                                            nombre_entrees,
                                                            namelist);
                    free_dirent_pp(namelist,nombre_entrees);
                    total+=nombre_entrees;
                }
            }
            return(total);
        }
        case lse_catalogue_type_ruban:
        case lse_catalogue_type_etagere:
            return(lse_catalogue_chercher_ruban(
                ceci,chemins_des_repertoires[lse_catalogue_type_ruban],
                "",0,3));
        default:
            selectionner_catalogue=ceci;
            nombre_entrees=scandir(chemins_des_repertoires[ceci->type],
                                   &namelist,
                                   selectionner_nouveaux,
                                   alphasort);
            if(nombre_entrees<=0){
                return(0);
            }
            lse_catalogue_ajouter_nouvelles_entrees(ceci,
                                                    LSE_COMPTE_PAR_DEFAUT,
                                                    nombre_entrees,namelist);
            free_dirent_pp(namelist,nombre_entrees);
            return(nombre_entrees);
        }
    }/*lse_catalogue_obtenir_nouvelles_entrees*/
    
    
    static void lse_catalogue_charger(lse_catalogue_t* ceci)
    {
        FILE* fichier;
        char  ligne[16]; /* 99NNNNN\n */
        char* nom=ligne+2;

        if((ceci->type<lse_catalogue_type_programme)
           ||(lse_catalogue_type_permanent<ceci->type)){
            lse_paniquer("CHARGEMENT D'UN CECI DE TYPE '%d' IMPOSSIBLE.",
                         ceci->type);
        }
        fichier=fopen(chemins_des_catalogues[ceci->type],"r");
        if(fichier==NULL){
            lse_paniquer("OUVERTURE DU CECI %s IMPOSSIBLE",
                         noms_des_catalogues[ceci->type]);
        }
        ceci->nombre=0;
        while(fgets(ligne,sizeof(ligne)-1,fichier)!=0){
            int numero;
            char* fin;
            numero=lse_chiffre(ligne[0])*10+lse_chiffre(ligne[1]);
            fin=strchr(nom,'\n');
            if(fin!=0){
                *fin='\0';
            }
            nom[5]='\0';
            lse_catalogue_ajouter(ceci,numero,nom);
        }
        fclose(fichier);
    }/*lse_catalogue_charger*/


    static void lse_catalogue_garer(lse_catalogue_t* ceci)
    {
        FILE* fichier;
        int i;
        if((ceci->type<lse_catalogue_type_programme)
           ||(lse_catalogue_type_permanent<ceci->type)){
            lse_paniquer("GARAGE D'UN CECI DE TYPE '%d' IMPOSSIBLE.",
                         ceci->type);
        }
        {
            char sauvegarde[4096];
            sprintf(sauvegarde,"%s~",chemins_des_catalogues[ceci->type]);
            unlink(sauvegarde);
            if(0==link(chemins_des_catalogues[ceci->type],sauvegarde)){
                unlink(chemins_des_catalogues[ceci->type]);
            }
        }
        fichier=fopen(chemins_des_catalogues[ceci->type],"w");
        if(fichier==NULL){
            lse_paniquer("OUVERTURE DU CECI %s IMPOSSIBLE",
                         noms_des_catalogues[ceci->type]);
        }
        for(i=0;i<ceci->nombre;i++){
            lse_catalogue_entree_t* entree=pjb_tableau_element_a(ceci->entrees,i);
            fprintf(fichier,"%02d%s\n",entree->numero,entree->nom);
        }
        fclose(fichier);
    }/*lse_catalogue_garer*/

    

    void lse_catalogue_initialiser(void)
    {
        catalogue_permanent=lse_catalogue_creer(lse_catalogue_type_permanent,16);
        pjb_objet_retenir(catalogue_permanent);
        lse_catalogue_charger(catalogue_permanent);
        catalogue_programme=lse_catalogue_creer(lse_catalogue_type_programme,16);
        pjb_objet_retenir(catalogue_programme);
        lse_catalogue_charger(catalogue_programme);
        lse_catalogue_synchroniser();
    }/*lse_catalogue_initialiser*/


    void lse_catalogue_synchroniser(void)
extern void lse_catalogue_synchroniser(void);
/* charge les création/suppression de fichier du disque, et 
   enregistre le catalogue mémoire sur disque, */

    {
        if(0<lse_catalogue_obtenir_nouvelles_entrees(catalogue_permanent)){
            lse_catalogue_garer(catalogue_permanent);
        }
        if(0<lse_catalogue_obtenir_nouvelles_entrees(catalogue_programme)){
            lse_catalogue_garer(catalogue_programme);
        }
   }/*lse_catalogue_synchroniser*/


    void lse_catalogue_terminer(void)
    /* appelle lse_catalogue_synchroniser et nettoie la mémoire */
    {
        lse_catalogue_synchroniser();
        pjb_objet_relacher(catalogue_permanent);
        catalogue_permanent=0;
        pjb_objet_relacher(catalogue_programme);
        catalogue_programme=0;
    }/*lse_catalogue_terminer*/


    static void lse_catalogue_selectionner(lse_catalogue_t* ceci,
                                           lse_catalogue_t* extrait,
                                           int numero)
    {
        int i;
        for(i=0;i<ceci->nombre;i++){
            lse_catalogue_entree_t* entree=pjb_tableau_element_a(ceci->entrees,i);
            if(entree->numero==numero){
                int taille=pjb_tableau_taille(extrait->entrees);
                if(extrait->nombre>=taille){
                    lse_catalogue_etendre(extrait,2*taille);
                }
                pjb_tableau_changer_element_a(extrait->entrees,
                                            extrait->nombre++,
                                            entree);
            }
        }
    }/*lse_catalogue_selectionner*/


    lse_catalogue_t* lse_catalogue_programmes(int compte)
    {
        if(compte==lse_compte_tous){
            lse_catalogue_actualiser(catalogue_programme);
            return(catalogue_programme);
        }else{
            lse_catalogue_t* ceci;
            ceci=lse_catalogue_creer(lse_catalogue_type_programme,
                                     catalogue_programme->nombre+1);
            lse_catalogue_selectionner(catalogue_programme,ceci,compte);
            return(ceci);
        }
    }/*lse_catalogue_programmes*/


    lse_catalogue_t* lse_catalogue_permanents(int compte)
    {
        if(compte==lse_compte_tous){
            lse_catalogue_actualiser(catalogue_programme);
            return(catalogue_permanent);
        }else{
            lse_catalogue_t* ceci;
            ceci=lse_catalogue_creer(lse_catalogue_type_permanent,
                                            catalogue_permanent->nombre+1);
            lse_catalogue_selectionner(catalogue_permanent,ceci,compte);
            return(ceci);
        }
    }/*lse_catalogue_permanents*/


    lse_catalogue_t* lse_catalogue_temporaires(int console)
    {
        lse_catalogue_t* temporaire;
        int nouvelles;
        temporaire=lse_catalogue_creer(lse_catalogue_type_temporaire,16);
        nouvelles=lse_catalogue_obtenir_nouvelles_entrees(temporaire);
        if((nouvelles==0)||(console==lse_compte_tous)){
            return(temporaire);
        }else{
            lse_catalogue_t* ceci;
            ceci=lse_catalogue_creer(lse_catalogue_type_temporaire,
                                     temporaire->nombre+1);
            lse_catalogue_selectionner(temporaire,ceci,console);
            return(ceci);
        }
    }/*lse_catalogue_temporaires*/


    lse_catalogue_t* lse_catalogue_rubans(void)
    {
        lse_catalogue_t* ceci;
        ceci=lse_catalogue_creer(lse_catalogue_type_ruban,16);
        (void)lse_catalogue_obtenir_nouvelles_entrees(ceci);
        return(ceci);
    }/*lse_catalogue_rubans*/


    lse_catalogue_t* lse_catalogue_etageres(void)
    {
        lse_catalogue_t* ceci;
        ceci=lse_catalogue_creer(lse_catalogue_type_etagere,16);
        (void)lse_catalogue_obtenir_nouvelles_entrees(ceci);
        return(ceci);
    }/*lse_catalogue_etageres*/


   int lse_catalogue_secteurs_libres(void)
    {
        return(156);
    }/*lse_catalogue_secteurs_libres(*/



    int lse_catalogue_max_taille_chemin(void)
    {
        int s;
        int m=0;
        s=strlen(chemins_des_repertoires[lse_catalogue_type_temporaire])+10;
        if(s>m){ m=s; }
        s=strlen(chemins_des_repertoires[lse_catalogue_type_programme])+7;
        if(s>m){ m=s; }
        s=strlen(chemins_des_repertoires[lse_catalogue_type_permanent])+7;
        if(s>m){ m=s; }
        s=strlen(chemins_des_repertoires[lse_catalogue_type_ruban])+18;
        if(s>m){ m=s; }
        return(m);
    }/*lse_catalogue_max_taille_chemin*/

||#


(defun catalog-make-path (type name &key (console *current-console-number*))
  "
PRE:    (eql type :temporary) ==> console must be provided.
RETURN: A pathname for the file named NAME in the catalog of type TYPE.
"
  
  );;catalog-make-path


#||
    void lse_catalogue_construire_chemin(lse_catalogue_type_t type,
                                         int console,
                                         const char* nom,
                                         char* chemin,
                                         int taille_chemin)
    {
        int taille=0;
        switch(type){
        case lse_catalogue_type_temporaire:
            taille=sprintf(chemin,"%s/%02d/%s",
                           chemins_des_repertoires[type],
                           console,
                           nom);
            break;
        case lse_catalogue_type_programme:
        case lse_catalogue_type_permanent:
        case lse_catalogue_type_ruban:
            taille=sprintf(chemin,"%s/%s",
                           chemins_des_repertoires[type],
                           nom);
            break;
        default:
            lse_paniquer("IMPOSSIBLE DE CONSTRUIRE UN CHEMIN DE TYPE '%d'.",
                         type);
            break;
        }
        if(taille>=taille_chemin){
            lse_paniquer("UN TAMPON A DEBORDE DANS LSE_CATALOGUE ! "
                         "J'ARRETE TOUT !");
        }   
    }/*lse_catalogue_construire_chemin*/


    lse_erreur_t lse_catalogue_cataloguer(int console,
                                          const char* temporaire,
                                          int compte,
                                          const char* permanent)
    {
        char chemin_temporaire[4096];
        char chemin_permanent[4096];
        if(!lse_catalogue_nom_valide(temporaire)
           ||!lse_catalogue_nom_valide(permanent)){
            return(lse_erreur_nom_de_fichier_invalide);
        }
        
        if(0<=lse_catalogue_chercher_nom(catalogue_permanent,permanent)){
            return(lse_erreur_nom_de_fichier_deja_pris);
        }

        lse_catalogue_construire_chemin(lse_catalogue_type_temporaire,
                                        console,temporaire,
                                        chemin_temporaire,
                                        sizeof(chemin_temporaire));
        lse_catalogue_construire_chemin(lse_catalogue_type_permanent,
                                        console,permanent,
                                        chemin_permanent,
                                        sizeof(chemin_permanent));

        if(0==link(chemin_temporaire,chemin_permanent)){
            unlink(chemin_temporaire);
            lse_catalogue_ajouter(catalogue_permanent,compte,permanent);
            return(lse_ok);
        }else{
            return(lse_erreur_cataloguage_fichier);
        }
    }/*lse_catalogue_cataloguer*/


    lse_erreur_t lse_catalogue_ajouter_fichier(lse_catalogue_type_t type,
                                               int numero,
                                               const char* nom)
    {
        lse_catalogue_t* ceci=0;
        switch(type){
        case lse_catalogue_type_programme:
            ceci=catalogue_programme;
            break;
        case lse_catalogue_type_permanent:
            ceci=catalogue_permanent;
            break;
        case lse_catalogue_type_temporaire:
            ceci=0;
            break;
        default:
            lse_paniquer("IMPOSSIBLE D'AJOUTER UNE ENTREE DE CATALOGUE "
                         "DE TYPE '%d'.",type);
        }
        if(ceci!=0){
            int idx=lse_catalogue_chercher_nom(ceci,nom);
            if(idx<0){
                return(lse_catalogue_ajouter(ceci,numero,nom));
            }
        }
        return(lse_ok);
    }/*lse_catalogue_ajouter_fichier*/
||#


(defun catalog-delete-file (type name &key (console *current-console-number*) (user *current-user-number*))
  "
PRE:  (eql TYPE :temporary) ==> console must be provided.
      (member TYPE '(:program :permanent) ==> user must be provided.
DO:   if (member type '(:program :permanent :temporary)) 
      then delete the file
      else panic.
"
  (let ((catalog (case type
                   ((:program)   *catalog-program*) 
                   ((:permanent) *catalog-permanent*)
                   ((:temporary) nil)
                   (otherwise
                    (lse-panic 
                     "IMPOSSIBLE DE SUPPRIMER UNE ENTREE DE CATALOGUE ~:
                      DE TYPE ~S." type)))))
    (when catalog
      (let ((entry (catalog-find-name catalog name)))
        (when entry
          (when (/= (catalog-entry-owner entry) user)
            (return-from catalog-delete-file
              +suppression-impossible-pas-proprietaire+)))))
    (error "Not implemented yet")))

#||
        lse_catalogue_construire_chemin(type,numero,nom,
                                        chemin,sizeof(chemin));
        if(0==unlink(chemin)){
            if((ceci!=0)&&(0<=idx)){
                lse_catalogue_enlever(ceci,idx);
            }
            return(lse_ok);
        }else{
            return(lse_erreur_suppression_fichier);
        }
    }/*lse_catalogue_supprimer_fichier*/

||#

(defun catalog-delete-temporaries (console)
  (dolist (file (directory 
                 (merge-pathnames
                  (make-pathname
                   :directory (list :relative (format nil "~2,'0D" console))
                   :name :wild :type :wild :version :wild)
                  *lse-rep-tempo*)))
    (delete-file file))
  (values))


;;;; catalog.lisp                     --                     --          ;;;;
