;;;;****************************************************************************
;;;;FILE:               task.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    EMULSE : L.S.E. [ EMULATION MITRA-15 ]
;;;;    
;;;;    An emulator of the CII MITRA-15 L.S.E. System 
;;;;    and programming language interpreter.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-08-02 <PJB> Converted to Common-Lisp.
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

(defstruct task
  (console 0 :type fixnum)
  (account 0 :type fixnum)
  (state   :in-limbo :type (member :in-limbo :to-connect :sleeping :active))
  (entree) ;; stream?
  (sortie) ;; stream?
  (entree-terminal) ;; stream?
  (sortie-terminal) ;; stream?
  (entree-ruban) ;; stream?
  (sortie-ruban) ;; stream?
  (abreger       nil :type boolean) ;; AB-REGER
  (silence       nil :type boolean) ;; SI-LENCE
  (interruption  nil :type boolean) ;; ESC
  (signal        nil :type boolean) ;; CTRL-A  Utilisé par ATT()
  (dectech       nil :type boolean) ;; police DecTech pour _ et ^.
  (echo          nil :type boolean) ;; parameter lse_es_lire_ligne
  (random-state  (make-random-state) :type random-state)
  (scanner       (make-instance 'scanner))
  (environnement nil :type (or null environnement))
  (decodeur-nom-fichier nil :type (or null ident))  ;; paramètre
  (decodeur-numero      nil :type (or null nombre)) ;; 
  (erreur               nil :type (or null erreur)) ;; resultat
;;         pthread_t               principal;
;;         pthread_mutex_t         mutex_principal;
;;         pthread_cond_t          condi_principal;
;;         pthread_t               terminal;
;;         pthread_mutex_t         mutex_terminal;
;;         pthread_cond_t          condi_terminal;
;; 
;;         pthread_mutex_t         mutex_tampon_entree;
  
;;         lse_chaine_t*           tampon_entree; /* parametre lse_es_lire_ligne */
;;         int                     terminateurs;  /* parametre lse_es_lire_ligne */
;; 
;;         struct sockaddr_in      remote_addr;
;;         
;;         lse_scanner_FlexLexer*  scanner;
;;         lse_chaine_t*           scanner_source;
;;         int                     scanner_position;
;;         int                     scanner_colonne;
;;      
;;         unsigned int            graine;
;;         int                     graine_semee;
        );;task


(defun task-state-in-limbo-p   (task)  (eq :in-limbo   (task-state task)))
(defun task-state-to-connect-p (task)  (eq :to-connect (task-state task)))
(defun task-state-sleeping-p   (task)  (eq :sleeping   (task-state task)))
(defun task-state-active-p     (task)  (eq :active     (task-state task)))
(defun task-state-awake-p      (task)  (eq :active     (task-state task)))

#||
/* methodes d'instance : */

    extern void lse_task_connecter(lse_task_t* task,
                                      int entree,int sortie);
        /*
            POST:   lse_task_etat(task)==lse_task_etat_dormant.
            NOTE:   Les threads sont activés. 
                    terminal est en attente d'une E/S, et 
                    principal est en attente de terminal...
        */

    extern void lse_task_deconnecter(lse_task_t* task);
        /*
            POST:   lse_task_etat(task)==lse_task_etat_inlimbo.
            NOTE:   Les threads sont de nouveau suspendus.
        */

    extern int  lse_task_etat(lse_task_t* task);

    extern void lse_task_etat_changer(lse_task_t* task,
                                         int nouvel_etat);

||#


(defparameter *task-count* 0   "Fixed number of tasks.")
(defparameter *tasks*      '() "The list of tasks.")
(defparameter *task-mutex* nil "pthread_mutex_t")
(defparameter *task* nil "The current task")

(defun task-initialize (task-count)
  "Appeler une seule fois dans le programme."
  ;; pthread_mutex_init(&mutex,NULL);
  (setf *task-count* task-count 
        *tasks*      nil)
  (dotimes (i *task-count*)
    (push (task-create (1- (- *task-count* i))) *tasks*))
  (values));;task-initialize


(defun task-count () *task-count*)
(defun task-console (console) (elt *tasks* console))


(defun task-inlimbo ()
  "(thread-safe)
Retourne un task qui était inlimbo (il est maintant aconnecter).
"
  ;;pthread_mutex_lock(&mutex);
  (unwind-protect
      (let ((result (find-if (function task-state-inlimbo-p) *tasks*)))
        (when result (task-state-change task :to-connect))
        result)
    (progn ;; pthread_mutex_unlock(&mutex);
      )));;task-inlimbo


#||
    static pthread_key_t    lse_task_cle;
    static int              lse_task_cle_initialisee=0;


    lse_task_t* lse_task_du_fil(void)
        /*
            RETURN: Le task associé au fil courrant.
        */
    {
        return((lse_task_t*)pthread_getspecific(lse_task_cle));
    }/*lse_task_du_fil*/


    static void* lse_task_principal(void* t)
    {
        lse_task_t* task=(lse_task_t*)t;
        if(!lse_task_cle_initialisee){
            pthread_key_create(&lse_task_cle,NULL);
            lse_task_cle_initialisee=1;
        }
        pthread_setspecific(lse_task_cle,task);
        while(1){
            /* attendons qu'un terminal soit connecté */
            pthread_mutex_lock(&(task->mutex_principal));
            pthread_cond_wait(&(task->condi_principal),
                              &(task->mutex_principal));
            pthread_mutex_unlock(&(task->mutex_principal));
            /* tasklons */
            lse_cmd_interprete(task);
        }
        return(0);
    }/*lse_task_principal*/


    static void* lse_task_terminal(void* t)
    {
        lse_task_t* task=(lse_task_t* )t;
        while(1){
            /* attendons qu'un terminal soit connecté */
            pthread_mutex_lock(&(task->mutex_terminal));
            pthread_cond_wait(&(task->condi_terminal),
                              &(task->mutex_terminal));
            pthread_mutex_unlock(&(task->mutex_terminal));
            pthread_cond_signal(&(task->condi_principal));
            
            lse_es_terminal(task);
        }
        return(0);
    }/*lse_task_terminal*/



    static void lse_task_supprimer(void* objet)
    {
        lse_task_t*  ceci=objet;
        if(ceci->etat>=lse_task_etat_dormant){
            lse_task_deconnecter(ceci);
        }
        pjb_objet_relacher(ceci->environnement);
    }/*lse_task_supprimer*/

    static pjb_objet_methodes_t lse_task_methodes={ lse_task_supprimer };


    lse_task_t* lse_task_creer(int console)
        /*
            NOTE:   Invoqué par lse_task_initialiser.
            DOES:   Initialise un nouveau task; créé les deux threads,
                    mais comme il n'est pas connecté, ils sont suspendus.
            POST:   lse_task_etat(task)==lse_task_etat_inlimbo.
        */

    {
        lse_task_t* task=(lse_task_t*)pjb_objet_creer(
            sizeof(lse_task_t),
            &lse_task_methodes);
        task->console=console;
        task->account=lse_account_anonyme;
        task->etat=lse_task_etat_inlimbo;
        task->abreger=0;
        task->silence=0;
        task->interruption=0; /* ESC    */
        task->signal=0;      /* CTRL-A */
        task->dectech=0;
        task->entree=-1;
        task->sortie=-1;
        task->entree_terminal=-1;
        task->sortie_terminal=-1;
        task->entree_ruban=-1;
        task->sortie_ruban=-1;
        task->environnement=0;  

        pthread_mutex_init(&(task->mutex_principal),0);
        pthread_cond_init(&(task->condi_principal),0);

        pthread_mutex_init(&(task->mutex_terminal),0);
        pthread_cond_init(&(task->condi_terminal),0);

        pthread_mutex_init(&(task->mutex_tampon_entree),0);
        task->tampon_entree=0;    /* parametre de lse_es_lire_ligne */
        task->terminateurs=mXOFF; /*    "                 "         */
        task->echo=1;             /*    "                 "         */
        
        task->scanner_source=0;
        task->scanner_position=0;
        task->scanner_colonne=0;
        task->graine=0;
        task->graine_semee=0;

        pthread_create(&(task->principal),0,
                       lse_task_principal,(void*)task);
        pthread_create(&(task->terminal),0,
                       lse_task_terminal,(void*)task);

        return(task);
    }/*lse_task_nouveau*/


    static void lse_task_nettoyer(lse_task_t* task)
    {
        task->account=lse_account_anonyme;
        task->abreger=0;
        task->silence=0;
        lse_environnement_effacer_programme(task->environnement);
    }/*lse_task_nettoyer*/



    static void lse_task_preparer(lse_task_t* task)
    {
        task->environnement=lse_environnement_creer();
        pjb_objet_retenir(task->environnement);
    }/*lse_task_preparer*/
||#


;;      *
;;      |
;;      |
;;      V
;;  +--------+  connecter     +----------+   BOnjour     +----------+
;;  | Limbo  |--------------->| Sleeping |-------------->| Active   |
;;  |        |                |          |  /preparer    |          |
;;  |        |                |          |               |          |
;;  |        |   ADieux/      |          |               |          |
;;  |        |   deconnecter  |          |   AUrevoir    |          |
;;  |        |<---------------|          |<--------------|          |
;;  +--------+                +----------+  /nettoyer    +----------+
;;      ^                                                     |
;;      |               etat_cmd_changer(0)                   |
;;      +-----------------------------------------------------+
;;                       /nettoyer


 
#||
    void lse_task_connecter(lse_task_t* task,int entree,int sortie)
    {
        task->entree_terminal=entree;
        task->sortie_terminal=sortie;
        task->entree=entree;
        task->sortie=sortie;
        lse_task_etat_changer(task,lse_task_etat_dormant);

        pthread_cond_signal(&(task->condi_terminal));
       
        lse_ecrire_format(task,"\ec\r\n");
        lse_ecrire_format(task,"\n\n\n\n\n\n\n\n");
        lse_ecrire_format(task,"\n\n\n\n\n\n\n\n");
        lse_ecrire_format(task,"\n\n\n\n\n\n\n\n");
        lse_ecrire_format(task,"%s\r\n",HEADER);
        lse_ecrire_format(task,"VERSION : %s\r\n",lse_version);
        lse_ecrire_format(task,"%s\r\n",COPYRIGHT);
        lse_ecrire_format(task,"\r\nCONSOLE NO. %02d\r\n\r\n",
                          task->console);
    }/*lse_task_connecter*/


    void lse_task_deconnecter(lse_task_t* task)
    {
        if(task->entree_terminal>=0){
            close(task->entree_terminal);
            task->entree_terminal=-1;
        }
        if(task->sortie_terminal>=0){
            close(task->sortie_terminal);
            task->sortie_terminal=-1;
        }
        if(task->entree_ruban>=0){
            close(task->entree_ruban);
            task->entree_ruban=-1;
        }
        if(task->sortie_ruban>=0){
            close(task->sortie_ruban);
            task->sortie_ruban=-1;
        }
        task->entree=-1;
        task->sortie=-1;
        lse_task_etat_changer(task,lse_task_etat_inlimbo);
    }/*lse_task_deconnecter*/



    void lse_task_etat_changer(lse_task_t* task,int nouvel_etat)
    {
        switch(task->etat){
        case lse_task_etat_inlimbo:
        case lse_task_etat_aconnecter:
            switch(nouvel_etat){
            case lse_task_etat_inlimbo:
            case lse_task_etat_aconnecter:
                task->etat=nouvel_etat;
                break;
            case lse_task_etat_dormant:
                /* connection ; NOP */
                task->etat=nouvel_etat;
                break;
            case lse_task_etat_actif:
                lse_task_preparer(task);
                task->etat=nouvel_etat;
                break;
            default:
                /* ??? */
                break;
            }
            break;
        case lse_task_etat_dormant:
            switch(nouvel_etat){
            case lse_task_etat_inlimbo:
            case lse_task_etat_aconnecter: /* pas normal */
                /* deconnection ; NOP */
                task->etat=nouvel_etat;
                break;
            case lse_task_etat_dormant:
                /* NOP */
                break;
            case lse_task_etat_actif:
                lse_task_preparer(task);
                task->etat=nouvel_etat;
                break;
            default:
                /* ??? */
                break;
            }
            break;
        case lse_task_etat_actif:
            switch(nouvel_etat){
            case lse_task_etat_inlimbo:
            case lse_task_etat_aconnecter: /* pas normal */
            case lse_task_etat_dormant:
                lse_task_nettoyer(task);
                task->etat=nouvel_etat;
                break;
            case lse_task_etat_actif:
                /* NOP */
                break;
            default:
                /* ??? */
                break;
            }
            break;
        default:
            /* ??? */
            break;
        }
    }/*lse_task_etat_changer*/

||#


;;;; THE END ;;;;
