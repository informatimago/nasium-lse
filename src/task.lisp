;;;; -*- mode:lisp; coding:utf-8 -*-
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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2000 - 2004
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


(defun task-state-label (state)
  (ecase state
    (:to-connect "A CONNECTER")
    (:in-limbo   "IN LIMBO")
    (:sleeping   "DORMANT")
    (:active     "MONITEUR")))


(defclass task ()

  ((console              :initarg :console
                         :reader task-console
                         :initform 0
                         :type fixnum)
   (account              :initarg :account
                         :reader task-account
                         :initform 0
                         :type fixnum)
   (state                :initarg :state
                         :accessor task-state
                         :initform :in-limbo
                         :type (member :in-limbo :to-connect :sleeping :active))

   (input                :initform nil
                         :reader task-input-stream
                         :documentation "current input stream.
Can be either (terminal-input-stream terminal) or tape-input.")
   (output               :initform nil
                         :reader task-output-stream
                         :documentation "current output stream.
Can be either (terminal-output-stream terminal) or tape-output.")

   (terminal             :initarg :terminal
                         :reader task-terminal
                         :initform nil
    :documentation "user interaction terminal.")
  
   (tape-input           :initarg :tape-input
                         :initform nil
                         :documentation "stream? when a tape is loaded.")
   (tape-output          :initarg :tape-output
                         :initform nil
                         :documentation "stream? to a temporary tape")
  

   (case-insensitive     :initarg :case-insensitive
                         :accessor task-case-insensitive
                         :initform nil
                         :type boolean
                         :documentation "whether the input should be case insensitive.") 

   (arrows               :initarg :arrows
                         :accessor task-arrows
                         :initform :ascii
                         :type (member :ascii :dectech :unicode :unicode-halfwidth)
                         :documentation "How to output arrows _ et ^.")
   (unicode              :initarg :unicode
                         :accessor task-unicode
                         :initform nil
                         :type boolean
                         :documentation "Whether the terminal encoding is utf-8.")
   (upcase-output        :initarg :upcase-output
                         :accessor task-upcase-output
                         :initform t
                         :type boolean
                         :documentation "whether the output should be upcased.")
   (accented-output      :initarg :accented-output
                         :accessor task-accented-output
                         :initform t
                         :type boolean
                         :documentation "Whether the output can contain accented characters.")

   (abreger              :accessor task-abreger          :initform nil :type boolean
                         :documentation "AB)REGER - le système ne complète pas les commandes.")
   (silence              :reader task-silence            :initform nil :type boolean
                         :documentation "SI)LENCE - suppression de l'echo (utilisateur, ruban).") 
   (pas-a-pas            :accessor task-pas-a-pas        :initform nil :type boolean
                         :documentation "PA)S A PAS - execution pas à pas.")
   (pas-a-pas-first      :accessor task-pas-a-pas-first
                         :initform nil
                         :documentation "Set to true when pas-a-pas just stopped,
so that if next command is ine, we continue automatically.")
   (interruption         :accessor task-interruption     :initform nil :type boolean
                         :documentation "ESC")
   (signal               :accessor task-signal           :initform nil :type boolean
                         :documentation "CTRL-A  Utilisé par ATT()")

   (echo                 :initarg :echo
                         :accessor task-echo
                         :initform nil
                         :type boolean
                         :documentation "parameter io-read-line.")
   
   (random-state         :accessor task-random-state
                         :initform (make-random-state t) :type random-state)
   (environnement        :accessor task-environnement :initform nil :type (or null environnement))
   (vm                   :reader task-vm
                         :initform (make-instance 'lse-vm) :type lse-vm)
   (files                :reader task-files
                         :initform (make-hash-table :test 'equalp)
                         :documentation "Maps file names to open FILE objects.")))


(defmethod (setf task-silence) (new-flag (task task))
  (setf (io-echo *task*)           (not new-flag)
        (slot-value task 'silence) (not (not new-flag))))


(defmethod (setf task-input) (stream (task task))
  (setf (slot-value task 'input) stream))

(defmethod task-input ((task task))
  (with-slots (input) task
    (when (null input) 
      (setf input (terminal-input-stream (task-terminal task))))
    input))

(defmethod (setf task-output) (stream (task task))
  (setf (slot-value task 'output) stream))

(defmethod task-output ((task task))
  (with-slots (output) task
    (when (null output) 
      (setf output (terminal-output-stream (task-terminal task))))
    output))

(defmethod (setf task-tape-input) (stream (task task))
  (setf (slot-value task 'tape-input) stream))

(defmethod task-tape-input ((task task))
  (with-slots (tape-input) task
    (when (null tape-input)
      (setf tape-input (make-string-input-stream "")))
    tape-input))

(defmethod (setf task-tape-output) (stream (task task))
  (setf (slot-value task 'tape-output) stream))

(defmethod task-tape-output ((task task))
  (with-slots (tape-output) task
    (when (null tape-output)
      (let ((path (catalog-pathname "RUBAN" "S")))
        (setf tape-output (open path
                                :direction :io
                                :if-does-not-exist :create
                                :if-exists :supersede))))
    tape-output))





(defmethod task-close-all-files ((task task))
  (maphash (lambda (name file)
             (declare (ignore name))
             (lse-data-file-close file))
           (task-files task))
  task)

(defun file-key (name fictype)
  (let* ((fictype  (cond
                     ((string-equal fictype "D") :data)
                     ((string-equal fictype "T") :temporary)
                     ((member fictype '(:data :temporary)) fictype)
                     (t (error 'lse-error
                               :format-control "INDICATEUR DE TYPE DE FICHIER INVALIDE: ~A; ATTENDU: D OU T."
                               :format-arguments (list fictype))))))
    (cons name fictype)))

(defmethod task-open-file ((task task) name fictype &key (if-does-not-exist :create))
  (let ((key (file-key name fictype)))
    (or (gethash key (task-files task))
        (let ((file (handler-case (lse-data-file-open (catalog-pathname name fictype)
                                                      :if-does-not-exist if-does-not-exist)
                      (file-error (err)
                        (error-no-file name fictype (file-error-pathname err))))))
          (when file
            (setf (gethash key (task-files task)) file))))))

(defmethod task-close-file ((task task) name fictype)
  (let* ((key (file-key name fictype))
         (file (gethash key (task-files task))))
    (when file
      (remhash key (task-files task))
      (lse-data-file-close file)))
  task)

(defmethod task-delete-file ((task task) name fictype)
  (task-close-file task name fictype)
  (handler-case (delete-file (catalog-pathname name fictype))
    (file-error (err)
      (error-no-file name fictype (file-error-pathname err) t))))



(defun task-state-in-limbo-p   (task)  (eq :in-limbo   (task-state task)))
(defun task-state-to-connect-p (task)  (eq :to-connect (task-state task)))
(defun task-state-sleeping-p   (task)  (eq :sleeping   (task-state task)))
(defun task-state-active-p     (task)  (eq :active     (task-state task)))
(defun task-state-awake-p      (task)  (eq :active     (task-state task)))


(defparameter *task-count*  0   "Fixed number of tasks.")
(defparameter *tasks*       '() "The list of tasks.")
(defparameter *task-mutex*  nil "pthread_mutex_t")
(defparameter *task*        nil "The current task")


(defun task-initialize (task-count)
  "Appeler une seule fois dans le programme."
  ;; pthread_mutex_init(&mutex,NULL);
  (setf *task-count* task-count 
        *tasks*      nil)
  (dotimes (i *task-count*)
    (push (make-instance 'task :console (1- (- *task-count* i)))
          *tasks*))
  (values))


(defun task-count () *task-count*)
(defun console-task (console) (elt *tasks* console))


(defun task-inlimbo ()
  "(thread-safe)
Retourne un task qui était inlimbo (il est maintant aconnecter).
"
  ;;pthread_mutex_lock(&mutex);
  (unwind-protect
      (let ((task (find-if (function task-state-inlimbo-p) *tasks*)))
        (when task
          (setf (task-state task) :to-connect))
        task)
    (progn ;; pthread_mutex_unlock(&mutex);
      )))

(defmethod task-disconnect ((task task))
  (declare (ignorable task))
  ;; for socket tasks, disconnect it.
  ;; for stdio task, exit?
  nil)


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
