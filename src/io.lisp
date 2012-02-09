;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               io.lisp
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
;;;;    This is the L.S.E. I/O module.
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

(defconstant +NUL+    0)
(defconstant +SOH+    1)
(defconstant +STX+    2)
(defconstant +ETX+    3)
(defconstant +EOT+    4)
(defconstant +ENQ+    5)
(defconstant +ACK+    6)
(defconstant +BEL+    7)
(defconstant +BS+     8)
(defconstant +HT+     9)
(defconstant +LF+    10)
(defconstant +VT+    11)
(defconstant +FF+    12)
(defconstant +CR+    13)

(defconstant +SO+    14)
(defconstant +SI+    15)
(defconstant +DLE+   16)
(defconstant +DC1+   17)
(defconstant +DC2+   18)
(defconstant +DC3+   19)
(defconstant +DC4+   20)
(defconstant +NAK+   21)
(defconstant +SYN+   22)
(defconstant +ETB+   23)
(defconstant +CAN+   24)
(defconstant +EM+    25)
(defconstant +SUB+   26)
(defconstant +ESC+   27)
(defconstant +FS+    28)
(defconstant +GS+    29)
(defconstant +RS+    30)
(defconstant +US+    31)
(defconstant +SP+    32)
(defconstant +DEL+   127)
(defconstant +XON+   +DC1+)
(defconstant +XOFF+  +DC3+)



(defun io-standard-redirection (task)
  (io-stop-tape-puncher task)
  (io-stop-tape-reader  task)
  (setf (task-silence task) nil)
  (values));;io-standard-redirection


(defun io-redirect-input-from-tape (task tape-name)
  "
RETURN: Then task-tape-input or nil.
"
  (when (task-tape-input task)
    (close (task-tape-input task))
    (setf  (task-tape-input task) nil))
  (setf (task-input task) (task-terminal-input task))
  (let ((path (catalog-make-path :tape name)))
    (if path
      (if (setf (task-tape-input task)
                (open path :direction :input :if-does-not-exists nil))
        (setf (task-input task) (task-tape-input task))
        (error-repport task :fichier-inaccessible))
      (error-report task :fichier-inexistant)))
  (task-tape-input task));;io-redirect-input-from-tape


(defun io-redirect-output-to-tape (task tape-name)
  "
RETURN: Then task-tape-output or nil.
"
  (when (task-tape-output task)
    (close (task-tape-output task))
    (setf  (task-tape-output task) nil))
  (setf (task-output task) (task-terminal-output task))
  (let ((path (catalog-make-path :tape name)))
    (if path
      (if (setf (task-tape-output task)
                (open path :direction :output
                      :if-does-not-exists :create
                      :if-exists :supersede))
        (setf (task-output task) (task-tape-output task))
        (error-repport task :fichier-inaccessible))
      (error-report task :fichier-inexistant)))
  (task-tape-output task));;io-redirect-output-to-tape


(defun io-valid-tape-name-p (name)
  "
RETURN: Whether name is a string with the following format:
        name ::= part | part '/' name .
        part ::= [A-Za-z][A-Za-z0-9]{0,4} /
"
  (do ((i 0)
       (l 0)
       (state :first))
      ((>= i (length name)) t)
    (if (eq state :first)
      (if (alpha-char-p (char name i))
        (setf i (1+ i) l 1 state :rest)
        (return-from io-valid-tape-name-p nil))
      (cond
       ((alphanumericp (char name i))
         (setf i (1+ i) l (1+ l))
         (when (> l 5) (return-from io-valid-tape-name-p nil)))
       ((char= (character "/") (char name i))
        (setf i (1+ i) state :first))
       (t
        (return-from io-valid-tape-name-p nil))))))


#||

    static const int entreesParPage=18;

    short lse_es_activer_perforateur_de_ruban(lse_travail_t* travail)
    {
        lse_chaine_t ligne;
        lse_catalogue_t* catalogue;
        int i;
        int p;
        int np;

        catalogue=lse_catalogue_etageres();
        p=0;
        np=(catalogue->nombre+entreesParPage-1)/entreesParPage;
        for(i=0;i<catalogue->nombre;i++){
            lse_catalogue_entree_t* entree;
            if(i%entreesParPage==0){
                p++;
                if(i>0){
                    lse_ecrire_format(travail,"PAGE SUIVANTE...\a");
                    lse_lire_ligne(travail,&ligne,1,mXOFF);
                }
                lse_ecrire_format(
                    travail,"\r\n"
                    "\r\nETAGERES DE RUBANS ARCHIVES DANS L'ARMOIRE (%2d/%2d)"
                    "\r\n**************************************************"
                    "\r\n"
                    "\r\n          NOM              DATE"
                    "\r\n",p,np);
            }
            entree=pjb_tableau_element_a(catalogue->entrees,i);
            lse_ecrire_format(travail,"\r\n%-25s%02d/%02d/%02d",
                              entree->chemin_relatif,
                              entree->jour,
                              entree->mois,
                              entree->annee);
        }   
        while(i%entreesParPage!=0){
            lse_ecrire_format(travail,"\n");
            i++;
        }
        lse_ecrire_format(travail,
                          "\r\nSOUS QUEL NOM FAUT-IL ARCHIVER CE RUBAN "
                          "(ETAGERE/NOM) ? \a");
        lse_lire_ligne(travail,&ligne,1,mXOFF);
        if(!lse_nom_de_ruban_est_valide(&ligne)){
            lse_erreur_formater(travail,
                                "LE NOM DE RUBAN DONNE EST INVALIDE");
            return(0);
        }
        catalogue=lse_catalogue_rubans();
        if(lse_catalogue_chercher_nom(catalogue,ligne.caracteres)>=0){
            lse_chaine_t    reponse;
            int             rep=-1;
            do{
                lse_ecrire_format(travail,"\r\nFAUT-IL JETER LE RUBAN "
                                  "EXISTANT DEJA SOUS CE NOM (OUI/NON) ? \a");
                lse_lire_ligne(travail,&reponse,1,mXOFF);
                if(strcmp(reponse.caracteres,"OUI")==0){
                    rep=1;
                }else if(strcmp(reponse.caracteres,"NON")==0){
                    rep=0;
                }
            }while(rep<0);
            if(rep==0){
                lse_ecrire_format(travail,"\r\nPERFORATION ANNULEE.");
                return(0);
            }
        }
        lse_es_rediriger_sortie_sur_ruban(travail,&ligne);
        return(1);
    }/*lse_es_activer_perforateur_de_ruban*/


    void lse_es_arreter_perforateur_de_ruban(lse_travail_t* travail)
    {
        travail->sortie=travail->sortie_terminal;
    }/*lse_es_arreter_perforateur_de_ruban*/


    short lse_es_activer_lecteur_de_ruban(lse_travail_t* travail)
    {
        lse_chaine_t     ligne;
        lse_catalogue_t* rubans;
        int i;
        int p;
        int np;
        rubans=lse_catalogue_rubans();
        p=0;
        np=(rubans->nombre+entreesParPage-1)/entreesParPage;
        for(i=0;i<rubans->nombre;i++){
            lse_catalogue_entree_t* entree;
            if(i%entreesParPage==0){
                p++;
                if(i>0){
                    lse_ecrire_format(travail,"PAGE SUIVANTE...\a");
                    lse_lire_ligne(travail,&ligne,1,mXOFF);
                }
                lse_ecrire_format(
                    travail,"\r\n"
                    "RUBANS ARCHIVES DANS L'ARMOIRE (%2d/%2d)\r\n"
                    "**************************************\r\n"
                    "\r\n"
                    "          NOM              DATE    TAILLE\r\n"
                    "\r\n",p,np);
            }
            entree=pjb_tableau_element_a(rubans->entrees,i);
            lse_ecrire_format(travail,"%-25s%02d/%02d/%02d%9d\r\n",
                              entree->chemin_relatif,
                              entree->jour,
                              entree->mois,
                              entree->annee,
                              entree->mots*2);
        }
        while(i%entreesParPage!=0){
            lse_ecrire_format(travail,"\n");
            i++;
        }
        lse_ecrire_format(travail,"QUEL RUBAN FAUT-IL LIRE ? \a");
        lse_lire_ligne(travail,&ligne,1,mXOFF);
        lse_ecrire_format(travail,"\r\n");
        if(!lse_nom_de_ruban_est_valide(&ligne)){
            lse_erreur_formater(travail,
                                "LE NOM DE RUBAN DONNE EST INVALIDE");
            return(0);
        }
        if(lse_catalogue_chercher_nom(rubans,ligne.caracteres)>=0){
            lse_es_rediriger_entree_sur_ruban(travail,&ligne);
            return(1);
        }else{
            lse_erreur_formater(travail,"RUBAN INEXISTANT");
            return(0);
        }        
    }/*lse_es_activer_lecteur_de_ruban*/


    void lse_es_arreter_lecteur_de_ruban(lse_travail_t* travail)
    {
        travail->entree=travail->entree_terminal;
    }/*lse_es_arreter_lecteur_de_ruban*/


     void lse_ecrire_format(lse_travail_t* travail,const char* format,...)
    {
        va_list ap;
        int taille;
        char tampon[4096];
        int ecrit;
        int reste;
        char* a_ecrire;
        int debut;
        int fin;

        va_start(ap,format);
        taille=vsprintf(tampon,format,ap);
        va_end(ap);
        
        if(taille>=4096){
            lse_paniquer("UN TAMPON A DEBORDE DANS LSE_ES ! J'ARRETE TOUT !");
        }
        
        debut=0;
        while(debut<taille){
            short encore=1;
            fin=debut;
            while((fin<taille)&&(encore)){
                switch(tampon[fin]){
                case 17:
                case 18:
                case 20:
                case 21:
                    encore=0;
                    break;
                default:
                    fin++;
                    break;
                }
            }

            reste=fin-debut;
            a_ecrire=tampon+debut;
    
            if((travail->dectech)&&(travail->sortie==travail->sortie_terminal)){
                int i;
                for(i=0;i<reste;i++){
                    if(a_ecrire[i]=='_'){ 
                        a_ecrire[i]=0xfb; 
                    }else if(a_ecrire[i]=='^'){ 
                        a_ecrire[i]=0xfc; 
                    }else if(islower(a_ecrire[i])){ 
                        a_ecrire[i]=toupper(a_ecrire[i]); 
                    }
                }
            }

            while(0<reste){
                struct timespec delai={0, 83333333/*41666666*/};
                ecrit=write(travail->sortie,a_ecrire,(unsigned)reste);
                if(ecrit>0){
                    a_ecrire+=ecrit;
                    reste-=ecrit;
                }
                nanosleep(&delai,0);
            }
            
            if(fin<taille){
                switch(tampon[fin]){
                case 17:
                    lse_es_activer_lecteur_de_ruban(travail);
                    break;
                case 18:
                    lse_es_activer_perforateur_de_ruban(travail);
                    break;
                case 20:
                    lse_es_arreter_perforateur_de_ruban(travail);
                    break;
                case 21:
                    lse_es_arreter_lecteur_de_ruban(travail);
                    break;
                }
            }
            debut=fin+1;
        }

    }/*lse_ecrire_format*/
||#

;; Le thread principal peut lire directement le ruban, 
;; mais pour le terminal, il doit prendre les caracteres dans tampon_entree,
;; et attendre sur le thread terminal.
;;
;; Le thread terminal lit chaque caractere venant du terminal et 
;; les aiguille :
;;     - interuption (ESC),
;;     - signal (CTRL-A),
;;     - tampon_entree lorsque le thread principal attend une entrée terminal,
;;     - /dev/null lorsque le thread principal n'attend pas d'entrée terminal,
;;     - echo sur le terminal lorsqu'on est pas en mode SILENCE.
;;     - fin de l'entrée et signal du thread principal sur terminateur.
;;
;; On va faire lire le ruban  par le thread terminal. Il fera un poll
;; (on pourrait avoir  plus de 32 descripteurs ouverts  en même temps
;; avec 16  consoles+rubans) sur  le terminal et  le ruban.  Quand le
;; ruban est lu,  on ignore toute entrée sur  le terminal sauf CTRL-A
;; et ESC.


#||
    short lse_lire_ligne(lse_travail_t* travail,lse_chaine_t* ligne,
                         int echo,int terminateurs)
    {
        ligne->longueur=0;
        pthread_mutex_lock(&(travail->mutex_tampon_entree));
        travail->tampon_entree=ligne;
        travail->terminateurs=terminateurs;
        travail->echo=echo;
        pthread_mutex_unlock(&(travail->mutex_tampon_entree));

        pthread_mutex_lock(&(travail->mutex_terminal));
        pthread_cond_wait(&(travail->condi_terminal),
                          &(travail->mutex_terminal));
        pthread_mutex_unlock(&(travail->mutex_terminal));
        
        pthread_mutex_lock(&(travail->mutex_tampon_entree));
        travail->tampon_entree=0;
        pthread_mutex_unlock(&(travail->mutex_tampon_entree));
        return(!travail->interruption);
    }/*lse_lire_ligne*/


    void lse_es_terminal(lse_travail_t* travail)
    {
        int i;
        struct termios term;

        tcgetattr(travail->entree_terminal,&term);
        /* cfmakeraw(&term); */
        term.c_iflag=IGNBRK|IGNPAR|ISTRIP;
        term.c_oflag=OLCUC|NL1|CR3|XTABS;
        term.c_cflag=CS7|CREAD;
        term.c_lflag=0;
        for(i=0;i<NCCS;i++){
            term.c_cc[0]=0;
        }
        term.c_cc[VMIN]=1;
        tcsetattr(travail->entree_terminal,TCSANOW,&term);

        /*
            stty raw  \
                olcuc -ocrnl -onlcr \
                -echo -echoe -echok -echonl -echoprt -echoke -echoctl \
                intr   0x00 \
                start  0x00 \
                stop   0x00 \
                susp   0x00 \
                quit   0x00 \
                erase  0x00 \
                kill   0x00 \
                eof    0x00 \
                eol    0x00 \
                eol2   0x00 \
                rprnt  0x00 \
                werase 0x00 \
                lnext  0x00 
        */

        while(lse_travail_etat(travail)!=lse_travail_etat_inlimbo){
            struct pollfd ufds[2];
            int  nfds=0;
            int           ifd;
            int           terminal_lu;
            int           ruban_lu;
            short         terminal_dispo=0;
            short         ruban_dispo=0;
            short         ruban_interruption=0;
            char terminal_car;
            char ruban_car;
            char entree_car;

            if(travail->entree_terminal>=0){
                ufds[nfds].fd=travail->entree_terminal;
                ufds[nfds].events=POLLIN|POLLPRI;
                ufds[nfds].revents=0;
                nfds++;
            }

            if((travail->entree==travail->entree_ruban)
               &&(travail->entree_ruban>=0)){
                ufds[nfds].fd=travail->entree_ruban;
                ufds[nfds].events=POLLIN|POLLPRI;
                ufds[nfds].revents=0;
                nfds++;
            }
            
            if(poll(ufds,(unsigned)nfds,10000)<=0){
                continue;
            }

            for(ifd=0;ifd<nfds;ifd++){
                if(ufds[ifd].fd==travail->entree_terminal){
                    if(((POLLIN|POLLPRI)&ufds[ifd].revents)!=0){
                        terminal_dispo=1;
                        terminal_lu=read(travail->entree_terminal,
                                         &terminal_car,1);
                    }else if(((POLLHUP|POLLERR)&ufds[ifd].revents)!=0){
                        lse_travail_deconnecter(travail);
                        return;
                    }
                }else if(ufds[ifd].fd==travail->entree_ruban){
                    if(((POLLIN|POLLPRI)&ufds[ifd].revents)!=0){
                        ruban_dispo=1;
                        ruban_lu=read(travail->entree_ruban,&ruban_car,1);
                    }else if(((POLLHUP|POLLERR)&ufds[ifd].revents)!=0){
                        ruban_dispo=-1;
                        ruban_lu=0;
                    }
                }
            }

            if(!(ruban_dispo||terminal_dispo)){
                continue;
            }

            pthread_mutex_lock(&(travail->mutex_tampon_entree));
              
            if(ruban_dispo){
                if(terminal_dispo){
                    terminal_car&=0x7f;
                    if(terminal_car==ESC){
                        ruban_interruption=1;
                    }else if(terminal_car==SOH){
                        travail->signal=1;
                    }
                }
                if(ruban_lu==0){
                    close(travail->entree_ruban);
                    travail->entree_ruban=-1;
                    travail->entree=travail->entree_terminal;
                    travail->silence=0; /* voir si ca vient de RU ou SI ? */
                    pthread_mutex_unlock(&(travail->mutex_tampon_entree));
                    continue;
                }else{
                    entree_car=ruban_car;
                }
            }else{
                entree_car=terminal_car;
            }
            entree_car&=0x7f;
            if(entree_car==SOH){
                travail->signal=1;
            }else if(ruban_interruption||(entree_car==ESC)){
                travail->silence=0;
                travail->interruption=1;
                lse_ecrire_format(travail,"\r\nPRET");
                if(travail->tampon_entree!=0){
                    travail->tampon_entree->longueur=0;
                }

                pthread_mutex_unlock(&(travail->mutex_tampon_entree));
                pthread_cond_signal(&(travail->condi_terminal));
                continue;
            }else if((entree_car<32)
                     &&((travail->terminateurs&(1<<entree_car))!=0)){
                if(travail->tampon_entree!=0){
                    if(entree_car!=XOFF){
                        if(!(travail->silence)){
                            lse_ecrire_format(travail,"\r\n"); 
                        }
                        if(travail->tampon_entree->longueur
                           <lse_chaine_maximum(travail->tampon_entree)){
                            travail->tampon_entree->caracteres[
                                travail->tampon_entree->longueur++]=entree_car;
                        }
                    }
                    travail->tampon_entree->caracteres[
                        travail->tampon_entree->longueur]='\0';
                    pthread_mutex_unlock(&(travail->mutex_tampon_entree));
                    pthread_cond_signal(&(travail->condi_terminal));
                    continue;
                }else{
                    entree_car=BEL;
                    write(travail->sortie,&entree_car,1);
                }
            }else if((32<=entree_car)&&(entree_car<DEL)){
                if(travail->tampon_entree!=0){
                    if(entree_car=='\\'){
                        if(travail->tampon_entree->longueur>0){
                            travail->tampon_entree->longueur--;
                        }
                    }else{
                        if(isalpha(entree_car)){
                            entree_car=toupper(entree_car);
                        }
                        if(travail->tampon_entree->longueur
                           <lse_chaine_maximum(travail->tampon_entree)){
                            travail->tampon_entree->caracteres[
                                travail->tampon_entree->longueur++]=entree_car;
                        }else{
                            entree_car=BEL;
                            write(travail->sortie,&entree_car,1);
                        }
                    }
                    if(travail->echo
                       &&isprint(entree_car)&&(!(travail->silence))){
                        lse_ecrire_format(travail,"%c",entree_car);
                    }
                }else{
                    entree_car=BEL;
                    write(travail->sortie,&entree_car,1);
                }
            }
                
            pthread_mutex_unlock(&(travail->mutex_tampon_entree));
        }/*while !inlimbo*/
    }/*lse_es_terminal*/



(ext:with-keyboard
 (loop 
  for ch = (read-char-no-hang ext:*keyboard-input*)
  until (equalp ch #S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\C))
  do (when ch (print ch))))


NIL or:
#S(SYSTEM::INPUT-CHARACTER :CHAR #\f :BITS 0 :FONT 0 :KEY NIL) 
#S(SYSTEM::INPUT-CHARACTER :CHAR #\s :BITS 0 :FONT 0 :KEY NIL) 
#S(SYSTEM::INPUT-CHARACTER :CHAR #\c :BITS 0 :FONT 0 :KEY NIL) 
#S(SYSTEM::INPUT-CHARACTER :CHAR #\s :BITS 0 :FONT 0 :KEY NIL) 
#S(SYSTEM::INPUT-CHARACTER :CHAR #\x :BITS 0 :FONT 0 :KEY NIL) 
#S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\S) 
#S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\S) 
#S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\C) 
#S(SYSTEM::INPUT-CHARACTER :CHAR #\q :BITS 0 :FONT 0 :KEY NIL) 
#S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\C) 
#S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\C) 
#S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\C) 
#S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\Q) 
#S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\Q) 
#S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\C) 
#S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\C) 
#S(SYSTEM::INPUT-CHARACTER :CHAR NIL :BITS 1 :FONT 0 :KEY #\Z) 
#S(SYSTEM::INPUT-CHARACTER :CHAR #\Escape :BITS 0 :FONT 0 :KEY NIL) 

||#



;;;; io.lisp                          --                     --          ;;;;
