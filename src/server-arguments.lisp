;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               server-arguments.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file processes command line arguments for the lse server.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-10 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
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
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.LSE.SERVER")


(defstruct options
  (input-reject-lowcase nil)
  (output-upcase        nil)
  (output-arrows        nil :type (member nil :dectech :unicode :unicode-halfwidth))
  (output-accented      t)
  (modern-mode          t)
  (return-is-xoff       nil))


(defvar *options* (make-options))


(defun apply-options (options task)
  (setf (task-case-insensitive task) (not (options-input-reject-lowcase options))
        (task-upcase-output    task) (options-output-upcase options)
        (task-accented-output  task) (options-output-accented options)
        (task-arrows           task) (options-output-arrows options))
  (let ((terminal (task-terminal task)))
    (when (typep terminal 'unix-terminal)
      (setf (terminal-modern-mode terminal) (or (member (getenv "TERM") '("emacs" "dumb")
                                                        :test (function string-equal))
                                                (options-modern-mode options))
            (terminal-cr-as-xoff terminal) (options-return-is-xoff options)))
    task))



(setf *documentation-text*
      "
Serveur système L.S.E.
Voir 'EMULSE.TXT' et 'SYSTEMES.TXT'.
")


(defchapter ("INTRODUCTION" "OPTIONS")
    #.(format nil "

~0@*~A est une commande unix interactive implémentant un système
L.S.E. avec son interpréteur.

~0@*~A accepte les options décrites dans ce chapitre.

Selon le compilateur Common Lisp utilisé pour compiler cette commande,
ces options peuvent devoir s'écrire après une option '--'.  (Les
options précédant '--' étant interprétées par l'implémentation Common
Lisp).

[AFAIRE] Établir la liste exacte des implémentations nécessitant '--'.


Ces options permettent de configurer le terminal, et les touches
utilisées.  Sur le systèmes L.S.E. MITRA-15 et T1600 des années 1970,
les terminaux était trés simples, et on tapait [CONTRÔLE-S] pour
envoyer les données saisies à l'ordinateur, la touche [ÉCHAPPEMENT]
pour interrompre un programme, la touche [\\] pour annuler le
caractère précédent, et la touche [CONTRÔLE-A] pour envoyer un signal
d'attention au programme.  La touche [ENTRÉE] pouvait être utilisée
pour saisir une chaîne, mais alors le code CARRIAGE RETURN était
ajouté en fin de chaîne.  Les consoles et télétypes n'étaient capable
d'afficher et d'encoder seulement des caractères majuscules, et les
accents étaient totalement inconnus.

Sur un terminal unix, on utilise [ENTRÉE] pour la saisie des données,
[EFFACEMENT] pour supprimer le caractère précédent, généralement,
[CONTRÔLE-C] pour interrompre un programme, et d'autres touches
configurées avec stty(1).  Les minuscules sont la norme.

On peut donc configurer ~0@*~A avec des options sur la ligne de
commande, ou interactivement, pour utiliser le mode ancien ou un mode
moderne.

[AFAIRE] établir un fichier de configuration pour éviter d'avoir à
refaire ces configurations à chaque fois.

~0@*~A tient compte des variables d'environnement suivantes:

LC\\_ALL, ou sinon LC\\_CTYPE, ou sinon LANG donnent l'encodage du
terminal.

TERM indiquent le type de terminal. 
"
              "lse"))



(defmacro defoption (names parameters &body body)
  (let ((docstring (when (stringp (first body))
                       (first body))))
   `(progn
      , (when docstring
          (loop
             :for sexp = `(defchapter (,(first names) "OPTIONS")
                             ,docstring)
             :for name :in (rest names)
             :do (setf sexp `(add-chapter ,name ,sexp))
             :finally (return sexp)))
        (define-option ,names ,parameters ,@body))))


;; (defoption ("aide" "--aide") ()
;;   "Affiche la liste des options."
;;   (call-option-function "help" ()))

(defoption ("--aide" "aide" "-h" "--help" "help") ()
  "Affiche la liste des options."
  (let ((options (option-list)))
    (format t "~2%Options de la commande ~A:~2%" (pname))
    (dolist (option (sort options (function string<)
                          :key (lambda (option) (first (option-keys option)))))
      (format t "    ~{~A~^ | ~}  ~:@(~{~A ~}~)~%~@[~{~%        ~A~}~]~2%"
              (option-keys option)
              (option-arguments option)
              (let ((lines (option-documentation option)))
                (when (zerop (length (first lines)))
                  (pop lines))
                lines)))
    (format t "~A~%" *documentation-text*)
    (parse-options-finish ex-ok)))



(defoption ("--version" "-V" "-v") ()
  "Affiche la version."
  (format t "
L.S.E.
VERSION ~A
~A

DISTRIBUE SELON LES TERMES DE LA LICENCE AGPLv3.

Ce programme est livré avec ABSOLUMENT AUCUNE GARANTIE; pour plus de
détails utilisez la commande DO GARANTIE.  Ce logiciel est libre, et
vous êtes les bienvenus pour redistribuer sous certaines conditions;
utilisez la commande DO LICENSE pour plus de détails.

"
          (version)
          *copyright*)
  (parse-options-finish ex-ok))


(defoption ("--fleches-ascii" "--ascii-arrows") ()
  "
Les caractères _ et ^ sont affichés tels quels.
"
  (setf (options-output-arrows *options*) nil))

(defoption ("--fleches-dectech" "--dectech-arrows") ()
  "
Le terminal est configuré avec une police de caractères DecTech.  Les
caractères _ et ^ sont alors mappés sur flêche vers la gauche et
flêche vers le haut.
"
  (setf (options-output-arrows *options*) :dectech))

(defoption ("--fleches-unicode" "--unicode-arrows") ()
  "
Le terminal est configuré avec une police de caractères Unicode
incorporant les flêches LEFTWARD_ARROW and UPWARD_ARROW (codes 8592 et
8593).  Les caractères _ et ^ sont alors mappés sur ces caractères.
"
  (setf (options-output-arrows *options*) :unicode))

(defoption ("--fleches-unicode-halfwidth" "--unicode-halfwidth-arrows") ()
  "
Le terminal est configuré avec une police de caractères Unicode
incorporant les flêches HALFWIDTH_LEFTWARD_ARROW and
HALFWIDTH_UPWARD_ARROW (codes 65513 et 65514).  Les caractères _ et ^
sont alors mappés sur ces caractères.
"
  (setf (options-output-arrows *options*) :unicode-halfwidth))


(defoption ("--entree-comme-xoff" "--return-is-xoff") ()
  "
Dans le mode ancien, traite la touche ENTRÉE comme la touche X-OFF.
"
  (setf (options-return-is-xoff *options*) t))


(defoption ("--rejeter-minuscules" "--reject-lowcase") ()
  "
Rejette tout caractère minuscule comme caractère invalide, ce qui
force l'utilisateur à ne saisir que des caractères majuscules.
"
  (setf (options-input-reject-lowcase *options*) t))


(defoption ("--accepter-minuscules" "--accept-lowcase") ()
  "
Accepte les caractères minuscules.  (Défaut).  Note: les mots clés et
identificateurs sont toujours mis en majuscules, mais les chaînes
peuvent contenir des minuscules.
"
  (setf (options-input-reject-lowcase *options*) nil))


(defoption ("--afficher-en-majuscules" "--upcase-output") ()
  "
Fait afficher tout en majuscules, comme sur les anciens terminaux.
"
  (setf (options-output-upcase *options*) t))


(defoption ("--affichage-mixte" "--mixed-output") ()
  "
Affiche en majuscules et minisucules.  (Défaut).
"
  (setf (options-output-upcase *options*) nil))


(defoption ("--afficher-sans-accent" "--no-accent-output") ()
  "
Si le terminal n'est pas capable d'afficher les accents, cette option
permet de convertir les lettres accentuees en lettres sans accent.
"
  (setf (options-output-accented *options*) nil))


(defoption ("--afficher-avec-accent" "--accented-output") ()
  "
Assume que le terminal est capable d'afficher les accents.  (Défaut).
"
  (setf (options-output-accented *options*) t))


(defoption ("--mode-moderne" "--modern-mode") ()
  "
Dans le mode moderne, les caractères et codes de contrôle configurés
par stty(1) sont utilisé (en général, [RETOUR] pour entrer une donnée,
[EFFACEMENT] pour effacer un caractère, [CONTRÔLE-C] pour interrompre,
etc).  (Défaut).
"
  (setf (options-modern-mode *options*) t))


(defoption ("--mode-ancien" "--old-mode") ()
  "
Dans le mode ancien, on utilise [CONTRÔLE-S] (X-OFF) pour entrer une
donnée,  [\\] pour effacer un caractère, et [ÉCHAPEMENT] pour
interrompre, entre autres.
"
  (setf (options-modern-mode *options*) nil))



(defun o-ou-n-p (&optional control &rest arguments)
  (loop
    (when control
      (format *query-io* "~? (O/N) ? " control arguments))
    (finish-output *query-io*)
    (let* ((input (string-left-trim " " (read-line *query-io*)))
           (rep   (subseq input 0 (min 1 (length input)))))
      (cond
        ((string-equal rep "O") (return t))
        ((string-equal rep "N") (return nil))
        (t (format *query-io* "REPONSE INVALIDE: ~S; TAPEZ 'O' OU 'N'.~%" rep))))))


(defun show-key-bindings (stream terminal)
  (format stream "
~@[~15A pour entrer les données.~%~]~
~@[~15A pour effacer le caractère précédent.~%~]~
~@[~15A pour interrompre.~%~]~
~@[~15A pour envoyer le signal d'attention (fonction ATT()).~%~]~
~@[~15A pour entrer les données, mais ajoute le code RETOUR aux chaînes.~%~]~
"
          (terminal-keysym-label terminal :xoff)
          (terminal-keysym-label terminal :delete)
          (terminal-keysym-label terminal :escape)
          (terminal-keysym-label terminal :attention)
          (terminal-keysym-label terminal :return)))


(defun configuration-interactive (options)
  (let* ((task *task*)
         (terminal (task-terminal task)))
    (terpri *query-io*)
    (when (o-ou-n-p "Voulez vous une configuration particuliere")
      (setf (options-input-reject-lowcase options)
            (not (o-ou-n-p "Est ce qu'il faut rejeter les minuscules")))
      (setf (options-output-upcase options)
            (o-ou-n-p "Est ce qu'il faut tout imprimer en majuscules"))
      (setf (options-output-accented options)
            (o-ou-n-p "Est ce que le terminal supporte les lettres accentuées"))
      (setf (options-output-arrows options)
            (if (task-unicode task)
                (cond
                  ((o-ou-n-p "Est ce que le terminal affiche les flêches unicode demi-largeur ~A et ~A"
                             *UNICODE-HALFWIDTH-LEFTWARDS-ARROW*
                             *UNICODE-HALFWIDTH-UPWARDS-ARROW*)
                   :unicode-halfwidth)
                  ((o-ou-n-p "Est ce que le terminal affiche les flêches unicode ~A et ~A"
                             *UNICODE-LEFTWARDS-ARROW*
                             *UNICODE-UPWARDS-ARROW*)
                   :unicode)
                  (t
                   nil))
                (if (o-ou-n-p "Est ce que le terminal utilise une police DecTech")
                    :dectech
                    nil)))
      (when (and (typep terminal 'unix-terminal)
                 (not (member (getenv "TERM") '("emacs" "dumb")
                              :test (function string-equal))))
        (format *query-io* "~%Choix du mode de saisie~%")
        (loop
          :for modern-mode :in '(nil t)
          :for title :in '("MITRA-15/T1600" "Moderne")
          :do (progn
               (setf (terminal-modern-mode terminal) modern-mode)
               (format *query-io* "~%Mode ~A:" title)
               (show-key-bindings *query-io* (task-terminal *task*))))
        (terpri *query-io*)
        (unless (setf (options-modern-mode options)
                      (o-ou-n-p "Faut-il utiliser le mode moderne"))
          (setf (options-return-is-xoff options)
                (o-ou-n-p "Faut-il traiter RETOUR comme X-OFF")))))
    (terpri *query-io*))
  options)


(defoption ("--configuration-interactive" "--interactive-configuration") ()
  "
Permet d'effectuer la saisie des options de ligne de commande de
manière interactive.
"
  (setf *options* (configuration-interactive *options*)))


(defoption ("--montrer-touches" "--show-bindings") ()
  "
Affiche les touches à utiliser.
"
  (apply-options *options* *task*)
  (show-key-bindings *standard-output* (task-terminal *task*)))


;;;; THE END ;;;;
