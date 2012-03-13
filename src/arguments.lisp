;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               arguments.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file processes command line arguments for the lse cli.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-10 <PJB> Created.
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
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.LSE.UNIX-CLI")

(defstruct options
  (input-reject-lowcase nil)
  (output-upcase        nil)
  (output-dectech       nil)
  (output-accented      t)
  (modern-mode          t)
  (return-is-xoff       nil))


(defvar *options* (make-options))


(defun apply-options (options task)
  (setf (task-case-insensitive task) (not (options-input-reject-lowcase options))
        (task-upcase-output    task) (options-output-upcase options)
        (task-accented-output  task) (options-output-accented options)
        (task-dectech          task) (options-output-dectech options))
  (let ((terminal (task-terminal task)))
    (when (typep terminal 'unix-terminal)
      (setf (terminal-modern-mode terminal) (or (member (getenv "TERM") '("emacs" "dumb")
                                                        :test (function string-equal))
                                                (options-modern-mode options))
            (terminal-cr-as-xoff terminal) (options-return-is-xoff options)))
    task))



(setf *documentation-text*
      "
Système et langage de programmation L.S.E.
Voir 'EMULSE.TXT' et 'SYSTEMES.TXT'.
")


;; (define-option ("aide" "--aide") ()
;;   "Donne cette aide."
;;   (call-option-function "help" ()))

(define-option ("aide" "--aide" "help" "-h" "--help") ()
  "Donne cette aide."
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


(define-option ("--dectech-font") ()
  "
Le terminal est configuré avec une police de caractères DecTech.  Les
caractères _ et ^ sont alors mappés sur flêche vers la gauche et
flêche vers le haut.
"
  (setf (options-output-dectech *options*) t))


(define-option ("--rejeter-minuscules" "--reject-lowcase") ()
  "
Rejette tout caractère minuscule comme caractère invalide, ce qui
force l'utilisateur à ne saisir que des caractères majuscules.
"
  (setf (options-input-reject-lowcase *options*) t))


(define-option ("--accepter-minuscules" "--accept-lowcase") ()
  "
Accepte les caractères minuscules.  (Défaut).  Note: les mots clés et
identificateurs sont toujours mis en majuscules, mais les chaînes
peuvent contenir des minuscules.
"
  (setf (options-input-reject-lowcase *options*) nil))


(define-option ("--afficher-en-majuscules" "--upcase-output") ()
  "
Fait afficher tout en majuscules, comme sur les anciens terminaux.
"
  (setf (options-output-upcase *options*) t))


(define-option ("--affichage-mixte" "--mixed-output") ()
  "
Affiche en majuscules et minisucules.  (Défaut).
"
  (setf (options-output-upcase *options*) nil))


(define-option ("--afficher-sans-accent" "--no-accent-output") ()
  "
si le terminal n'est pas capable d'afficher les accents, cette option
permet de convertir les lettres accentuees en lettres sans accent.
"
  (setf (options-output-accented *options*) nil))


(define-option ("--afficher-avec-accent" "--accented-output") ()
  "
Assume que le terminal est capable d'afficher les accents.  (Défaut).
"
  (setf (options-output-accented *options*) t))


(define-option ("--mode-moderne" "--modern-mode") ()
  "
Dans le mode moderne, les caractères et codes de contrôle configurés
par stty(1) sont utilisé (en général, [RETOUR] pour entrer une donnée,
[EFFACEMENT] pour effacer un caractère, [CONTRÔLE-C] pour interrompre,
etc).  (Défaut).
"
  (setf (options-modern-mode *options*) t))


(define-option ("--mode-ancien" "--old-mode") ()
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
          (terminal-key terminal :return)))


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
      (setf (options-output-dectech options)
            (o-ou-n-p "Est ce que le terminal utilise une police DecTech"))
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


(define-option ("--configuration-interactive" "--interactive-configuration") ()
  "
Permet d'effectuer la saisie des options de ligne de commande de
manière interactive.
"
  (setf *options* (configuration-interactive *options*)))


(define-option ("--montrer-touches" "--show-bindings") ()
  "
Affiche les touches à utiliser.
"
  (apply-options *options* *task*)
  (show-key-bindings *standard-output* (task-terminal *task*)))


;;;; THE END ;;;;
