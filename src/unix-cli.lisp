;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               unix-cli.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Unix Command Line Interface.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-15 <PJB> Created
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

(defparameter *tape-banner* "
------------------------------------------------------------------------ 
\\     ooo                oooooooooo       oooooooooo                    \\      
 \\    ooo               oooooooooooo      oooooooooo      ooooooooooo    \\
  \\   ooo               oooo     ooo      ooo                             \\
   \\  ooo                oooo             oooooo               o oo        \\
    > ooo                  oooo           oooooo          ooo o    o        >
   /......................................................................./        
  /   ooo              ooo     oooo       ooo             oooo oo oo      /         
 /    oooooooooo  ooo  oooooooooooo  ooo  oooooooooo  ooo oooo  oo oo    / 
/     oooooooooo  ooo   oooooooooo   ooo  oooooooooo  ooo  o oooo oo    / 
------------------------------------------------------------------------ 
")

(defparameter *unix-banner* "

L.S.E.
VERSION ~A-UNIX
COPYRIGHT 1984 - 2012 PASCAL BOURGUIGNON

DISTRIBUE SELON LES TERMES DE LA LICENCE AGPLv3.

Ce programme est livré avec ABSOLUMENT AUCUNE GARANTIE; pour plus de
détails utilisez la commande DO GARANTIE.  Ce logiciel est libre, et
vous êtes les bienvenus pour redistribuer sous certaines conditions;
utilisez la commande DO COPIE pour plus de détails.

Tapez AI pour avoir de l'aide.


BONJOUR     ~8A

")





(defun locale-terminal-encoding ()
  "Returns the terminal encoding specified by the locale(7)."
  (dolist (var '("LC_ALL" "LC_CTYPE" "LANG")
               :iso-8859-1) ; some random default…
    (let* ((val (getenv var))
           (dot (position #\. val))
           (at  (position #\@ val :start (or dot (length val)))))
      (when (and dot (< dot (1- (length val))))
        (return (intern (string-upcase (subseq val (1+ dot) (or at (length val))))
                        "KEYWORD"))))))


(defun set-terminal-encoding (encoding)
  #-(and ccl (not swank)) (declare (ignore encoding))
  #+(and ccl (not swank))
  (mapc (lambda (stream)
          (setf (ccl::stream-external-format stream)
                (ccl:make-external-format :domain nil
                                          :character-encoding encoding
                                          ;; :line-termination line-termination
                                          )))
        (list (two-way-stream-input-stream  *terminal-io*)
              (two-way-stream-output-stream *terminal-io*)))
  (values))


;; (setf ccl:*default-external-format*           :unix
;;       ccl:*default-file-character-encoding*   :utf-8
;;       ccl:*default-line-termination*          :unix
;;       ccl:*default-socket-character-encoding* :utf-8)



(defgeneric stream-input-stream (stream)
  (:method ((stream stream))
    stream)
  (:method ((stream concatenated-stream))
    (stream-input-stream (first (concatenated-stream-streams stream))))
  (:method ((stream echo-stream))
    (stream-input-stream (echo-stream-input-stream stream)))
  (:method ((stream synonym-stream))
    (stream-input-stream (symbol-value (synonym-stream-symbol stream))))
  (:method ((stream two-way-stream))
    (stream-input-stream (two-way-stream-input-stream stream))))

(defgeneric stream-output-stream (stream)
  (:method ((stream stream))
    stream)
  (:method ((stream broadcast-stream))
    (stream-output-stream (first (broadcast-stream-streams stream))))
  (:method ((stream echo-stream))
    (stream-input-stream (echo-stream-output-stream stream)))
  (:method ((stream synonym-stream))
    (stream-input-stream (symbol-value (synonym-stream-symbol stream))))
  (:method ((stream two-way-stream))
    (stream-input-stream (two-way-stream-output-stream stream))))


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


(defun configuration-interactive (task terminal terminal-class)
  (terpri)
  (when (o-ou-n-p "VOULEZ VOUS UNE CONFIGURATION PARTICULIERE")
    (setf (task-case-insensitive task)
          (not (o-ou-n-p "EST CE QU'IL FAUT REJETER LES MINUSCULES")))
    (setf (task-upcase-output task)
          (o-ou-n-p "EST CE QU'IL FAUT TOUT IMPRIMER EN MAJUSCULES"))
    (setf (task-accented-output task)
          (o-ou-n-p "EST CE QUE LE TERMINAL SUPPORTE LES LETTRES ACCENTUÉES"))
    (when (and (eq terminal-class 'unix-terminal)
               (not (member (getenv "TERM") '("emacs" "dumb"))))
      (format *query-io* "~%CHOIX DU MODE DE SAISIE~%")
      (format *query-io* "
MODE MITRA-15:
[CONTROL-S]      POUR ENTRER LES DONNEES.
[\\]              POUR 'EFFACER' LE CARACTERE PRECEDENT.
[ESCAPE]         POUR INTERROMPRE.
[CONTROL-A]      POUR ENVOYER LE SIGNAL D'ATTENTION (FONCTION ATT()).
[ENTREE]         POUR ENTRER LES DONNEES, MAIS AJOUTE LE CODE CR AUX CHAINES.
")
      (format *query-io* "
MODE MODERNE:
~@[~14A POUR ENTRER LES DONNEES.~]
~@[~14A POUR EFFACER LE CARACTERE PRECEDENT.~]
~@[~14A POUR INTERROMPRE.~]
~@[~14A POUR ENVOYER LE SIGNAL D'ATTENTION (FONCTION ATT()).~]
"
              (terminal-key terminal :xoff)
              (terminal-key terminal :delete)
              (terminal-key terminal :escape)
              (terminal-key terminal :attention))
      (terpri)
      (unless (setf (terminal-modern-mode terminal)
                    (o-ou-n-p "FAUT-IL UTILISER LE MODE MODERNE"))
        (setf (terminal-cr-as-xoff terminal)
              (o-ou-n-p "FAUT-IL TRAITER RETOUR COMME X-OFF")))))
  (terpri))

(defun main (&optional args)
  (declare (ignore args))
  (let ((encoding (locale-terminal-encoding)))
    (set-terminal-encoding encoding)
    (let* ((terminal-class (progn
                             #+swank
                             (cond
                               ((typep (stream-output-stream *terminal-io*)
                                       'swank-backend::slime-output-stream)
                                'swank-terminal)
                               #+unix
                               ((member (getenv "TERM") '("emacs" "dumb")
                                        :test (function string=))
                                'standard-terminal)
                               (t
                                'unix-terminal))
                             #+(and (not swank) unix)
                             (cond
                               ((member (getenv "TERM") '("emacs" "dumb")
                                        :test (function string=))
                                'standard-terminal)
                               (t
                                'unix-terminal))
                             #+(and (not swank) (not unix))
                             'standard-terminal))
           (terminal (make-instance terminal-class
                            :input-stream  (stream-input-stream  *terminal-io*)
                            :output-stream (stream-output-stream *terminal-io*)))
           (task     (make-instance 'task
                         :state :active
                         :case-insensitive t
                         :upcase-output nil
                         :dectech nil
                         :unicode #+swank (eql encoding :utf-8) #-swank nil
                         :terminal terminal)))
      (configuration-interactive task terminal terminal-class)
      (setf *task* task)
      (terminal-initialize terminal)
      (unwind-protect
           (progn
             (io-format *task* "~A" *tape-banner*)
             (io-format *task* "~?" *unix-banner*  (list *version* (subseq (dat) 9)))
             (command-repl *task*))
        (task-close-all-files *task*)
        (terminal-finalize terminal))))
  0)


;;;; THE END ;;;;
