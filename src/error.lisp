;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               error.lisp
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
;;;;    This is the error management module.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2004-08-03 <PJB> Converted to Common-Lisp.
;;;;    2000-12-09 <PJB> Added this header comment.
;;;;BUGS
;;;;    Needs some more work to convert error handling to lisp condition system.
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

(define-condition lse-error (simple-error)
  ((code :type symbol :initarg :code)))


(defparameter +code-message-assoc+
  '(
    ( :division-par-zero
      "DIVISION PAR ZERO" )
    ( :racine-imaginaire
      "RACINE IMAGINAIRE" )
    ( :chaine-trop-courte
      "CHAINE TROP COURTE" )
    ( :chaine-trop-longue
      "CHAINE TROP LONGUE" )
    ( :argument-invalide
      "ARGUMENT INVALIDE" )
    ( :valeur-non-definie
      "VALEUR NON DEFINIE" )
    ( :type-non-booleen
      "LES OPERATEURS BOOLEENS ATTENDENT DES ARGUMENTS BOOLEENNES" )
    ( :types-incompatibles
      "LES TYPES DES OPERANDES SONT INCOMPATIBLES" )
    ( :valeurs-incomparables
      "LES VALEURS SONT INCOMPARABLES" )
    ( :conversion-impossible
      "CONVERSION IMPOSSIBLE" )
    ( :mauvais-identifiant
      "MAUVAIS IDENTIFIANT" )
    ( :espace-temporaire-desire-invalide
      "ESPACE TEMPORAIRE DESIRE INVALIDE" )
    ( :nom-de-fichier-invalide
      "NOM DE FICHIER INVALIDE" )
    ( :nom-de-fichier-deja-pris
      "NOM DE FICHIER DEJA PRIS" )
    ( :nom-de-fichier-attendu
      "NOM DE FICHIER ATTENDU" )
    ( :nom-de-programme-attendu
      "NOM DE PROGRAMME ATTENDU" )
    ( :fichier-inaccessible
      "FICHIER INACCESSIBLE" )
    ( :fichier-inexistant
      "FICHIER INEXISTANT" )
    ( :fichier-ecriture-impossible
      "FICHIER ECRITURE IMPOSSIBLE" )
    ( :fichier-access-impossible
      "FICHIER ACCESS IMPOSSIBLE" )
    ( :enregistrement-inexistant
      "ENREGISTREMENT INEXISTANT" )
    ( :enregistrement-invalide
      "ENREGISTREMENT INVALIDE" )
    ( :deux-noms-de-fichier-attendus
      "DEUX NOMS DE FICHIER ATTENDUS" )
    ( :cataloguage-fichier
      "CATALOGUAGE FICHIER" )
    ( :suppression-fichier
      "SUPPRESSION FICHIER" )    
    ( :type-de-fichier-attendu
      "TYPE DE FICHIER ATTENDU" )
    ( :type-de-fichier-invalide
      "TYPE DE FICHIER INVALIDE" )
    ( :impossible-pas-proprietaire
      "IMPOSSIBLE PAS PROPRIETAIRE" )
    ( :non-autorise
      "NON AUTORISE" )
    ( :numero-de-ligne-invalide
      "NUMERO DE LIGNE INVALIDE" )
    ( :numero-de-ligne-attendu
      "NUMERO DE LIGNE ATTENDU" )
    ( :ligne-inexistante
      "LIGNE INEXISTANTE" )
    ( :plus-d-instruction
      "PLUS D'INSTRUCTION" )
    ( :virgule-attendue
      "VIRGULE ATTENDUE" )
    ( :virgule-ou-a-attendus
      "VIRGULE OU A ATTENDUS" )
    ( :rien-de-plus-attendu
      "RIEN DE PLUS ATTENDU" )
    ( :numero-attendu
      "NUMERO ATTENDU" )
    ( :numero-d-enregistrement-invalide
      "NUMERO D'ENREGISTREMENT INVALIDE" )
    ( :plus-de-place
      "PLUS DE PLACE" )
    ( :entree-sortie-materiel
      "ENTREE SORTIE MATERIEL" )
    ( :entree-sortie-generique
      "ENTREE SORTIE GENERIQUE" )
    ( :entree-sortie-impossible
      "ENTREE SORTIE IMPOSSIBLE" )
    ( :donnee-trop-grande
      "DONNEE TROP GRANDE" )
    ( :pas-implemente
      "PAS IMPLEMENTE" )
    ( :instruction-inconnue
      "INSTRUCTION INCONNUE" )
    ( :cons-es-impossible
      "INTERNE - E/S D'UN CONS IMPOSSIBLE" )
    ))


(defun error-signal (code &rest args)
  (signal (apply (function make-condition) 'lse-error :code code args)))


(defun error-report (task code)
  (when code
    (let ((entry (assoc code +code-message-assoc+)))
      (if entry
        (error-format task "~S" code)
        (error-format task "~A" (second entry))))))


(defun error-format (task format-str &rest args)
  (io-standard-redirection task)
  (io-new-line task)
  (do* ((+line-length+ 72)
        (+left-margin+ 4)
        (message (apply (function format) nil format-str args))
        (column
         (if (and (task-environment task)
                  (< environment-next-action
                     (environment-current-line (task-environment task))))
           (prog1 22
             (io-format task "ERREUR EN LIGNE ~3D : "
                        (environment-current-line (task-environment task))))
           (prog1 9
             (io-format task "ERREUR : ")))
         0)
        (i 0 (1+ pos))
        (pos (- (+ i +line-length+) column)  (+ i +line-length+)))
      ((>= pos (length message))
       (io-format task "~VA~A"
                  (if (> column +left-margin+) 0 +left-margin+) ""
                  (subseq message i)))
    (setf pos (do ((pos pos (1- pos)))
                  ((or (= (char message pos) (character " ")) (> i pos))
                   pos)))
    (when (= i pos)
      (setf pos (- (+ i +line-length+) column)))
    (io-format task "~VA~A"
               (if (> column +left-margin+) 0 +left-margin+) ""
               (subseq message i pos))
    (io-new-line task))
  (io-finish-output task)
  (values))


(defun error-panic (format-str &rest args)
  (let ((message (format nil "~APANIQUE : ~A~A~A"
                         (io-new-line-string)
                         (apply (function format) nil format-str args)
                         (io-new-line-string)
                         (io-new-line-string))))
    ;; Trouver tous les travail, et envoyer le message 
    ;; sur tous les terminal-sortie 
    (format *error-output* "~A" message)
    (dolist (task *tasks*)
      (when (task-state-awake-p task)
        (io-standard-redirect task)
        (io-format task "~A" message)))
    (lse-terminate)
    (lse-exit 13)))


;;;; THE END ;;;;

