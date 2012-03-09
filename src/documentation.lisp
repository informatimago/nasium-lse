;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               documentation.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines documentation tools.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-08 <PJB> Extracted from commands.lisp
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

(in-package "COM.INFORMATIMAGO.LSE")


(defstruct chapter
  title
  category
  oneliner
  text)


(defparameter *chapters* (make-hash-table :test (function equalp)))


(defmacro defchapter (title-category-oneliner &body text)
  (let ((title    (if (listp title-category-oneliner)
                      (first title-category-oneliner)
                      title-category-oneliner))
        (category (if (listp title-category-oneliner)
                      (second title-category-oneliner)
                      nil))
        (oneliner (if (listp title-category-oneliner)
                      (third title-category-oneliner)
                      nil)))
    `(setf (gethash ',title *chapters*)
           (make-chapter :title ',title
                         :category ',category
                         :oneliner ',oneliner
                         :text ,(if (every 'stringp text)
                                    (unsplit-string text #\Newline)
                                    `(lambda (chapter)
                                       (declare (ignorable chapter))
                                       (block chapter ,@text)))))))

(defmacro definstruction (title-category-oneliner &body text)
  (let ((oneliner (if (listp title-category-oneliner)
                      (third title-category-oneliner)
                      nil)))
    `(defchapter ,title-category-oneliner
         ,(when oneliner `(io-format *task* "~A~%" ',oneliner))
       ,@(mapcar (lambda (item) `(write-documentation *task* ,item)) text))))


(defun find-chapter (title)
  (gethash title *chapters*))


(defun find-category (category)
  (let ((result '()))
   (maphash (lambda (title chapter)
              (declare (ignore title))
              (when (string-equal category (chapter-category chapter))
                (push chapter result)))
            *chapters*)
   (sort result (function string-lessp) :key (function chapter-title))))




(defparameter *character-foldings*
  '( ("A" "ÀÁÂÃÄÅ") ("AE" "Æ") ("C" "Ç") ("E" "ÈÉÊË") ("I" "ÌÍÎÏ") 
    ("ETH" "Ð") ("N" "Ñ") ("O" "ÒÓÔÕÖØ") ("U" "ÙÚÛÜ") ("Y" "Ý")
    ("TH" "Þ") ("ss" "ß") ("a" "àáâãäå") ("ae" "æ") ("c" "ç")
    ("e" "èéêë") ("i" "ìíîï") ("eth" "ð") ("n" "ñ") ("o" "òóôõöø")
    ("u" "ùúûü") ("u" "ýÿ") ("th" "þ")))

(defparameter *accented-letters*
  (with-output-to-string (out)
    (dolist (cf *character-foldings*)
      (princ (second cf) out))))

(defun accented-letter-p (ch)
  (find ch *accented-letters*))

(defun character-folding (character)
  (car (member (character character) *character-foldings* 
               :test (function position) :key (function second))))

(defun character-fold (character)
  "
RETURN: A string containing the character without accent 
        (for accented characters), or a pure ASCII form of the character.
"
  (car (character-folding character)))

(defun remove-accents (string)
  (if (find-if (function accented-letter-p) string)
      (with-output-to-string (out)
        (loop
          :for ch :across string
          :do (let ((conv (character-folding ch)))
                (princ (if conv
                           (first conv)
                           ch)
                       out))))
      string))


(defparameter *key-labels*
 '(("[ESC]"     . :escape)
   ("[CTRL-A]"  . :attention)
   ("[XOFF]"    . :xoff)
   ("[DEL]"     . :delete)))


(defun process-doc (docstring)
  (let ((terminal (task-terminal *task*)))
    (funcall (if (task-upcase-output *task*)
                 (function string-upcase)
                 (function identity))
             (funcall (if (task-accented-output *task*)
                          (function identity)
                          (function remove-accents))
                      (if (find #\[ docstring)
                          (with-output-to-string (out)
                            (loop
                              :with end = (length docstring)
                              :with start = 0
                              :with sstart = 0
                              :while (< sstart end)
                              :do (let* ((lcro (position #\[ docstring :start sstart))
                                         (rcro (and lcro
                                                    (position #\] docstring :start (1+ lcro))))
                                         (key  (and rcro
                                                    (assoc (subseq docstring lcro (1+ rcro))
                                                           *key-labels* :test (function string=)))))
                                    (if key
                                        (progn
                                          (princ (subseq docstring start lcro)     out)
                                          (princ (terminal-key terminal (cdr key)) out)
                                          (setf sstart (setf start (1+ rcro))))
                                        (setf sstart (if lcro (1+ lcro) end))))
                              :finally (princ (subseq docstring start end) out)))
                          docstring)))))


(defun split-text (text)
  (with-input-from-string (inp text)
    (loop
      :with text = '()
      :with para = '()
      :for line = (read-line inp nil nil)
      :while line
      :do (if (zerop (length (string-trim " " line)))
              (progn (push (nreverse para) text)
                     (setf para '()))
              (setf para (nreconc (split-sequence #\space line :REMOVE-EMPTY-SUBSEQS t)
                                  para)))
      :finally (push (nreverse para) text) (return (nreverse text)))))


(defun justify-text (text first-left-margin left-margin right-margin)
  (with-output-to-string (out)
    (loop
      :with margin1 = (make-string left-margin       :initial-element #\space)
      :with margin2 = (make-string (+ 2 left-margin) :initial-element #\space)
      :with para-margin = "" 
      :for column = first-left-margin :then left-margin
      :for para :in text
      :do (progn
            (princ para-margin out)
            (setf para-margin (if (string= "-" (first para))
                                  margin2
                                  margin1))
            (dolist (word para)
              (let* ((spaces (if (find (aref word (1- (length word))) ".!?")
                                 "  " " ")))
                (when (< right-margin (+ column (length word)))
                  (terpri out)
                  (princ para-margin out)
                  (setf column (length para-margin)))
                (princ word out)
                (when (<= (+ column (length word) (length spaces)) right-margin)
                  (princ spaces out))
                (incf column (+ (length word) (length spaces)))))
            (terpri out)
            (terpri out)))))


(defun write-documentation (task text)
  (io-format task "~A~%"
             (justify-text (split-text (process-doc text))
                           0 0
                           (terminal-columns (task-terminal task)))))


(defun first-line (text)
  (subseq text 0 (position #\Newline text)))



(defchapter ("GARANTIE" "LEGAL")
    "
Système L.S.E

Copyright (C) 2012 Pascal Bourguignon

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
")


(defchapter ("COPIE" "LEGAL")
    (io-format *task* "~A" *license*))


(defchapter ("SOURCES" "LEGAL")
    "
Les sources de ce programme sont disponibles sous license AGPLv3
à l'adresse suivante:

http://www.ogamita.com/lse/
")



(definstruction ("AFFECTATION" "INSTRUCTIONS" "ref _ expression")
    "
L'instruction d'affectation permet de changer la valeur d'une variable
ou d'un élément de tableau.  ref doit être un identificateur de
variable: une lettre suivie d'au plus quatre lettres ou chiffres, ou
une référence de tableau: un identificateur de tableau, suivi d'un
crochet ouvrant, d'une ou deux expressions index séparées par une
virgule, et un crochet fermant.  L'identificateur ne peut pas être un
mot réservé du langage L.S.E.  Examples:

Exemples: I  PRNOM  C12  V[2]  V[I+1]  M[1,3]  M[I,J] 

L'expression est une expression arithmétique ou une expression chaîne.

Si ref est une référence de tableau, alors il doit avoir été déclaré
au préalable par une instruction TABLEAU.

Si l'expression est une expression chaîne, alors la variable ref doit
avoir été déclarée au préalable par une instruction CHAINE.

Note: _ s'affiche comme une flêche vers la gauche sur un terminal
Unicode, ou comme un souligné sur un terminal ASCII.  Il se tape comme
un souligné, touche 8 sur un clavier AZERTY, touche shift-moins sur un
clavier QWERTY.

Voir: TABLEAU, CHAINE, FONCTIONS")



(definstruction ("CHAINE" "INSTRUCTIONS" "CHAINE id1,...,idn")
    "
L'instruction CHAINE permet de déclarer une ou plusieurs variable
chaines.  Ces variables ne doivent pas avoir été déclarées comme
tableau ou affecté une valeur numérique au préalable (voir instruction
LIBERER).  Une fois déclarée comme chaîne, on peut lui affecter des
valeurs chaîne de caractère.

Exemple:

1 CHAINE CH,CH2;CH_'BON';CH2_'JOUR';AFFICHER CH!CH2

2 TERMINER

Voir: LIBERER, TABLEAU, FONCTIONS")



(definstruction ("TABLEAU" "INSTRUCTIONS" "TABLEAU tab1,...,tabn")
    "
L'instruction TABLEAU permet de déclarer une ou plusieurs variable
tableau. Chaque tabi doit être de la forme: IDENT[EXPR] pour un
vecteur (tableau de dimension 1) ou IDENT[EXPR1,EXPR2] pour une
matrice (tableau de dimension 2).

Ces variables ne doivent pas avoir été déclarées comme chaîne, ou
affecté une valeur numérique au préalable (voir instruction LIBERER).
Une fois déclarée comme tableau, on peut lui affecter des valeurs
numérique à ses éléments.

Exemple:

1 N_3;M_4;TABLEAU V[3],MAT[N,M];V[1]_1;MAT[2,2]_2;AFFICHER V[1]+MAT[2,2]

2 TERMINER

Voir: LIBERER, CHAINE, FONCTIONS")



(definstruction ("LIBERER" "INSTRUCTIONS" "LIBERER id1,...,idn")
    "

L'instruction LIBERER permet de libérer une variable ou plusieurs
variables, c'est à dire, d'oublier leurs valeurs et leurs types chaîne
ou tableau.  Les variables libérées peuvent ainsi être réutilisée
(affectation d'une valeur numérique, ou déclaration par CHAINE ou
TABLEAU).

Exemple:

1 CHAINE C;C_'BONJOUR';AFFICHER C

2 LIBERER C;C_42;AFFICHER C

4 TERMINER

Voir: AFFECTATION, CHAINE, TABLEAU")



(definstruction ("APPEL" "INSTRUCTIONS"
                     "&PROID(ARGUMENTS,...)")
    "

&PROID est un identificateur de procédure sous-programme, il est
composé du caractère &, suivi d'une lettre, suivi d'au plus quatre
lettres ou chiffres.

Les ARGUMENTS sont des expressions pour les paramètres passés par
valeur, ou des références (variables ou référence tableau) pour les
paramètres passé par référence.

Voir: PROCEDURE")


(definstruction ("LIRE" "INSTRUCTIONS" "LIRE ref1,...,refn")
    "
L'instruction LIRE permet d'affecter aux variables ou éléments de
tableau référencés une valeur tapée au clavier.  L'instruction LIRE
émet un siflement, puis attend que l'utilisateur saisisse les valeurs
séparées par des espaces et/ou terminées par [XOFF].  Les chaînes
doivent être terminées par [XOFF].

Si une variable tableau est lue, l'utilisateur doit saisir une valeur
pour chaque élément du tableau.

VOIR: AFFECTATION")



(definstruction ("AFFICHER" "INSTRUCTIONS" "AFFICHER[format]expr1,...,expr2")
    "
L'instruction AFFICHER permet de faire afficher sur le terminal, des
données.  Le format entre crochet est optionel.  Sans format, la
valeur de chaque expression est affichée selon le format U
(universel).  Le format permet de spécifier plus précisément comment
les valeurs doivent être formatées pour l'affichage.

En mode machine de bureau, AFFICHER peut aussi s'écrire ?.

Exemples:

? 2+3

1 AFFICHER 'BONJOUR'

2 AFFICHER[/,'X=',F5.2,3X,'N=',U,/]12.34,'BONJOUR'

Voir: FORMAT")


(definstruction ("FORMAT" "INSTRUCTIONS" "AFFICHER[format]expr1,...,expr2")
    "
Le format est une liste entre crochets de spécificateur de format
séparés par des virgules.  Chaque spécificateur peut être précédé d'un
facteur de répétition, qui peut être soit un nombre, soit une étoile
*.  Lorsque c'est une étoile, une valeur dans la liste d'expressions
est prise comme facteur de répétition variable.

Les spécificateurs sont:

'LITERAL' : la chaîne litérale est affichée (répétée s'il y a un
facteur de répétition).

U : une valeur prise dans les expressions est affichée selon un format
universel.

X : un espace est affiché.

L : provoque le passage à la ligne suivante sans retour au début de la ligne.

/ : provoque le passage à la ligne suivante, avec retour au début de la ligne.

C : provoque le retour au début de la ligne, sans passage à la ligne suivante.

Fe.d : affiche un nombre avec e chiffres avant le point décimal, et d
chiffres après le point décimal.

Ee.d : affiche un nombre en notation scientifique avec e chiffres
avant le point décimal, et d chiffres après le point décimal.

e et d sont compris entre 0 et 100.

Voir: AFFICHER")



(definstruction ("ALLEREN" "INSTRUCTIONS" "ALLER EN ligne")
    "
L'instruction ALLER EN permet de poursuivre l'exécution à la ligne
dont le numéro est indiqué.

Note: La ligne peut aussi être une expression arithmétique utilisant @
qui représente la ligne courante.")


(definstruction ("ALLEREN" "INSTRUCTIONS" "ALLER EN ligne")
    "
L'instruction ALLER EN permet de poursuivre l'exécution à la ligne
dont le numéro est indiqué.")


(definstruction ("INSTRUCTIONSI" "INSTRUCTIONS" "SI condition ALORS instruction1 [SINON instruction2]")
    "
L'instruction SI permet de faire un choix entre deux instructions.  Si
la valeur de la condition est vrai, alors instruction1 est exécutée,
sinon c'est instruction2 qui est exécutée.

On peut grouper plusieurs instructions en une seule grâce à
l'instruction DEBUT ... FIN.

Voir: DEBUT")


(definstruction ("DEBUT" "INSTRUCTIONS" "DEBUT instruction1;...;instructionN FIN")
    "
L'instruction DEBUT ... FIN permet de regrouper plusieurs instructions
pour qu'elles soient considérée comme une seule.

Voir: SI")


(definstruction ("FAIREJUSQUA" "INSTRUCTIONS" "FAIRE ligne POUR var _ expri [PAS exprp] JUSQUA exprf")
"
L'instruction FAIRE JUSQUA permet d'exécuter une boucle itérative.

ligne est le numéro n de la dernière ligne de la partie de programme à
répéter.

var est le nom de la variable contrôlée; ce doit être une variable
arithmétique simple.

expri est une expression qui donne la valeur initiale vi affectée à var.

exprp est une expression qui donne le pas vp, c'est à dire l'incrément
ajouté à var à chaque itération.  Par défaut c'est 1.

exprf est une expression qui donne la valeur finale vf.

La première fois qu'au cours de l'exécution on arrive à l'instruction
FAIRE, la valeur vi est affectée à la variable contrôlée var.  La
valeur de var est testée.

- si var > vf (avec vp > 0) ou var < vf (avec vp < 0) alors on sort de
  la bouche et on va exécuter la première ligne qui suit la ligne de
  numéro n.

- sinon on exécute une fois la boucle ; le pas vp est ensuite ajouté à
  var et on compare var à vf comme précédement.  Ceci conduit de
  nouveau soit à sortir de la boucle soit à la réexécution.


Voir: FAIRETANTQUE"  #+lse-extensions ", XIT")



(definstruction ("FAIRETANTQUE" "INSTRUCTIONS" "FAIRE ligne POUR var _ expri [PAS exprp] TANT QUE condition")
    "
L'instruction FAIRE TANT QUE permet d'exécuter une boucle itérative.

ligne est le numéro n de la dernière ligne de la partie de programme à
répéter.

var est le nom de la variable contrôlée; ce doit être une variable
arithmétique simple.

expri est une expression qui donne la valeur initiale vi affectée à var.

exprp est une expression qui donne le pas vp, c'est à dire l'incrément
ajouté à var à chaque itération.  Par défaut c'est 1.

condition est une condition qui indique quand la boucle doit finir.

La première fois qu'au cours de l'exécution on arrive à l'instruction
FAIRE, la valeur vi est affectée à la variable contrôlée var.  La
condition est testée.

- si la condition est fausse alors on sort de la bouche et on va
  exécuter la première ligne qui suit la ligne de numéro n.

- sinon on exécute une fois la boucle ; le pas vp est ensuite ajouté à
  var et on évalue la condition de nouveau.  Ceci conduit de nouveau
  soit à sortir de la boucle soit à la réexécution.


Voir: FAIREJUSQUA"  #+lse-extensions ", XIT")



(definstruction ("RETOUR" "INSTRUCTION" "RETOUR")
    "
L'instruction RETOUR permet de finir l'exécution d'une procédure
sous-programme et de retourner à l'instruction qui suit l'appel de
la procédure esous-programme.

Voir: PROCEDURE, RETOUREN, RESULTAT")



(definstruction ("RETOUREN" "INSTRUCTION" "RETOUR EN ligne")
    "
L'instruction RETOUR EN permet de finir l'exécution d'une procédure
sous-programme et de retourner à la ligne indiquée.

Voir: PROCEDURE, RETOUR, RESULTAT")



(definstruction ("RESULTAT" "INSTRUCTION" "RESULTAT expression")
    "
L'instruction RESULTAT permet de finir l'exécution d'une procédure
fonction et de donner le résultat de cette fonction.  Ce résultat est
alors utilisé dans l'expression d'où venait l'appel de procédure
fonction.

Voir: PROCEDURE, RETOUR, RETOUR EN")


(definstruction ("PROCEDURE" "INSTRUCTION" "PROCEDURE &PROID(parametre,...) [LOCAL var,...]")
    "
PROCEDURE n'est pas vraiment une instruction, mais plutôt une
déclaration, car on ne peut pas l'exécuter : une erreur est signalée
si l'exécution atteint cette déclaration.

PROCEDURE permet de déclarer une procédure sous-programme ou une
procédure fonction.

&PROID est un identificateur de procédure, il est composé du caractère
&, suivi d'une lettre, suivi d'au plus quatre lettres ou chiffres.

Suit une liste entre parenthèses d'identificateurs nommant les paramètres.

Optionnellement, la liste des paramètres peut être suivie de LOCAL
suivit d'une liste d'identificateur de variables locales.

Ces variables locales peuvent aussi include des paramètres. Ils sont
alors passé par valeur, au lieu d'être passés par référence.

Une procédure sous-programme doit finir avec une instruction RETOUR ou RETOUR EN.

Une procédure fonction doit finir avec une instruction RESULTAT.

Voir: RETOUR, RETOUREN, RESULTAT")


(definstruction ("GARER" "INSTRUCTION" "GARER VAR,NE,NF")
    "
L'instruction GARER permet copier l'information contenue dans la
variable VAR (qui peut être une variable simple, une chaîne ou un
tableau) dans l'enregistrement de numéro NE du fichier NF.

NE doit être une expression donnant un nombre entier supérieur ou égal
à 1.

NF doit être une expression chaîne donnant le nom du fichier (une
lettre, suivi au plus de quatre lettres ou chiffres). Si le nom du
fichier est précédé du caractère #, il s'agit d'un fichier donnée
permanent, sinon d'un fichier temporaire.

Exemple:

1 CHAINE CH;CH_'BONJOUR'

2 I_42

3 GARER I,1,'FTEMP'

4 GARER CH,2,'#PERMA'

5 TERMINER

Voir: CHARGER, SUPPRIMER")


(definstruction ("CHARGER" "INSTRUCTION" "CHARGER VAR,NE,NF ou CHARGER VAR,NE,NF,VE")
    "
L'instruction CHARGER permet définir implicitement l'identificateur
VAR comme une variable simple, une chaîne ou un tableau et de la
charger avec le contenu de l'enregistrement de numéro NE du fichier
NF.

NE doit être une expression donnant un nombre entier supérieur ou égal
à 1.

NF doit être une expression chaîne donnant le nom du fichier (une
lettre, suivi au plus de quatre lettres ou chiffres). Si le nom du
fichier est précédé du caractère #, il s'agit d'un fichier donnée
permanent, sinon d'un fichier temporaire.

Si le paramètre optionel VE est donné, ce doit être une variable
simple, à laquelle est affectée un compte rendu du chargement:

VE = -2 le fichier de nom NF n'existe pas.

VE = -1 l'enregistrement de numéro NE n'existe pas.

VE = 0 VAR est une variable simple.

VE = 1 VAR est une variable tableau de une dimension.

VE = 2 VAR est une variable tableau de deux dimensions.

VE = 3 VAR est une variable chaîne.


Exemple:

1 CHARGER I,1,'FTEMP',VE;AFFICHER VE,I

2 CHARGER CH,2,'#PERMA',VE;AFFICHER VE,CH

3 TERMINER

Voir: GARER, SUPPRIMER")



(definstruction ("SUPPRIMER" "INSTRUCTIONS" "SUPPRIMER NF,NE ou SUPPRIMER NF")
    "
L'instruction SUPPRIMER permet de supprimer un enregistrement d'un
fichier si NE est donné, ou un fichier entier sinon.

Exemple:

1 SUPPRIMER 'FTEMP'

2 SUPPRIMER '#PERMA',2

3 TERMINER

Voir: GARER, CHARGER")



(definstruction ("PAUSE" "INSTRUCTIONS" "PAUSE")
    "
L'instruction PAUSE suspend l'exécution du programme, et fait afficher
sur l'écran un message indiquant le numéro de la ligne où se trouve
l'instruction PAUSE.

On peut fair epoursuivre l'exécution du programme en utilisant les
commandes CONTINUER, REPRENDRE ou POURSUIVRE.

Voir: TERMINER, CO, RE, PO")


(definstruction ("TERMINER" "INSTRUCTIONS" "TERMINER")
    "
L'instruction TERMINER arrête l'exécution du programme, et fait
afficher sur l'écran un message indiquant TERMINE "
 #+(or lse-unix lse-t1600) "EN LIGNE nnn" "

Cette instruction est la dernière exécutée (ce n'est pas forcément la
dernière dans le programme).

Voir: PAUSE, CO, PO, RE, EX")



(definstruction ("EXECUTER" "INSTRUCTIONS" "EXECUTER FP ou EXECUTER FP,LN")
    "
L'instruction EXECUTER fonctionne comme une double commande : elle
provoque le chargement du programme indiqué par l'expression chaîne FP
(comme la commande APPELER), puis lance son exécution à partir de la
ligne LN (ou de 1 si LN n'est pas donné) (comme la commande EXECUTER A
PARTIR DE).

Celà permet donc d'enchaîner des programmes automatiquement.

Voir: AP, EX")



(defchapter "INSTRUCTIONS"
    (write-documentation *task*
                         "Voici la liste des instructions disponibles.  

Taper DO)CUMENTATION <instruction> pour avoir la documentation de
chaque instruction:
")
  
  (let* ((chapters (find-category "INSTRUCTIONS"))
         (title-width (reduce (function max) chapters
                              :key (compose length chapter-title)
                              :initial-value 0)))
    (dolist (chapter chapters)
      (when (stringp (chapter-text chapter))
        (let ((title    (chapter-title chapter))
              (oneliner (chapter-oneliner chapter)))
          (io-format *task* "~VA  ~A~%"
                     title-width title oneliner))))))


(defchapter "FONCTIONS"
    (write-documentation *task*
     "Voici la liste des fonctions disponibles.  

Taper DO)CUMENTATION <fonction> pour avoir la documentation de
chaque fonction.
")
  (let* ((chapters (find-category "FONCTIONS"))
         (title-width (reduce (function max) chapters
                              :key (compose length chapter-title)
                              :initial-value 0)))
   (dolist (chapter chapters)
     (when (stringp (chapter-text chapter))
       (let ((syntax   (first-line (chapter-text chapter)))
             (title    (chapter-title chapter))
             (oneliner (chapter-oneliner chapter)))
         (io-format *task* "~VA  ~A~@[, ~A~];~%"
                    title-width title syntax oneliner))))))


;;;; THE END ;;;;
