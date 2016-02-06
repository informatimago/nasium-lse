;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               chapters.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Documentation.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-15 <PJB> Extracted from documentation.html
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LSE")


;;;---------------------------------------------------------------------
;;; General introduction.
;;;


(defchapter ("INTRODUCTION" "GÉNÉRALITÉS")
    "
NASIUM L.S.E.

SYSTÈME L.S.E.

LANGAGE SYMBOLIQUE D'ENSEIGNEMENT
")


(defchapter ("1. PRESENTATION" "GÉNÉRALITÉS")
    #.(format nil "
Le L.S.E. est un langage de programmation adapté aux besoins de
l'enseignement.  (C'est à dire aux besoins de l'enseignement dans les
années 1970, je ne mets pas d'émoticon car ils n'étaient pas inventés
à l'époque).  Il fut inventé par l'École Supérieure d'Électricité dans
les années 1970.  Il fût utilisé par l'enseignement secondaire en
France, et également par l'École d'Application des Transmissions de
l'Armée.

Ce langage de programmation était utilisé dans 57 lycées de France
faisant partie de l'expérience des 57 minis.  C'étaient des
mini-ordinateurs CII (SEMS) MITRA-15 et Télémécanique T-1600, sur
lesquels un environnement et un interpréteur L.S.E.  interactifs
fonctionnaient, via 8 ou 16 consoles de visualisation et 1 ou 2
télétypes/perforatrices de ruban.

Un compilateur L.S.E. pour ordinateurs CII 10170 et CII IRIS 80 sur
les systèmes SIRIS 7 et SIRIS 8 fût également développé et utilisé
dans les universités disposant de ces matériels, dont l'utilisation
était basée sur les cartes perforées.

Au début des années 1980, ces mini-ordinateurs fûrent submergés par la
vague des 10000 micros tels les R2E Micral, et les Logabax, sur
lesquels l'environnement L.S.E. et l'interpreteur L.S.E. fûrent
portés, (SARL Microdur fondée en 1982 afin de poursuivre le
développement de L.S.E., EDL et, plus recemment Logicia), puis même
sur les suivants, les compatibles PC, le Macintosh, et des matériels
Commodore.

Ces 57 mini-ordinateurs disposaient d'une mémoire centrale de 4 ou 8
kilo-mots, c'est à dire 8192 octets ou 16384 octets, pas kilo-octets,
ni même, n'y pensez pas méga-octets.  Ce document ne tiendrait pas
entierement dans ces mémoires centrales!  Les disques durs
atteignaient à peine les 256 Ko et les disquettes de 8\" les 64 Ko.
L'espace de mémoire centrale restant pour stocker les programmes
L.S.E. et les données de l'utilisateurs était de l'ordre de 2000 mots
(4 ko, 4096 octets !  On pouvait les compter sur ses doigts en
binaire, et il restait 8 doigts).  Quand à l'espace de disque dur
disponible, il se comptait en dizaines de secteurs de 256 octets, pas
beaucoup plus.  Heureusement, on disposait d'un lecteur de disquettes 8\".
D'accord, disons plutôt, de galettes souples, de 8 pouce, c'est
pas vraiment des disquettes.  Leur capacité étant, si je me souviens
bien de 40 pistes de 8 secteurs de 128 octets soit 40960 octets en
tout (plus que la mémoire centrale).

Et bien entendu, les unités centrales de ces \"mini\" ordinateurs
occupaient la place d'un trés gros réfrigerateur, et nécessitaient une
alimentation électrique triphasée.

~a devrait vous permettre de resentir les joies qu'offraient un
tel environnement de programmation.  Malgrès tous nos efforts, nous ne
réussirons surement pas à vous empêcher de programmer en L.S.E. en
évitant les avantages offerts par les méga-systèmes-GUI actuels.
Néanmoins, il constitura surement un article de valeur dans le musée
historique virtuel des systèmes anciens.

L.S.E. n'est plus un langage \"vivant\" et il ne me semble pas
intéressant d'envisager une implémentation \"contemporaine\" (voir les
versions pour les premiers micro-ordinateurs, avec introduction des
miniscules et des graphiques bit-map).  Aussi ce projet se limite
volontairement au système qui existait sur MITRA-15 avec toutes ses
contraintes et limites. ~a

Veuillez envoyer vos commentaires, rapports de bogues, suggestions à :
Pascal Bourguignon
mailto:pjb@ogamita.com
"
              #+lse-unix "LSE" #-lse-unix "EMULSE" 
              #+lse-unix "Avec cependant quelques modifications afin
de l'adapter à l'environnement unix."  #-lse-unix ""))


(defchapter ("2. B.A.BA" "GÉNÉRALITÉS")
    #.(format nil "
~A

Dans l'état actif, on peut donner:

- des commandes,

- des lignes de programmes numérotées, et

- des instructions à exécuter immédiatement en mode \"machine de bureau\".

Les commandes sont données avec leur deux premières lettres.  Le
système L.S.E. complète la commande (sauf lorsque la commande ABREGER
a été donnée) une fois que la saisie des deux lettres est validée.


Les saisies sont validées avec le code X-OFF, c'est a dire,
CONTRÔLE-S, pas avec ENTRÉE.  Ceci étant dû au fonctionnement des
consoles qui acceptent la saisie d'une ligne sans intervention de
l'unité centrale.  la pression de X-OFF provoque la transmission de la
ligne saisie a l'unite centrale.

Les consoles SINTRA fonctionnaient comme une télétype : la saisie
(hors ligne) n'était possible que sur la ligne du bas.  il n'y avait
pas de curseur, la ligne était emplie de soulignés que la saisie
recouvrait.  L'affichage se faisaient également à partir du bas de
l'écran et à la réception d'un saut de ligne ça défilait vers le haut
comme le papier de la télétype et sans possibilité de revenir en
arrière.  ~1@*~A ne simule pas cette ligne de soulignés et laisse
l'emulateur de terminal utilisé afficher un curseur.


Comme la saisie se faisait sans intervention de l'unité centrale, il
n'y avait pas moyen de corriger vraiment les caractères saisis (pas
d'édition de la ligne de saisie).  On peut cependant commander la
suppression du ou des caractères précédement saissis en tapant un ou
plusieurs caractères contre-cotice :

Taper :                BON\\\\\\SALE\\UT! 

Équivaut à taper :     SALUT!

La touche ÉCHAPEMENT (ESC) permet d'interrompre le traitement et
revenir à l'invite.

La touche CONTRÔLE-A permet d'envoyer un signal asynchrone qui est lu
avec la fonction ATT().

En résumé :

- CONTRÔLE-S pour saisir les données,

- ÉCHAPEMENT pour interrompre,

- CONTRÔLE-A pour envoyer un signal lu par ATT(),

- control-cotice \\ pour supprimer un caractère.

- ENTRÉE peut aussi s'utiliser pour entrer une chaîne de caractère,
  mais alors un code RETOUR CHARIOT (13) est ajouté à la fin de la
  chaîne.


~A

Nous avons introduit quelques commandes nouvelles, en particulier :

- AI)DE permet d'afficher la liste des commandes disponibles dans
  l'état où on se trouve.

- DO)CUMENTATION permet de lire la documentation en-ligne.



Exemple de session.

Rappel : les commandes se tapent avec leurs deux premiers caractères
et la touche d'entrée (CONTRÔLE-S ou ENTRÉE selon la configuration),
lse complétant:  B O [XOFF] --> BONJOUR.

|   || = LIGNE AFFICHEE PAR LE SYSTEME.
|   
|   >| = LES DEUX PREMIERS CARACTÈRES SONT TAPES PAR L'UTILISATEUR, SUIVIT
|        DE [XOFF] (ET ÉVENTUELLEMENT LE PARAMÈTRE DE LA COMMANDE).
|   
|   >> = TOUTE LA LIGNE EST TAPÉE PAR L'UTILISATEUR, TERMINÉE PAR [ENTRÉE]
|   
|   |> = AFFICHAGE D'UN MESSAGE PAR LE SYSTÈME, SUIVIT D'UNE SAISIE PAR
|        L'UTILISATEUR TEMINÉE PAR [XOFF].
|   
|   
|   ||  NASIUM L.S.E.
|   ||  LANGAGE SYMBOLIQUE D'ENSEIGNEMENT
|   ||  VERSION 1.0.0-0.272-CL-UNIX
|   ||  COPYRIGHT 1984 - 2014 PASCAL BOURGUIGNON
|   ||  
|   ||  DISTRIBUE SELON LES TERMES DE LA LICENCE AGPLv3.
|   ||  
|   ||  Ce programme est livré avec ABSOLUMENT AUCUNE GARANTIE; pour plus de
|   ||  détails utilisez la commande DO GARANTIE.  Ce logiciel est libre, et
|   ||  vous êtes les bienvenus pour redistribuer sous certaines conditions;
|   ||  utilisez la commande DO LICENSE pour plus de détails.
|   ||  
|   ||  Tapez AI pour avoir de l'aide.
|   ||  
|   ||  BONJOUR     21:15:40
|   ||  
|   ||  PRET
|   ||  
|   >>  1*TEST
|   >>  10 AFFICHER 'QUI EST\\ TU ? '
|   >>  20 CHAINE NOM;LIRE NOM
|   >>  30 AFFICHER 'ENCHANTE ',NOM,' !'
|   >>  40 AFFICHER 'N\\\\NOM,' \\, DONNE MOIS\\ UN NOMBRE, SIL T \\\\\\\\\\'IL TE PLAIT : '
|   >|  LISTER A PARTIR DE 40
|   ||  40 AFFICHER NOM,', DONNE MOI UN NOMBRE, S''IL TE PLAIT : '
|   >>  50 LIRE N
|   >>  60 AFFICHER '\\N,' * 'M\\,NM\\,' = ',N*N
|   >>  70 AFFICHER 'AU REVOIR '\\\\M\\, ',NOM
|   >>  80 TERMINER
|   >|  LISTER A PARTIR DE 1
|   ||  1*TEST
|   ||  10 AFFICHER 'QUI ES TU ? '
|   ||  20 CHAINE NOM;LIRE NOM
|   ||  30 AFFICHER 'ENCHANTE ',NOM,' !'
|   ||  40 AFFICHER NOM,', DONNE MOI UN NOMBRE, S''IL TE PLAIT : '
|   ||  50 LIRE N
|   ||  60 AFFICHER N,' * ',N,' = ',N*N
|   ||  70 AFFICHER 'AU REVOIR, ',NOM
|   ||  80 TERMINER
|   >|  EXECUTER A PARTIR DE 1
|   |>  QUI ES TU ? PASCAL
|   ||  ENCHANTE PASCAL !
|   |>  PASCAL, DONNE MOI UN NOMBRE, S'IL TE PLAIT : 12
|   ||  12  * 12  = 144 
|   >|  AU REVOIR, PASCAL
|   ||  TERMINE
|   ||  
|   >|  AU REVOIR  04:09:21
"
              
              #+lse-unix "Au lancement de la commande lse, on entre
directement dans l'état actif, et la commande AU)REVOIR fait quiter le
programme lse et revenir à la coquille (au \"shell\")."
              #-(and) "Voir le document 'SYSTEMES.TXT'."
              
              #-lse-unix "La console est d'abord en état dormant.
Pour passer à l'état actif, il faut utiliser la commande BO)NJOUR.
Pour revenir à l'état dormant (ce qui efface la mémoire de travail),
utiliser la commande AU)REVOIR.

La commande spécifique à emulse AD)IEUX permet d'arrêter le système
L.S.E."
              
              "lse"
              
              #+lse-unix "
Cependant, on peut sélectionner le mode moderne, dans lequel les
touches utilisées sont celles configurées par stty(1), normalement:

- ENTRÉE pour saisir les données,

- CONTRÔLE-C pour interrompre,

- CONTRÔLE-\\ pour envoyer un signal lu par ATT(),

- EFFACEMENT pour supprimer un caractère.

Ceci permet de conserver ses habitudes récentes.
"
              #-lse-unix ""

))







"
INSTALLATION A PARTIR DES SOURCES
=================================

0- lire entierement ce document.
1- make cli
2- su root
3- groupadd lse
4- adduser lse
5- make install
6- su user
7- lse
8- et programmer en L.S.E.
"

"
FONCTIONNEMENT GENERAL
======================

*** PAS ENCORE IMPLEMENTE ***

EMULSE TOURNE DANS UN PROCESSUS UNIX UNIQUE, SIMULANT LE MITRA-15 SOUS
SYSTEME L.S.E.  CE PROCESSUS PREND COMME ARGUMENT UN NUMERO DE PORT
SUR LEQUEL IL ATTEND LES CONNEXIONS DES \"CONSOLES\".

IL Y A UN SCRIPT PERMETTANT DE CONFIGURER UNE FENETRE XTERM AVEC UN
ASPECT RETRO : CARACTERES VERTS SUR FOND NOIR, BIEN QUE LES CONSOLES
SINTRA AVAIENT DU PHOSPHORE BLANC SUR FOND NOIR, ET POLICE
DEC-TERMINAL (QUI COMPORTE DES CARACTERES FLECHES GAUCHE ET HAUT
UTILISES POUR L'ASSIGNATION ET L'EXPONENTIATION.  LES LIGNES SONT
\"ENVOYEES\" A L'UNITE CENTRALE EN TAPANT CONTROL-S (CODE ASCII X-OFF),
ET NON RETURN !

SONT  EGALEMENT FOURNIS UNE PAGE HTML ET LE CLIENT TELNET JAVA SOUS
LICENCE GPL DE  MATTHIAS L.  JUGEL ET MARCUS MEISSNER, PERMETTANT
D'ACCEDER A UN SYSTEME EMULSE A PARTIR D'UNE PAGE WEB.  CECI PERMET
PAR EXEMPLE D'AJOUTER AUTOUR DE L'APPLET TERMINAL CONNECTE AU SYSTEME
L.S.E.  UNE INTRODUCTION AU LANGAGE L.S.E.



::

      *
      |
      |
      V
  +--------+  connecter     +----------+   BOnjour     +----------+
  | Limbo  |--------------->| Sleeping |-------------->| Active   |
  |        |                |          |  /preparer    |          |
  |        |                |          |               |          |
  |        |   ADieux/      |          |               |          |
  |        |   deconnecter  |          |   AUrevoir    |          |
  |        |<---------------|          |<--------------|          |
  +--------+                +----------+  /nettoyer    +----------+
      ^                                                     |
      |               etat_cmd_changer(0)                   |
      +-----------------------------------------------------+
                       /nettoyer




STOCKAGE DES FICHIERS L.S.E. SUR UNIX
=====================================

LORS DE LA COMPILATION D'EMULSE, ON PEUT DEFINIR DES MACROS STIPULANT
LES CHEMINS D'ACCES AUX DIFFERENTS REPERTOIRES DISCUTES CI-APRES.
VOICI CEPENDANT LA HIERARCHIE ET LA CONFIGURATION DE DROITS D'ACCES
PRECONISES.

DRTS PRO/GRP CHEMIN                DESCRIPTION
---- ------- --------------------  ------------------------------------------
0755 lse.lse $(BASE)
0755 lse.lse $(BASE)/bin           LES EXECUTABLES UNIX
4755 lse.lse $(BASE)/bin/lse       L'INTERPRETEUR/ENVIRONNEMENT L.S.E.
0700 lse.lse $(BASE)/admin         CONFIG, DROITS D'ACCES, COMPTES, ...
0700 lse.lse $(BASE)/admin/compt   LISTES DES (CLE+COMPTE) ET DROITS D'ACCES
0700 lse.lse $(BASE)/admin/progr   ASSOCIE FICHIERS PROGRAMMES ET COMPTES
0700 lse.lse $(BASE)/admin/perma   ASSOCIE FICHIERS PERMANENTS ET COMPTES
0700 lse.lse $(BASE)/perma         FICHIERS PERMANENTS DE L.S.E.
0700 lse.lse $(BASE)/tempo         FICHIERS TEMPORAIRES DE L.S.E.
0700 lse.lse $(BASE)/tempo/00      UN SOUS REPERTOIRE PAR CONSOLE
0700 lse.lse $(BASE)/tempo/01      (LES FICHIERS TEMPORAIRES SONT LIES 
0700 lse.lse $(BASE)/tempo/02       A LA CONSOLE PAR LAQUELLE ON ACCEDE
0700 lse.lse $(BASE)/tempo/03       A L'ENVIRONNEMENT L.S.E.)
0700 lse.lse $(BASE)/tempo/04      CES FICHIERS SONT PRIVES POUR CHAQUE
0700 lse.lse $(BASE)/tempo/05      UTILISATEUR, ET SONT SUPPRIMES EN FIN
0700 lse.lse $(BASE)/tempo/06      DE SESSION.
0700 lse.lse $(BASE)/tempo/07
0700 lse.lse $(BASE)/progr         FICHIERS PROGRAMMES (\"BINAIRES\")
1755 lse.lse $(BASE)/ruban         BIBLIOTHEQUE DE BANDES PERFOREES VIRTUELLES
0755 lse.lse $(BASE)/ruban/EXEMP   CONTIENT LES BANDES PERFOREES EXEMPLES.


UN COMPTE ET UN GROUPE UNIX NOMMES \"LSE\" PEUVENT ETRE CREES POUR
INSTALLER EMULSE.  TOUS LES FICHIERS LSE LEUR APPARTIENDRONT

DANS LA MESURE OU LES FICHIERS L.S.E.  ONT UN FORMAT PROPRE (STRUCTURE
D'ENREGISTREMENTS), ILS NE SONT INTERPRETABLE QUE PAR LE
BIBLIOTHECAIRE L.S.E., ET DIFFICILEMENT UTILISABLES PAR LES
UTILITAIRES UNIX STANDARD.  AUSSI ON PEUT SANS PROBLEME PROHIBER LEUR
ACCES PAR LE RESTE DU SYSTEME.

D'UN AUTRE COTE, LA STRUCTURE DES BANDES PERFOREES ET TOUT A FAIT
EXPLOITABLE PAR LES UTILITAIRES UNIX [FIN DE LIGNE X-OFF (DC3, 0x13,
19) AU LIEU DE (LF, 0x0A, 10)], ET PEUT SERVIR DE PASSERELLE POUR
L'IMPORTATION/EXPORTATION DE DONNEES OU DE SOURCES (COMME LES BANDES
PERFOREES LE PERMETTAIENT, COMME SUPPORT COMPATIBLE ENTRE MITRA-15 ET
T-1600 PAR EXEMPLE). AINSI, $(BASE)/ruban EST ACCESSIBLE A TOUS.
CEPENDANT, PUISQUE LES BANDES PERFOREES ETAIENT UN SUPPORT WORM
(WRITE-ONCE-READ-MULTIPLE), LES FICHIERS RUBAN SONT AUTOMATIQUEMENT
MIS EN LECTURE SEULE PAR LSE.

DES SOUS-REPERTIORES DANS $(BASE)/ruban POURRONT ETRE CREES A VOLONTE
POUR CLASSER LES BANDES.  NOTER QUE LE BIBLIOTHECAIRE L.S.E.  N'A PAS
DE NOTION DE REPERTOIRE : L'ESPACE DE NOM (1 LETTRE SUIVIT D'AU PLUS 4
ALPHA-NUMERIQUES) 26*36^4 > 43E6 ETANT LARGEMENT SUFFISANT POUR DES
DISQUES DUR DE NETTEMENT MOINS D'UN MEGA-OCTETS, OU DES DISQUETTES DE
MOINS DE 100 KILO-OCTETS.

ATTENTION : L.S.E.  CONVERTI EN MAJUCULE TOUTES LES LETTRES DANS LES
NOMS DES FICHIERS ET REPERTOIRES SOUS RUBAN.  MIEUX VAUT DONC
N'UTILISER QUE DES MAJUSCULES AFIN D'EVITER LES COLISIONS DE NOM.





GENERATION DU FICHIER COMPT
===========================

ETANT DONNE L'OBJECTIF PEDAGOGIQUE DE L.S.E., LE SYSTEME IMPOSAIT UN
MINIMUM DE CONTRAITE POUR L'ACCES AU SYSTEME.  AINSI, IL SUFFIT DE
TAPER LA COMMANDE \"BO\" (BONJOUR) POUR POUVOIR COMMENCER A PROGRAMMER
ET POUR AVOIR ACCES AUX PROGRAMMES PUBLIQUES.  PAR CONTRE, POUR ETRE
AUTORISE A ENREGISTRER UN PROGRAMME OU UN FICHIER DONNEE PERMANENT, IL
FAUT S'IDENTIFIER AUPRES DU SYSTEME.  L'IDENTIFICATION COMPORTE  UNE
CLE SECRETE SUR TROIS CHIFFRES ET UN NUMERO DE COMPTE SUR DEUX
CHIFFRES.  CHAQUE COMPTE A DES DROITS D'ACCES CONFIGURABLES PAR
L'ADMINISTRATEUR.

DANS EMULSE, LES CLES SECRETES ET LES DROITS D'ACCES SONT ENREGISTRES
DAS LE FICHIER $(BASE)/admin/compt, LE FICHIER \"COMPT\".

CE FICHIER CONTIENT EXACTEMENT 512 OCTETS (2 SECTEURS) FORMATES EN:
100 LIGNES DE 4 CARACTERES PLUS NEW-LINE : XXXK\\N,  COMPLETE
PAR 12 OCTETS SUPPLEMENTAIRES  POUR UN TOTAL 512 OCTETS.

LES XXX SONT LA CLE SECRETE DE CHAQUE IDENTIFICATEUR.  LE NUMERO DE
COMPTE CORRESPONDANT AU NUMERO DE LA LIGNE (PARTANT DE 00).

LE CARACTERE K SUIVANT ENCODE LES DROITS D'ACCES.  LES QUATRES BITS DE
POIDS FAIBLE SONT UTILISES SELON LA TABLE SUIVANTE :

[1 = A: CREATION, MODIFICATION, SUPPRESSION DE PROGRAMMES]
[2 = B: CREATION, MODIFICATION, SUPPRESSION DE PERMANENTS]
[4 = C: AUGMENTATION DYNAMIQUE DE L'ESPACE TEMPORAIRE    ]
[8 = D: ALLOCATION D'UN ESPACE TEMPORAIRE FIXE SUPERIEUR ]

COMBINAISON DE DROITS :  0    A    B    AB   C    AC   BC   ABC  D    BD   ABD
CODES A UTILISER       
EN QUATRIEME COLONE :    0    1    2    3    4    5    6    7    8    :     ;
                 OU :    @    A    B    C    D    E    F    G    H    J     K

DONC, C ET D SONT EXCLUSIFS L'UN DE L'AUTRE, ET A PART CETTE
CONTRAINTE, TOUTES LES COMBINAISONS SONT POSSIBLES.

PAR DEFAUT NOUS METTONS LES DROITS A ZERO POUR TOUS LES COMPTES, SAUF
LE 99 QUI EST LE COMPTE AUQUEL SONT ASSIGNES LES \"NOUVEAUX\" PROGRAMMES
ET FICHIERS PERMANENTS QUI SONT PLACES DANS LES REPERTOIRES
$(BASE)/PERMA ET $(BASE)/PROGR PAR D'AUTRES PROGRAMMES QUE EMULSE.
EMULSE CONSERVE UNE TABLE DES FICHIERS QU'IL GERE EN CORRESPONDANCE
AVEC LE COMPTE AUQUEL ILS APPARTIENNENT.


POUR LE MOMENT, IL FAUT EDITER LE FICHIER COMPT (HORS DE LSE) POUR
CHANGER LES CLES.  LES DROITS D'ACCES PEUVENT ETRE CONFIGURES A PARTIR
DE LSE, AVEC LA COMMANDE DR(OITS).



DIFFERENCES
===========

EMULSE VEUT \"EMULER\" L'IMPLEMENTATION L.S.E. DU MITRA-15, AU NIVEAU DE
L'UTILISATEUR, SANS CHERCHER FORCEMENT A REPRODUIRE LE FONCTIONNEMENT
INTERNE DU SYSTEME L.S.E.  SUR MITRA-15. (IMPLEMENTATION \"COMPATIBLE\"
MITRA-15). CEPENDANT, CERTAINES DIFFERENCES SONT INEVITABLES :

    - JEUX DE PERIFERIQUES DIFFERENTS,
    - MANQUE DE DOCUMENTATION,
    - SIMPLICITE ET RAPIDITE D'IMPLEMENTATION,
    - AJOUT DE FACILITES.







COMMANDES SUPPLEMENTAIRES
-------------------------

DR(OITS) MOT-DE-PASSE  
    
    (HORS-SESSION)
    PERMET DE DEFINIR LES DROITS D'ACCES DES COMPTES.

    IL EXISTAIT UNE COMMANDE SIMILAIRE, MAIS JE N'EN AI PAS LA
    DOCUMENTATION, ET NE ME SOUVIENS PLUS DE SON INTERFACE UTILISATEUR.


AI(DE)
    
    DONNE LA LISTE DES COMMANDES DISPONIBLES.


AD(IEUX)

    PERMET DE SE DECONNECTER COMPLETEMENT DU SYSTEME L.S.E.



COMMANDES DIFFERENTES
---------------------

PE(RFORER A PARTIR DE)

    NE PERFORE PAS DE RUBAN, MAIS ECRIT UN FICHIER DANS LE
    REPERTOIRE $(BASE)/RUBAN.

    APRES AVOIR DEMANDE L'INTERVALE DE LIGNES A PERFORER, CETTE
    COMMANDE DEMANDE SOUS QUEL NOM FAUT-IL ARCHIVER LA BANDE QUE L'ON
    VA PERFORER, ET ENREGISTRE EN FAIT UN FICHIER DANS
    $(BASE)/RUBAN/NOM_DONNE.

RU(BAN)

    NE LIT PAS DE RUBAN PERFORE, MAIS LIT UN FICHIER DANS LE
    REPERTOIRE $(BASE)/RUBAN.

    DONNE LA LISTE DES RUBANS (VIRTUELS) ARCHIVES DANS L'ARMOIRE, ET
    DEMANDE LEQUEL ON DOIT LIRE.



"

(defchapter ("7. MODE EMACS" "GÉNÉRALITÉS")
   "
Un mode emacs pour l'édition des programmes L.S.E. est fourni avec les
sources (voir section LÉGAL, SOURCES).

En particulier, il remplace l'affichage caractère souligné par une flêche
gauche (unicode)  de l'affectation, et celui du caractère chapeau par
une flêche vers le haut (unicode) de la puissance, qui étaient dans le
jeu de caractère des systèmes L.S.E.

Les numéros de lignes ont automatiquement insérés par la touche
retour, et une commande pour renuméroter est fournie.

Pour l'activer, utiliser la commande emacs:

    M-x load-file RET lse-mode.el RET

ou placer dans ~/.emacs:

    (require 'lse-mode)


http://www.gnu.org/s/emacs

http://www.emacsformacosx.com/
")


(defchapter ("8. REFERENCES" "GÉNÉRALITÉS")
    "
| \"LANGAGE SYMBOLIQUE D'ENSEIGNEMENT L.S.E.  MANUEL D'UTILISATION\"
| BULLETIN DE LIAISON \"L'INFORMATIQUE ET L'ENSEIGNEMENT SECONDAIRE\"
| JANVIER 1975
| C.LAFOND - INSTITUT NATIONAL DE RECHERCHE ET DE DOCUMENTATION PEDAGOGIQUES
|
|
| \"LANGAGE SYMBOLIQUE D'ENSEIGNEMENT LSE 15 - MANUEL D'UTILISATION\"
| ECOLE D'APPLICATION DES TRANSMISSIONS - DIRECTION DE L'INSTRUCTION
| DIFFUSION INTERNE A DES FINS PEDAGOGIQUES SEULEMENT
| ARTICLE II MARCHE 78 P 78
|
|
| \"LANGAGE LSE SOUS SIRIS 7 / SIRIS 8 - MANUEL D'UTILISATION\"
| P. FISCHER, P. MERCIER (I.U.C.A)
| REFERENCE : NOTC0031 MAIS 1978
| INSTITUT UNIVERSITAIRE DE CALCUL AUTOMATIQUE DE LORRAINE
| INSTITUT UNIVERSITAIRE DE TECHNOLOGIE (DEPARTEMENT INFORMATIQUE)
| INSTITUT DE RECHERCHE SUR L'ENSEIGNEMENT DES MATHEMATIQUES.
|
|
| \"L.S.E. POUR TOUS\"
| EPI - ENSEIGNEMENT PUBLIC ET INFORMATIQUE (ASSOC. LOI DE 1901)
| BULLETIN JANVIER 81 NO.  SPECIAL  - 1ER EDITION - ISBN 2-86469-014-5 - 1981
|
|
| NORME AFNOR 1982
|
|
| \"L.S.E. POUR TOUS\"
| CENTRE NATIONAL DE DOCUMENTATION PEDAGOGIQUE
| EPI - ENSEIGNEMENT PUBLIC ET INFORMATIQUE (ASSOC. LOI DE 1901)
| BROCHURE NO. 42006  - 4E EDITION 1983
|
|
| \"LE SYSTEME LSE - MIEUX CONNAITRE SON FONCTIONNEMENT SUR MICRO-ORDINATEUR\"
| EPI - ENSEIGNEMENT PUBLIC ET INFORMATIQUE (ASSOC. LOI DE 1901)
| DOSSIER EPI NO. 3 JUIN 89
|
|
| \"La Saga du LSE et de sa famille (LSD/LSG/LST)\"

http://wwwsi.supelec.fr/yn/sagaLSx.html

http://www.epi.asso.fr/revue/54/b54p216.htm

| \"LSE83\"
| Jacques Arsac
| Bulletin de l'EPI n° 38   

http://archive-edutice.ccsd.cnrs.fr/docs/00/03/06/74/PDF/b38p116.pdf 


Article Wikipedia sur le L.S.E. :

http://fr.wikipedia.org/wiki/LSE_(langage_de_programmation)
")


;;;---------------------------------------------------------------------
;;; Legal
;;;

(defchapter ("GARANTIE" "LÉGAL")
    "
Système L.S.E

Copyright (C) 1984 - 2014 Pascal Bourguignon

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with this program.  If not, see
http://www.gnu.org/licenses/
")


(defchapter ("LICENSE" "LÉGAL")
    #.*license*)


(defchapter ("SOURCES" "LÉGAL")
    "
Les sources de ce programme sont disponibles sous license AGPLv3
à l'adresse suivante:

http://www.ogamita.com/lse/
")



;;;---------------------------------------------------------------------
;;; Commands introduction.
;;;

(defchapter ("INTRODUCTION" "COMMANDES")
    #.(format nil "

Ce chapitre présente une description succinte des commandes du
système L.S.E.  Pour plus de détails, voir les références.


Une fois démarré le système L.S.E. affiche le mot: PRET.  Il est alors
prêt à recevoir une commande, une instruction à exécuter en mode
machine de bureau, ou une ligne de programme.


On donne une commande au système L.S.E. en saisissant les deux
premiers caractères de la commande et en les envoyant au système en
tapant sur la touche [X-OFF].  Le système complète alors le nom de la
commande, et éventuellement attend la saisie des arguments, puis il
exécute la commande.


Chacune des sections suivantes décrivent une commande, selon le
format suivant:

- nom de la commande,

- syntaxe: les deux lettres initiales à taper, ), le reste du nom de
  la commande, suivit éventuellement des arguments attendus.

- description,

- exemples (optionels),

- références (optionelles).


Description de la syntaxe des arguments:

- <N> : un numéro de ligne.

- <N1>,<N2> : deux numéros de ligne, séparés par une virgule.

- [<N1>[,<N2>]] : deux numéros de ligne optionnels séparés par une
  virgule.  On peut saisir aucun numéro, un numéro, ou deux numéro
  séparés par une virgule.

- * | <N1>[,<N2>[...,<Nn>]...] | <N1> A <N2> : une étoile représente
  toutes les lignes; sinon on peut écrire une liste de numéros de
  ligne, ou d'intervale (deux numéros de lignes séparés par le mot A),
  séparés par une virgule.

- <NOMPG> : un nom de programme.  C'est normalement un identificateur,
  composé d'une lettre, suivi d'au plus quatre lettres ou chiffres.
  ~A

- <NOMFI> : un nom de fichier.  C'est normalement un identificateur,
  composé d'une lettre, suivi d'au plus quatre lettres ou chiffres.
  ~A

- <NOMF1>,<NOMF2> : deux noms de fichier, séparés par une virgule.

- <NOMFI>[,<N1>[,<N2>]] : un nom de fichier, suivit de deux numéros de
  lignes optionnels.

- <TEXTE> : une ligne de texte.  

- * | <NOMFI>,P|D|T : soit une étoile signifiant tous les fichiers
  temporaires, soit un nom de fichier, suivit d'une virgule et d'une
  lettre P, D, ou T, signifiant: P = fichier programme, D = fichier
  donnée permanent, T = fichier temporaire.

"
              #+lse-unix "Le nom es mis en minuscule et une extension
'.lse' est ajoutée pour former le nom du fichier unix."
              #-lse-unix ""
              #+lse-unix "Le nom es mis en minuscule et une extension
'.don' est ajoutée pour former le nom du fichier unix."
              #-lse-unix ""))


;;;---------------------------------------------------------------------
;;; Functions introduction.
;;;

(defchapter ("INTRODUCTION" "FONCTIONS")
    "
Ce chapitre présente une description succinte des fonctions du
système L.S.E.  Pour plus de détails, voir les références.

On inclu également les opérateurs.  Les expressions peuvent combiner
plusieurs opérations et appels de fonctions.  Voir la section
PRIORITE, pour l'ordre d'évaluation des opérateurs, lorsqu'ils ne sont
pas groupés avec des parenthèses.

Les appels peuvent se faire aux fonctions primitives décrite dans ce
chapitre, ou à des procédure fonctions déclarée dans le programme
(voir instructions PROCEDURE et RESULTAT).  Les procédure fonctions
ont un identificateur començant par un caractère '&'.


Chacune des sections suivantes décrivent une commande, selon le
format suivant:

- nom de la fonction, description succinte,

- syntaxe,

- description,

- exemples (optionels),

- références (optionelles).
")


;;;---------------------------------------------------------------------
;;; Instructions introduction.
;;;


(defchapter ("INTRODUCTION" "INSTRUCTIONS")
    #. (format nil "

Ce chapitre présente une description succinte des instructions du
langage de programmation L.S.E.  Pour plus de détails, voir les
références.

Un programme se compose d'une sequence discontinue de lignes de
programmes numérotés de 1 à ~A.  Chaque ligne de programme peut
contenir plusieurs instructions, séparées par un point-virgule.  La
dernière instruction peut être un commentaire commençant par le
caractère *, et allant jusqu'à la fin de la ligne.

On peut aussi saisir une instruction en mode machine de bureau, c'est
à dire sans numéro de ligne.  Elle est alors exécutée immédiatement.

Exemple, un petit programme:

1* Résolution d'une équation du second degré.
10 AFFICHER 'A=';LIRE A
12 AFFICHER 'B=';LIRE B
14 AFFICHER 'C=';LIRE C
16 AFFICHER['Equation: U,'*X^2 + ',U,'*X + ',U,' = 0']A,B,C
20 DELTA_B*B-4*A*C
22 SI DELTA<0 ALORS AFFICHER 'Pas de solutions réelles.';TERMINER
24 SI DELTA=0 ALORS AFFICHER 'Solution: X=',-B/2/A;TERMINER
26 AFFICHER 'Première solution: X=',(-B+RAC(DELTA))/2/A
28 AFFICHER 'Deuxième solution: X=',(-B-RAC(DELTA))/2/A
30 TERMINER


Chacune des sections suivantes décrivent une instruction, selon le
format suivant:

- nom de l'instruction,

- syntaxe,

- description,

- exemples (optionels),

- références (optionelles).

"
               #+LSE-T1600 80
               #+LSE-MITRA-15 80
               #-(or LSE-T1600 LSE-MITRA-15) 65535))



(definstruction ("AFFECTATION" "INSTRUCTIONS" "ref _ expression")
    "
L'instruction d'affectation permet de changer la valeur d'une variable
ou d'un élément de tableau.  ref doit être un identificateur de
variable: une lettre suivie d'au plus quatre lettres ou chiffres, ou
une référence de tableau: un identificateur de tableau, suivi d'un
crochet ouvrant, d'une ou deux expressions index séparées par une
virgule, et un crochet fermant.  L'identificateur ne peut pas être un
mot réservé du langage L.S.E.  

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
pour qu'elles soient considérées comme une seule.

Voir: INSTRUCTIONSI")


(definstruction ("FAIREJUSQUA" "INSTRUCTIONS" "FAIRE ligne POUR var _ expri [PAS exprp] JUSQUA exprf")
    #.(format nil "
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


Voir: FAIRETANTQUE~A"  #+lse-extensions ", XIT" #-lse-extensions ""))



(definstruction ("FAIRETANTQUE" "INSTRUCTIONS" "FAIRE ligne POUR var _ expri [PAS exprp] TANT QUE condition")
    #.(format nil "
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


Voir: FAIREJUSQUA~A" #+lse-extensions ", XIT" #-lse-extensions ""))



(definstruction ("RETOUR" "INSTRUCTIONS" "RETOUR")
    "
L'instruction RETOUR permet de finir l'exécution d'une procédure
sous-programme et de retourner à l'instruction qui suit l'appel de
la procédure esous-programme.

Voir: PROCEDURE, RETOUREN, RESULTAT")



(definstruction ("RETOUREN" "INSTRUCTIONS" "RETOUR EN ligne")
    "
L'instruction RETOUR EN permet de finir l'exécution d'une procédure
sous-programme et de retourner à la ligne indiquée.

Voir: PROCEDURE, RETOUR, RESULTAT")



(definstruction ("RESULTAT" "INSTRUCTIONS" "RESULTAT expression")
    "
L'instruction RESULTAT permet de finir l'exécution d'une procédure
fonction et de donner le résultat de cette fonction.  Ce résultat est
alors utilisé dans l'expression d'où venait l'appel de procédure
fonction.

Voir: PROCEDURE, RETOUR, RETOUR EN")


(definstruction ("PROCEDURE" "INSTRUCTIONS" "PROCEDURE &PROID(parametre,...) [LOCAL var,...]")
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


(definstruction ("GARER" "INSTRUCTIONS" "GARER VAR,NE,NF")
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


(definstruction ("CHARGER" "INSTRUCTIONS" "CHARGER VAR,NE,NF ou CHARGER VAR,NE,NF,VE")
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
    #.(format nil "
L'instruction TERMINER arrête l'exécution du programme, et fait
afficher sur l'écran un message indiquant TERMINE ~A

Cette instruction est la dernière exécutée (ce n'est pas forcément la
dernière dans le programme).

Voir: PAUSE, CO, PO, RE, EX"
            #+(or lse-unix lse-t1600) "EN LIGNE nnn"
            #-(or lse-unix lse-t1600) ""))



(definstruction ("EXECUTER" "INSTRUCTIONS" "EXECUTER FP ou EXECUTER FP,LN")
    "
L'instruction EXECUTER fonctionne comme une double commande : elle
provoque le chargement du programme indiqué par l'expression chaîne FP
(comme la commande APPELER), puis lance son exécution à partir de la
ligne LN (ou de 1 si LN n'est pas donné) (comme la commande EXECUTER A
PARTIR DE).

Celà permet donc d'enchaîner des programmes automatiquement.

Voir: AP, EX")



(defchapter ("INTRODUCTION" "GRAMMAIRE")
    "

Les symboles terminaux peuvent être nommés ou anonymes.  Les symboles
terminaux nommés sont écrits en majuscules, et sont décrits par une
expression régulière écrite entre cotices.  Les symboles terminaux
anonymes sont écrits dans les règles de grammaire entre apostrophes.

Exemple: un symbole terminal nommé:

|    IDENTIFICATEUR       = /[A-Z][0-9A-Z]*/.


Exemple: un symbole terminal anonyme:

|    'AFFICHER'


Les symboles non-terminaux sont écrits en minuscules.

Les règles de grammaire ont la forme :

|    symbole-non-terminal ::= partie-droite .

La partie droite peut être (a et b sont des parties droites) :

- une séquence : a b 

- une alternative (éventuellement entre parenthèse pour éviter les ambiguités) : a | b 

- une optionelle (0 ou 1) : [ a ] 

- une répetitive (0 ou plus) : { a } 

- un symbole non-terminal ;

- un symbole terminal.

Le symbole initial de la grammaire est debut.

nil représente le vide.

")

;;;---------------------------------------------------------------------
;;; Root chapters
;;;

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
