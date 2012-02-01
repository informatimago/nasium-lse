;;;;****************************************************************************
;;;;FILE:               configuration.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the configuration of the lisp system.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-24 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2005
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

(let ((dir ;; /home/pjb/src/lse-repository
       '(:absolute "home" "pjb" "src" "lse-repository" :wild-inferiors)))
  ;;   '(:absolute "usr" "local" "share" "lse" :wild-inferiors)))
  (SETF (LOGICAL-PATHNAME-TRANSLATIONS "LSE")
        `(("**;*"     ,(MAKE-PATHNAME :DIRECTORY dir :NAME :WILD))
          ("**;*.*"   ,(MAKE-PATHNAME :DIRECTORY dir :NAME :WILD :TYPE :WILD))
          ("**;*.*.*" ,(MAKE-PATHNAME :DIRECTORY dir :NAME :WILD :TYPE :WILD 
                                      :VERSION :WILD)))))

(defparameter +LSE-REP-ADMIN+    "LSE:ADMIN;")
(defparameter +LSE-REP-PROGR+    "LSE:PROGR;")
(defparameter +LSE-REP-PERMA+    "LSE:PERMA;")
(defparameter +LSE-REP-TEMPO+    "LSE:TEMPO;")
(defparameter +LSE-REP-RUBAN+    "LSE:RUBAN;")

(defparameter +LSE-FIC-COMPTE+   "LSE:ADMIN;COMPT")
(defparameter +LSE-FIC-PROGR+    "LSE:ADMIN:PROGR")
(defparameter +LSE-FIC-PERMA+    "LSE:ADMIN:PERMA")


(defparameter +DEFAULT-ACCOUNT+ 99)

(defparameter +PASSWORD+    "MAYER")

(defparameter +HEADER+      "EMULSE :  L.S.E.  [ EMULATION MITRA-15 ]")
(defparameter +COPYRIGHT+   "COPYRIGHT 1984 - 2004 PASCAL BOURGUIGNON")


(map nil (function ensure-directories-exist)
     (list +LSE-REP-ADMIN+  +LSE-REP-PROGR+  +LSE-REP-PERMA+ 
           +LSE-REP-TEMPO+  +LSE-REP-RUBAN+))


;;;; configuration.lisp               --                     --          ;;;;
