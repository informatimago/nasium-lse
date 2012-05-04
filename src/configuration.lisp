;;;; -*- mode:lisp; coding:utf-8 -*-
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
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2005
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
;;;;****************************************************************************

(in-package "COM.INFORMATIMAGO.LSE")


(eval-when (:compile-toplevel :load-toplevel :execute)
 (let ((dir '(:relative "lse-repository" :wild-inferiors)))
   ;;   '(:absolute "usr" "local" "share" "lse" :wild-inferiors)))
   (setf (logical-pathname-translations "LSE") nil)
   (setf (logical-pathname-translations "LSE")
         `(("LSE:**;*"     ,(make-pathname :defaults (user-homedir-pathname)
                                           :directory dir :name :wild))
           ("LSE:**;*.*"   ,(make-pathname :defaults (user-homedir-pathname)
                                           :directory dir :name :wild :type :wild))
           ("LSE:**;*.*.*" ,(make-pathname :defaults (user-homedir-pathname)
                                           :directory dir :name :wild :type :wild 
                                           :version :wild))))))

(defparameter *lse-rep-admin*    #p"LSE:ADMIN;")
(defparameter *lse-rep-progr*    #p"LSE:PROGR;")
(defparameter *lse-rep-perma*    #p"LSE:PERMA;")
(defparameter *lse-rep-tempo*    #p"LSE:TEMPO;")
(defparameter *lse-rep-ruban*    #p"LSE:RUBAN;")

(defparameter *lse-fic-compte*   #p"LSE:ADMIN;COMPT")
(defparameter *lse-fic-progr*    #p"LSE:ADMIN;PROGR")
(defparameter *lse-fic-perma*    #p"LSE:ADMIN;PERMA")


(defparameter *default-account* 99)

(defparameter *password*    "MAYER")

(defparameter *header*      "EMULSE :  L.S.E.  [ EMULATION MITRA-15 ]")
(defparameter *copyright*   "COPYRIGHT 1984 - 2012 PASCAL BOURGUIGNON")


;; (map nil (function ensure-directories-exist)
;;      (list *lse-rep-admin*  *lse-rep-progr*  *lse-rep-perma* 
;;            *lse-rep-tempo*  *lse-rep-ruban*))



;; (map nil 'print (directory #P"LSE:**;*.*"))




;;;; THE END ;;;;
