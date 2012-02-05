;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               packages.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the packages for the LSE system.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Extracted from loader.lisp.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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
;;;;**************************************************************************


(in-package "COMMON-LISP-USER")


(DEFPACKAGE "COM.INFORMATIMAGO.LSE"
  (:nicknames "LSE")
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE"
        "COM.HP.ZEBU" ;; Done by ZEBU anyways.

        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PMATCH"
        
        #+clisp "COM.INFORMATIMAGO.CLISP.XTERM"
       ))


(DEFPACKAGE "COM.INFORMATIMAGO.LSE.SERVER"
  (:nicknames "LSE-SERVER")
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE"

        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PMATCH"

        #+clisp "COM.INFORMATIMAGO.CLISP.XTERM"

        "COM.INFORMATIMAGO.LSE"
        "COM.INFORMATIMAGO.LOGGER"
        "COM.INFORMATIMAGO.IOLIB.UTILS"
        "COM.INFORMATIMAGO.IOLIB.END-POINT"
        "COM.INFORMATIMAGO.IOLIB.MESSAGE"
        "COM.INFORMATIMAGO.IOLIB.SERVER"
       ))


(defpackage "COM.INFORMATIMAGO.LSE.BYTE-CODE"
  (:nicknames "BC")
  (:use))


(defpackage "COM.INFORMATIMAGO.LSE.IDENTIFIERS"
  (:nicknames "ID")
  (:use))


;; (defpackage "COM.INFORMATIMAGO.LSE.SOCKET"
;;   (:nicknames "SOCK")
;;   (:use "COMMON-LISP"
;;         #+clisp "SOCKET"
;;         ;; From IOLIB:
;;         #-clisp "IOMUX"
;;         #-clisp "SOCKETS")
;;   (:export "SOCKET-ACCEPT" "SOCKET-CONNECT" "SOCKET-OPTIONS" 
;;            "SOCKET-SERVER" "SOCKET-SERVER-CLOSE" "SOCKET-SERVER-HOST" 
;;            "SOCKET-SERVER-PORT" "SOCKET-SERVICE-PORT" "SOCKET-STATUS" 
;;            "SOCKET-STREAM-HOST" "SOCKET-STREAM-LOCAL" 
;;            "SOCKET-STREAM-PEER" "SOCKET-STREAM-PORT" 
;;            "SOCKET-STREAM-SHUTDOWN" "SOCKET-WAIT" "STREAM-HANDLES"))

;;; THE END ;;;;

