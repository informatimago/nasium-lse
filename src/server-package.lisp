;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               server-package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This defines the server package.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-23 <PJB> Extracted from packages.
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


;;;; THE END ;;;;
