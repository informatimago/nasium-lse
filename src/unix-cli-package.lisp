;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               unix-cli-package.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This defines the unix-cli package.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-23 <PJB> Created.
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


(in-package "COMMON-LISP-USER")

(defpackage "COM.INFORMATIMAGO.LSE.UNIX-TERMINAL"
  (:use "COMMON-LISP"
        "TRIVIAL-GRAY-STREAMS"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.RDP"
        "COM.INFORMATIMAGO.SIGNAL"
        "COM.INFORMATIMAGO.LSE.OS"
        "COM.INFORMATIMAGO.LSE")

  (:export "UNIX-TERMINAL"
           "TERMINAL-MODERN-MODE" "TERMINAL-CR-AS-XOFF"
           "TERMINAL-VINTR" "TERMINAL-VQUIT" "TERMINAL-VSUSP"
           "TERMINAL-VKILL" "TERMINAL-VEOF" "TERMINAL-VEOL"
           "TERMINAL-VEOL2" "TERMINAL-VERASE" "TERMINAL-VWERASE"
           "TERMINAL-VREPRINT" "TERMINAL-VSTART" "TERMINAL-VSTOP"

           ))


(defpackage "COM.INFORMATIMAGO.LSE.UNIX-CLI"
  (:nicknames "LSE-CLI")
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.UNIX.OPTION"
        "COM.INFORMATIMAGO.LSE.OS"
        "COM.INFORMATIMAGO.LSE"
        "COM.INFORMATIMAGO.LSE.UNIX-TERMINAL")
  (:export "MAIN"))


;;;; THE END ;;;;
