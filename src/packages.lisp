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


(DEFPACKAGE "COM.INFORMATIMAGO.LSE"
  (:nicknames "LSE")
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE" "BABEL"

        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PMATCH"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM"
        
        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER"

        "COM.INFORMATIMAGO.SIGNAL"
        "COM.INFORMATIMAGO.RDP")
  (:export 

           "TERMINAL" "TERMINAL-INITIALIZE" "TERMINAL-FINALIZE"
           "TERMINAL-COLUMNS" "TERMINAL-ROWS" "TERMINAL-INPUT-STREAM"
           "TERMINAL-OUTPUT-STREAM" "TERMINAL-RING-BELL"
           "TERMINAL-CARRIAGE-RETURN" "TERMINAL-LINE-FEED"
           "TERMINAL-MOVE-UP"
           "TERMINAL-NEW-LINE"
           "TERMINAL-WRITE-STRING" "TERMINAL-FINISH-OUTPUT"
           "TERMINAL-READ-LINE" "TERMINAL-READ" "TERMINAL-ECHO"

           "STANDARD-TERMINAL"

           "IO-TERMINAL-OUTPUT-P" "IO-TERMINAL-INPUT-P"
           "IO-TAPE-OUTPUT-P" "IO-TAPE-INPUT-P" "IO-BELL"
           "IO-MOVE-UP" "IO-CARRIAGE-RETURN"
           "IO-NEW-LINE" "IO-FINISH-OUTPUT"
           "IO-READ-LINE" "IO-READ" "IO-READ-STRING" "IO-READ-NUMBER"
           "IO-ECHO" "IO-FORMAT"

           "*TASK*" "TASK" "TASK-CLOSE-ALL-FILES"
           "COMMAND-REPL"

           "*VERSION*"

           "DAT"
           ))



(defpackage "COM.INFORMATIMAGO.LSE.BYTE-CODE"
  (:nicknames "BC")
  (:use))


(defpackage "COM.INFORMATIMAGO.LSE.IDENTIFIERS"
  (:nicknames "ID")
  (:use))


;;; THE END ;;;;

