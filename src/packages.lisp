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

(defpackage "COM.INFORMATIMAGO.LSE.OS"
  (:use "COMMON-LISP")
  (:export
   "*EXTERNAL-FORMAT/ISO-8859-1*" "*EXTERNAL-FORMAT/UTF-8*"
   "GETENV" "GETUID"
   "RUN-SHELL-COMMAND"
   "MAKE-FD-STREAM" "FD-STREAM-P" "FD-STREAM-FD"
   "CHDIR" "GETCWD"
   "SHELL" "RUN-PROGRAM" "QUIT" "ARGUMENTS" "PROGRAM-NAME"
   "UNAME" "STTY")
  (:documentation "A portability layer for a few OS operators."))

(defpackage "COM.INFORMATIMAGO.LSE.BYTE-CODE"
  (:nicknames "BC")
  (:use)
  (:documentation "The package where the byte-code symbols for the LSE VM are interned."))

(defpackage "COM.INFORMATIMAGO.LSE.IDENTIFIERS"
  (:nicknames "ID")
  (:use)
  (:documentation "The package where the identifiers for the LSE language are interned."))

(defpackage "COM.INFORMATIMAGO.LSE"
  (:nicknames "LSE")
  (:use "COMMON-LISP"
        "BABEL"

        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.SEQUENCE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ARRAY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.CHARACTER"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STREAM"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.ASCII"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PMATCH"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.PEEK-STREAM"

        "COM.INFORMATIMAGO.COMMON-LISP.PARSER.SCANNER"

        "COM.INFORMATIMAGO.ENVIRONMENT"
        "COM.INFORMATIMAGO.SIGNAL"
        "COM.INFORMATIMAGO.RDP"

        "COM.INFORMATIMAGO.LSE.OS")
  (:shadowing-import-from "SPLIT-SEQUENCE"
                          "SPLIT-SEQUENCE" "SPLIT-SEQUENCE-IF")
  (:shadow "DOCUMENTATION")
  (:export
   "LSE"
   "LSE-ERROR"

   "DONNEE-LSE" "PARSE-DONNEE-LSE"

   "DEFCHAPTER"

   "GRAMMAR-ALL-NON-TERMINALS"
   "GRAMMAR-ALL-TERMINALS"
   "GRAMMAR-NAME"
   "GRAMMAR-NAMED"
   "GRAMMAR-RULES"
   "GRAMMAR-SKIP-SPACES"
   "GRAMMAR-START"
   "GRAMMAR-TERMINALS"
   "CLEAN-UP-RULE"

   "TERMINAL" "TERMINAL-INITIALIZE" "TERMINAL-FINALIZE"
   "TERMINAL-COLUMNS" "TERMINAL-ROWS" "TERMINAL-INPUT-STREAM"
   "TERMINAL-ECHO"
   "TERMINAL-KEYSYM-LABEL" "TERMINAL-KEYSYM-CHARACTER" "TERMINAL-CHARACTER-KEYSYM"
   "TERMINAL-OUTPUT-STREAM" "TERMINAL-RING-BELL"
   "TERMINAL-CARRIAGE-RETURN" "TERMINAL-LINE-FEED" "TERMINAL-NEW-LINE"
   "TERMINAL-WRITE-STRING" "TERMINAL-FINISH-OUTPUT"
   "TERMINAL-YIELD"
   "TERMINAL-GET-NEXT-CHAR"
   "TERMINAL-SKIP-CHARACTERS" "TERMINAL-READ-BUFFERED-CHARACTER"

   "STANDARD-TERMINAL"
   "INPUT-STREAM" "OUTPUT-STREAM"
   "WITH-TEMPORARY-ECHO"

   "WITH-PAGER"
   "OUTPUT-SUBSTITUTE"
   "IO-TERMINAL-OUTPUT-P" "IO-TERMINAL-INPUT-P"
   "IO-TAPE-OUTPUT-P" "IO-TAPE-INPUT-P" "IO-BELL"
   "IO-CARRIAGE-RETURN" "IO-LINE-FEED"
   "IO-NEW-LINE" "IO-FINISH-OUTPUT"
   "IO-READ-LINE" "IO-READ-STRING" "IO-READ-NUMBER"
   "IO-ECHO" "IO-FORMAT"
   "*DECTECH-LEFTWARDS-ARROW*"
   "*DECTECH-UPWARDS-ARROW*"
   "*UNICODE-LEFTWARDS-ARROW*"
   "*UNICODE-UPWARDS-ARROW*"
   "*UNICODE-HALFWIDTH-LEFTWARDS-ARROW*"
   "*UNICODE-HALFWIDTH-UPWARDS-ARROW*"

   "*TASK*" "TASK"
   "TASK-TERMINAL"
   "TASK-UNICODE" "TASK-ARROWS"
   "TASK-CASE-INSENSITIVE" "TASK-UPCASE-OUTPUT" "TASK-ACCENTED-OUTPUT"
   "TASK-ALLOW-BELL-OUTPUT"
   "TASK-SCRIPT-PATH" "TASK-SCRIPT-ARGUMENTS" "TASK-SCRIPT-DEBUG"
   "TASK-INTERRUPTION" "TASK-SIGNAL" "TASK-INTERRUPT"
   "TASK-VM"
   "TASK-CLOSE-ALL-FILES"

   "COMMAND-REPL" "COMMAND-RUN-SCRIPT" "COMMAND-STATUS"
   "COMMAND-EVAL-LINE"

   "VERSION" "LONG-VERSION" "*VERSION*" "*COPYRIGHT*" "*TITLE-BANNER*" "VERSIONS"

   "DAT"
   "PRET"

   "*CURRENT-DIRECTORY*" "*CURRENT-SHELF*" "*LSE-ROOT*"

   "SLOG"

   "LSE-FILE-ERROR" "FILE-ERROR-FILE" "FILE-ERROR-RECORD-NUMBER"
   "FILE-ERROR-RECORD-ALREADY-EXISTS" "FILE-ERROR-FILE-DOES-NOT-EXIST"
   "FILE-ERROR-FILE-IS-INACCESSIBLE" "FILE-ERROR-FILE-ALREADY-EXISTS"
   "LSE-DATA-FILE-OPEN" "LSE-DATA-FILE-CLOSE" "LSE-DATA-FILE-OPEN-P"
   "READ-RECORD" "WRITE-RECORD" "DELETE-RECORD"
   "*MAX-RECORD-CHAINE-SIZE*"
   "*MAX-RECORD-TABLEAU-SIZE*"
   "*MAX-RECORD-VECTEUR-SIZE*"

   "INDEFINI" "INDEFINIP"
   "NOMBRE" "NOMBREP"
   "VECTEUR" "VECTEURP"
   "TABLEAU" "TABLEAUP"
   "CHAINE" "CHAINEP")

  (:documentation "The LSE language and system implementation."))

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
           "TERMINAL-VREPRINT" "TERMINAL-VSTART" "TERMINAL-VSTOP"))


;;; THE END ;;;;

