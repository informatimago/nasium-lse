;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-server.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This scripts generates a unix cli lse executable.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-23 <PJB> Created.
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

(in-package "COMMON-LISP-USER")
(defparameter *program-name*    "lse-server")
(defparameter *program-system*  :com.informatimago.lse.server)
(defparameter *program-main*    (lambda () (intern "MAIN" "COM.INFORMATIMAGO.LSE.SERVER")))
(defparameter *program-features*
  '(:lse-server
    ;; :lse-scanner-debug    
    ;; :debugging
    :lse-allow-lisp      ; gives access to low level lisp command and functions. 
    :lse-case-insensitive 
    :lse-unix             
    :lse-extensions       
    #-(and) :lse-mitra-15
    #-(and) :lse-t1600
    ))

(load "generate-program.lisp")

;;;; THE END ;;;;
