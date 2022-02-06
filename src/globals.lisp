;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               globals.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    EMULSE : L.S.E. [ EMULATION MITRA-15 ]
;;;;
;;;;    An emulator of the CII MITRA-15 L.S.E. System
;;;;    and programming language interpreter.
;;;;
;;;;    This file defines the global variables of the LSE system.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2022-02-05 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2022 - 2022
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
;;;;****************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LSE")


(defparameter *task-count*  0   "Fixed number of tasks.")
(defparameter *tasks*       '() "The list of tasks.")
(defparameter *task-mutex*  nil "pthread_mutex_t")
(defparameter *task*        nil "The current task")

(defvar *vm* nil "Current LSE-VM.")

(defvar *debug-vm* nil)
;; (setf *debug-vm* '(:cop))
;; (setf *debug-vm* '(:error))
;; (setf *debug-vm* nil)

