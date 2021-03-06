; -*- mode:     CL -*- ----------------------------------------------------- ;
; File:         pc1-p.lisp
; Description:  
; Author:       Joachim H. Laubsch
; Created:      13-Apr-92
; Modified:     Thu Oct  2 12:57:16 1997 (Joachim H. Laubsch)
; Language:     CL
; Package:      CL-USER
; Status:       Experimental (Do Not Distribute) 
; RCS $Header: $
;
; (c) Copyright 1992, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Revisions:
; RCS $Log: $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package "CL-USER")

(defun PRINT-PROPOSITIONAL-VARIABLE (ITEM STREAM LEVEL)
  (DECLARE (IGNORE LEVEL))
  (format STREAM
	  "~S"
	  (PROPOSITIONAL-VARIABLE--name ITEM)))

(defun PRINT-BOOLEAN-OR (ITEM STREAM LEVEL)
  (DECLARE (IGNORE LEVEL))
  (format STREAM
	  "~S or ~S"
	  (BOOLEAN-EXPR--RAND1 ITEM)
	  (BOOLEAN-EXPR--RAND2 ITEM)))

(defun PRINT-BOOLEAN-AND (ITEM STREAM LEVEL)
  (DECLARE (IGNORE LEVEL))
  (format STREAM
	  "~S and ~S"
	  (BOOLEAN-EXPR--RAND1 ITEM)
	  (BOOLEAN-EXPR--RAND2 ITEM)))

(defun print-atomic-wff (item stream level)
  (declare (ignore level))
  (format stream
	  "~a(~a)"
	  (atomic-wff--predicate item)
	  (atomic-wff--role-argument-pairs item)))

(defun print-role-argument-pair (item stream level)
  (declare (ignore level))
  (format stream
	  "~a: ~a"
	  (role-argument-pair--role item)
	  (role-argument-pair--argument item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         end of pc1-p.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

