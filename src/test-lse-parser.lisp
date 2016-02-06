;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               test-lse-parser.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Tests the LSE parser.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2016-02-06 <PJB> Added this header, used simple-test.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2016
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LSE")
(import '(com.informatimago.common-lisp.cesarum.simple-test:define-test
          com.informatimago.common-lisp.cesarum.simple-test:check
          com.informatimago.common-lisp.cesarum.simple-test:assert-true
          com.informatimago.common-lisp.cesarum.simple-test:assert-false))

(defun test-parse-stream (src)
  (let ((*scanner* (make-instance 'lse-scanner :source src)))
    (loop
      :with result := '()
      :until (eofp (scanner-current-token *scanner*))
      :do (let ((parsed-line (lse-parse-one-line *scanner*)))
            (when parsed-line
              (push parsed-line result)))
      :finally (return (nreverse result)))))

(defun test-parse-file (path)
  (with-open-file (src path)
    (test-parse-stream src)))

(defun test-parse-string (source)
  (with-input-from-string (src source)
    (test-parse-stream src)))

(define-test test/parse-stream-linums ()
  (let ((parsed (test-parse-string  "
1 afficher 'hello'
2 afficher 'world'
3 afficher 'bonjour'
4 afficher 'le monde'
5 terminer
")))
    (loop
      :for line :in parsed
      :for lino := (ignore-errors (numero-valeur (second line)))
      :for expected-lino :from 1
      :do (check eql lino expected-lino
                 (lino expected)
                 "Line number parsed: ~S~%Expected line number: ~S~%"
                 lino expected-lino))))



(defun test-compile-lse-stream (src)
  (loop
    :for line = (read-line src nil nil)
    :while line
    :do (terpri) (princ ";; |  ") (write-string line)
    :do (let ((comp (compile-lse-line line)))
          (print comp)
          (print (code-source comp))
          (print (disassemble-lse (code-vector comp))))
    :finally (terpri) (finish-output))
  (values))

(defun test-compile-lse-file (path)
  (with-open-file (src path)
    (test-compile-lse-stream src))
  (values))

(defun test-compile-lse-string (source)
  (with-input-from-string (src source)
    (test-compile-lse-stream src))
  (values))

(defun test-parse-line (source-line)
  (let* ((*scanner* (make-instance 'lse-scanner :source source-line))
         (parse-tree
           (prog1 (parse-lse *scanner*)
             (accept *scanner* 'eol))))
    (values parse-tree
            (ignore-errors (compile-lse-line-parse-tree parse-tree)))))


;; (test-parse-file #P"~/src/pjb/nasium-lse/src/scratch/test.lse")
;; (test-parse-string "100 PROCEDURE &ATTEN(DELAI) LOCAL TEMPS,I,FINI,TRAIT;CHAINE TRAIT;AFFICHER 'ATTEN'")
;; (test-parse-line  "100 PROCEDURE &ATTEN(DELAI) LOCAL TEMPS,I,FINI,TRAIT;CHAINE TRAIT;AFFICHER 'ATTEN'")

(string-tokens  "95 AFFICHER['Après la pause…',/]")

(defun parse-tree-equal (a b)
  (cond
    ((null a) (null b))
    ((atom b) (equal a b))
    ((eq 'token (first b))
     (and (typep a (second b))
          (equal (token-text a) (third b))))
    ((atom a) nil)
    (t (and (parse-tree-equal (car a) (car b))
            (parse-tree-equal (cdr a) (cdr b))))))

(define-test test/parse-simple-afficher-format ()
  (initialize-debugging-task)
  (multiple-value-bind (parse-tree code-vector)
      (test-parse-line  "95 AFFICHER['Après la pause…',/]")
    (check parse-tree-equal
           parse-tree
           '(:ligne-programme (token tok-numero "95")
                  (:afficher ((:spec-chaine (:rep-1) (token tok-litchaine "'Après la pause…'"))
                              (:spec-slash (:rep-1))))))
    (assert-true code-vector)
    (check equalp
           code-vector
           #S(code :line 95 :vector #(49 1 49 "Après la pause…" 21 49 1 22 26) :procedure nil :source nil))))


;; (compile-lse-line "95 AFFICHER['Après la pause…',/]")
;; #S(code :line 95 :vector #(49 1 49 "Après la pause…" 21 49 1 22 26) :procedure nil :source "95 AFFICHER['Après la pause…',/]")

;; (compile-lse-line "20 afficher [10/,20x,'Hello',/,20x,5'*',2/]")


;; (test/compile-lse-string "4 executer 'tfic'")
;; (4 #(50 "tfic" 50 1 35 26) "4 EXECUTER 'tfic'")

;; (test/parse-string "6 TABLEAU V[3],M[2,2]")
;; (test/compile-lse-string "6 TABLEAU V[3],M[2,2]")








;; (compile-lse-file #P "../BOURG/BOUR.LSE")
;; (compile-lse-file #P "TESTCOMP.LSE")

;; (test/compile-lse-file #P "TESTCOMP.LSE")
;; (test/compile-lse-file "tpars.lse")



(define-test test/all ()
  (test/parse-stream-linums)
  (test/parse-simple-afficher-format))

;;;; THE END ;;;;
