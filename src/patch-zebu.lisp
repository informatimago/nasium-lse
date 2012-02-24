;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               patch-zebu.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Patches to the com.hp.zebu parser generator.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Extracted from loader.lisp
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

;; ----------------------------------------
;; Patch Zebu:
;; ----------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (com.informatimago.common-lisp.cesarum.package:add-nickname "ZEBU" "COM.HP.ZEBU"))


(in-package "COM.HP.ZEBU")

(setq *lexer-debug* t)

(defun delete-grammar (name)
  (setf *all-grammars* (delete (string name) *all-grammars*
                               :test (function equal)
                               :key (function car))))
(export 'delete-grammar)


(defun undef-action-error (token index expected grammar)
  (let* ((lexicon (grammar-lexicon grammar))
         (type (and index (svref lexicon index))))
    (list :syntax-error token type
          (map 'list (lambda (action)  (svref lexicon (car action))) expected))))


;; TODO: Clean-up this lr-parse. A lot of useless cruft in here...
(defun lr-parse (next-token-fn error-fn grammar
                 &optional junk-allowed last-pos-fn
                 &aux symbol-stack client-stack state-stack
                 action-table-top state-stack-top)
  (declare #+(or :MCL :ANSI-COMMON-LISP)
           (dynamic-extent symbol-stack client-stack state-stack)
           (type (or cons null) symbol-stack client-stack state-stack)
           (type grammar grammar)
           (type (function (simple-vector) (values t fixnum)) next-token-fn)
           (type (function (string) error) error-fn))
  (let ((start-state (grammar-lr-parser-start-state-index grammar))
        (production-info (grammar-production-info grammar))
        (action-table (grammar-action-table grammar))
        (goto-table (grammar-goto-table grammar))
        (client-lambdas (grammar-client-lambdas grammar))
        (end-symbol-index (grammar-end-symbol-index grammar))
        action-entry)
    (declare (fixnum end-symbol-index)
             (simple-vector action-table goto-table))
    (push start-state state-stack)
    (setf state-stack-top start-state
          action-table-top (svref action-table start-state))
    (multiple-value-bind (input-symbol-instantiation input-symbol-index)
        (funcall next-token-fn action-table-top)
      (if-debugging (say-looking-at))
      (setf action-entry (vec-bs-assoc (the fixnum input-symbol-index)
                                       action-table-top))
      (loop
         (when (null action-entry)
           (if (eq input-symbol-index end-symbol-index)
               (funcall error-fn
                        (undef-action-error input-symbol-instantiation
                                            input-symbol-index
                                            action-table-top
                                            grammar))
               (unless (and junk-allowed
                            ;; assume that EOF was seen
                            (setq action-entry 
                                  (vec-bs-assoc
                                   end-symbol-index action-table-top)))
                 (funcall error-fn
                          (undef-action-error input-symbol-instantiation
                                              input-symbol-index
                                              action-table-top
                                              grammar)))))	   
         ;; there should always be a non null action-entry !!
         (let ((ae-cdr (cdr (the cons action-entry))))
           (case (car (the cons ae-cdr))
             (:S                                   ; Shift.
               (setf state-stack-top (cadr ae-cdr) ; new-state
                     action-table-top (svref action-table state-stack-top))
               (push state-stack-top state-stack)
               (if-debugging (format t "~%Shift to ~S" state-stack-top))
               (push input-symbol-index symbol-stack)
               (push input-symbol-instantiation client-stack)
               (multiple-value-setq
                   (input-symbol-instantiation input-symbol-index)
                 (funcall next-token-fn action-table-top))
               (if-debugging (say-looking-at))
               (setf action-entry (vec-bs-assoc (the fixnum input-symbol-index)
                                                action-table-top)))
             (:R                        ; Reduce.
              (let* ((prod-index (cadr ae-cdr))
                     (p (svref production-info prod-index))
                     ;; p = <lhs-symbol-index> . <production-length>
                     (prod-lhs (car (the cons p)))
                     (prod-ln (cdr (the cons p)))
                     (client-lambda (svref client-lambdas prod-index)))
                (if-debugging (format t "~%Reduce ~S" prod-index))
                ;; optimize simple cases
                (case prod-ln
                  (0        ; Apply the client lambda and store the result.
                   (if-debugging (format t "~%; Calling ~S" client-lambda))
                   (push (funcall client-lambda) client-stack)
                   (if-debugging 
                    (let ((R (car client-stack)))
                      (format t "~%; -> ~S : ~S" R (type-of R)))))
                  (1        ; Apply the client lambda and store the result.
                   (when client-lambda
                     (if-debugging (format t "~%; Applying ~S to ~S"
                                           client-lambda (car client-stack)))
                     (setf (car client-stack)
                           (funcall client-lambda (car client-stack)))
                     (if-debugging 
                      (let ((R (car client-stack)))
                        (format t "~%; -> ~S : ~S" R (type-of R)))))
                   (setq symbol-stack (cdr symbol-stack)
                         state-stack  (cdr state-stack)
                         ))
                  (2        ; Apply the client lambda and store the result.
                   (if-debugging (format t "~%; Applying ~S to ~{ ~s~}"
                                         client-lambda (subseq client-stack 0 2)))
                   (when client-lambda
                     (let* ((arg2 (pop client-stack))
                            (R (funcall client-lambda
                                        (car client-stack)
                                        arg2)))
                       (setf (car client-stack) R)))
                   (setq symbol-stack (cddr symbol-stack)
                         state-stack  (cddr state-stack))
                   (if-debugging 
                    (let ((R (car client-stack)))
                      (format t "~%; -> ~S : ~S" R (type-of R)))))
                  (t (let (constituents)
                       (dotimes (i prod-ln) 
                         (setq symbol-stack (cdr symbol-stack)
                               state-stack  (cdr state-stack))
                         (push (pop client-stack) constituents))
                       ;; Apply the client lambda and store the result.
                       (if-debugging (format t "~%; Applying ~S to ~S"
                                             client-lambda constituents))
                       (push (apply client-lambda ; action
                                    constituents)
                             client-stack)
                       (if-debugging 
                        (let ((R (car client-stack)))
                          (format t "~%; -> ~S : ~S" R (type-of R)))))))
                (push prod-lhs symbol-stack) ; Push lhs of production.
                (let ((goto (cdr (the cons
                                   (vec-bs-assoc
                                    prod-lhs
                                    (svref goto-table (car state-stack)))))))
                  (if (null goto) 
                      (funcall error-fn (list :table-error-undefined-goto
                                              prod-lhs (car state-stack))))
                  (push goto state-stack)
                  (setf state-stack-top goto ; new-state
                        action-table-top (svref action-table state-stack-top)
                        action-entry (vec-bs-assoc
                                      (the fixnum input-symbol-index)
                                      action-table-top))
                  )))
             (:A
               ;; Accept on END symbol.
               (if-debugging (format t "~%Accepting"))
               ;; (break "Accept ~s" input-symbol-index)
               (if junk-allowed
                   (return
                     (values (car client-stack)
                             (when last-pos-fn (funcall last-pos-fn))))
                   (if (= input-symbol-index end-symbol-index)
                       (return
                         (values (car client-stack)
                                 (when last-pos-fn (funcall last-pos-fn))))
                       (funcall error-fn
                                (list (if (eq input-symbol-instantiation T)
                                          :unexpected-token
                                          :extra-input)
                                      input-symbol-index)))))
             (T (funcall error-fn (list :bogus-action (car ae-cdr))))))))))

;;;; THE END ;;;;
