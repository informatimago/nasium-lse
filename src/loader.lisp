;;****************************************************************************
;;FILE:               loader.lisp
;;LANGUAGE:           Common-Lisp
;;SYSTEM:             Common-Lisp
;;USER-INTERFACE:     NONE
;;DESCRIPTION
;;    
;;    Loads the LSE interpreter and environment emulator.
;;    
;;AUTHORS
;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;MODIFICATIONS
;;    2005-08-23 <PJB> Added this header.
;;BUGS
;;LEGAL
;;    GPL
;;    
;;    Copyright Pascal Bourguignon 2005 - 2005
;;    
;;    This program is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU General Public License
;;    as published by the Free Software Foundation; either version
;;    2 of the License, or (at your option) any later version.
;;    
;;    This program is distributed in the hope that it will be
;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;    PURPOSE.  See the GNU General Public License for more details.
;;    
;;    You should have received a copy of the GNU General Public
;;    License along with this program; if not, write to the Free
;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;    Boston, MA 02111-1307 USA
;;****************************************************************************


;; ----------------------------------------
;; Load utilities:
;; ----------------------------------------

(in-package "COMMON-LISP-USER")
;;(asdf:operate 'asdf:load-op :com.informatimago.common-lisp)
(asdf:operate 'asdf:load-op :com.informatimago.clisp)
;;(asdf:operate 'asdf:load-op :com.informatimago.susv3)
(use-package  :com.informatimago.common-lisp.package)
(asdf:operate 'asdf:load-op :split-sequence)


;; (push  #P"/home/pjb/src/pjb/projects/cl-lse/ucw-0.3.9/libs/arnesi/arnesi.asd" ASDF:*CENTRAL-REGISTRY*)
;; (asdf:operate 'asdf:load-op :arnesi)

;; ----------------------------------------
;; Load Zebu:
;; ----------------------------------------

(in-package "COMMON-LISP-USER")
(load "PACKAGES:COM;HP;ZEBU")
(PACKAGE:ADD-NICKNAME "ZEBU" "COM.HP.ZEBU")

;;(asdf:oos 'asdf:load-op :zebu)
;;(package:add-nickname "ZEBU" "COM.HP.ZEBU")


;; ----------------------------------------
;; Patch Zebu:
;; ----------------------------------------

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


;; ----------------------------------------
;; Define the LSE package
;; ----------------------------------------

(in-package "COMMON-LISP-USER")
(DEFPACKAGE "COM.INFORMATIMAGO.LSE"
  (:nicknames "LSE")
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE"
        "COM.HP.ZEBU" ;; Done by ZEBU anyways.

        "COM.INFORMATIMAGO.COMMON-LISP.LIST"
        "COM.INFORMATIMAGO.COMMON-LISP.PMATCH"
        "COM.INFORMATIMAGO.COMMON-LISP.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.STRING"
        "COM.INFORMATIMAGO.CLISP.XTERM"
       ))

(IN-PACKAGE "COM.INFORMATIMAGO.LSE")


(defpackage "COM.INFORMATIMAGO.LSE.BYTE-CODE"
  (:nicknames "BC")
  (:use))

(defpackage "COM.INFORMATIMAGO.LSE.IDENTIFIERS"
  (:nicknames "ID")
  (:use))

(defun snoc (list item) (nconc list (cons item nil)))




(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *grammar-directory*
    (make-pathname :directory '(:relative "GRAMMARS") :case :common)
    "The directory where the grammars files are stored.
Used by the DEFINE-GRAMMAR macro in command.lisp"))

(ensure-directories-exist (make-pathname :name "TEST" :type "TXT" :case :common
                                         :defaults *grammar-directory*))


;; Compile the LSE grammar:
(LET ((COM.HP.ZEBU::*WARN-CONFLICTS*  T)
      (COM.HP.ZEBU::*ALLOW-CONFLICTS* T))
  (COM.HP.ZEBU::ZEBU-COMPILE-FILE
   (make-pathname :name "PARSER-LSE" :type "ZB" :case :common)
   :VERBOSE nil))

;; Load the grammar:
(zebu:delete-grammar "LSE")
(zebu-load-file
 (make-pathname :name "PARSER-LSE" :type "TAB" :case :common))
;; (load (compile-file "lse-domain.lisp")) ;; done by zebu-load-file


(dolist (item '(
                configuration
                io
                error

                task

                catalog
                functions
                scanner-lse
                compiler

                commands
                
                ;server
                ))
  (loop :until (restart-case
                   (load (make-pathname
                                 :name (string-downcase (string item))
                                 :type "lisp"))
                 (reload () nil))))
       

 

(SETF *TASK* (MAKE-TASK))

(defun parse-lse (path)
  (let ((zebu:*current-grammar*
         (or (find-grammar "LSE")
             (error "JE TROUVE PAS MA GRAMMAIRE LSE"))))
    (with-open-file (src path :direction :input)
      (loop for line = (read-line src nil nil)
         while line
         do (print (read-parser line))))))
    

(defun parse-lse-file (path)
  (with-open-file (src path)
    (let ((scanner (make-instance 'scanner :source src))
          (first-time t))
      (lr-parse
       (lambda ()
         (if first-time
             (setf first-time nil)
             (advance-token))
         (let ((token (token scanner)))
           (values token (token-kind token)))))
      (lambda (message)
        (format *standard-error*
          "~&~A:~D:~D: ~A~%" path (line scanner) (column scanner) message)
        (error "~A~%" message))
      (find-grammar "LSE"))))
      

(defun clean ()
  (let ((*default-pathname-defaults* (load-time-value *load-pathname*)))
    (mapcar 'delete-file
            (sort (delete-duplicates
                   (append "lse-domain.*"
                           "parser-lse.tab"
                           (directory "**/*.fas")
                           (directory "**/*.lib")
                           (directory "grammars/*.*")))
                  'string<= :key 'namestring))))

#||
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(load  "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PMATCH")

(directory "/home/pjb/src/pjb/projects/lse/EXEMP/*.lse")
(dolist (src  (directory "/home/pjb/src/pjb/projects/lse/BOURG/*.LSE"))
  (print src)
  (test-scan-file src))

(test-scan-file "SYNTERR.LSE")


(dolist (src  (directory "/home/pjb/src/pjb/projects/lse/BOURG/*.LSE"))
  (print src)
  (test-parse-file src))

(test-parse-file "SYNTERR.LSE")


(compile-line "10 LIRE A,B;C_A*2+B*B/3;TABLEAU T[2];T[1]_A;T[2]_B;AFFICHER C,T;TERMINER")

(compile-line "10 LIRE A,B;SI A=B ALORS C_A*2+B*B/3 SINON DEBUT TABLEAU T[2];T[1]_A;T[2]_B;AFFICHER [/,20X,F3.2]T FIN;AFFICHER A,B;TERMINER")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
||#
