;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               loader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Loads the LSE interpreter and environment emulator.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-23 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2005
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;****************************************************************************


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
;; Define the LSE package
;; ----------------------------------------
(IN-PACKAGE "COM.INFORMATIMAGO.LSE")

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
