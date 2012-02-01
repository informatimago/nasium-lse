;; -*- mode:     CL -*- ----------------------------------------------------- ;
;; File:         zebu-init.lisp
;; Description:  Loading Zebu and the Compiler
;; Author:       Joachim H. Laubsch
;; Created:      19-May-92
;; Modified:     Thu Jan  7 11:15:55 1999 (Joachim H. Laubsch)
;; Language:     CL
;; Package:      CL-USER
;; Status:       Experimental (Do Not Distribute) 
;; RCS $Header: $
;;
;; (c) Copyright 1992, Hewlett-Packard Company
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revisions:
;; RCS $Log: $
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; To use or compile Zebu, you must load this file first.
;; To load the Zebu Compiler, it has to be compiled.
;; To compile the Zebu Compiler load compile-zebu.lisp
;; zb:zebu to               "Load the Zebu Parser"
;; zb:zebu-compiler to      "Load the Zebu Compiler"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-(or CLISP ccl MCL) (in-package "USER")
#+(or CLISP ccl MCL) (in-package "CL-USER")

#-(or CLISP ccl MCL)
(progn
  (unless (find-package "CL-USER")
    (defpackage "USER" (:nicknames "CL-USER")))
  (in-package "CL-USER"))

(provide "zebu-init")



;; edit the following forms for your Lisp.

(defparameter *ZEBU-directory*
  #.(make-pathname
     :name nil :type nil :version nil
     :defaults
     #+mcl               (truename *loading-file-source-file*)
     #+ALLEGRO           (merge-pathnames *load-pathname*
                                          *default-pathname-defaults*)
     #-(or mcl allegro) (or *compile-file-pathname* *load-pathname*))
  "Directory where you keep Zebu sources.")


(defparameter *ZEBU-binary-directory*
  (make-pathname :name nil :type nil :version nil
                 :directory (append (pathname-directory *ZEBU-directory*)
                                    (list "binary"))
                 :defaults *ZEBU-directory*)
  "Directory for compiled grammars.")


(defparameter *load-source-pathname-types*
  #+(or (and WINDOWS ACL3.0) :HARLEQUIN-PC-LISP) '("lsp" nil)
  #-(or (and WINDOWS ACL3.0) :HARLEQUIN-PC-LISP) '("lisp" NIL))


(defparameter *load-binary-pathname-types*
  #+(or MCL Allegro)                             '("fasl")
  #+clisp                                        '("fas")
  #+(and sun lucid)                              '("sbin")
  #+(or (and WINDOWS ACL3.0) :HARLEQUIN-PC-LISP) '("fsl")
  #-(or mcl allegro clisp (and sun lucid) (and WINDOWS ACL3.0) HARLEQUIN-PC-LISP)
  (let ((path "TEST.LISP"))
    (with-open-file (src path
                         :direction :output
                         :if-does-not-exist :create
                         :if-exists :supersede)
      (print '(defvar *test* nil) src))
    (let ((object (compile-file path)))
      (delete-file object)
      (delete-file path)
      (list (pathname-type object)))))





#+clisp (ensure-directories-exist
         (make-pathname :name "test" :type "lisp"
                        :defaults  *ZEBU-binary-directory*))
#-clisp (unless (probe-file *ZEBU-binary-directory*)
          #+LUCID
          (shell (format nil "mkdir ~a" (namestring *ZEBU-binary-directory*)))
          #+ALLEGRO
          (let ((dir (format nil "~abinary"
                             (namestring *ZEBU-directory*))))
            (unless (zerop (run-shell-command
                            (format nil "mkdir ~a" dir)))
              (error "Could not create directory ~s" dir)))
          #+MCL
          (create-file *ZEBU-binary-directory* :if-exists nil)
          #+(and WINDOWS ACL3.0)
          (create-directory *ZEBU-binary-directory*)
          )

;;----------------------------------------------------------------------------;
;; ZEBU package aka ZB
;;-------------
;;

(shadowing-import '(zb::zebu zb::zebu-compiler) (find-package "CL-USER"))
(export '(zb::zebu zb::zebu-compiler) (find-package "ZB"))


;;----------------------------------------------------------------------------;
;; zb:zebu
;;--------
;; load the zebu parser runtime system
;; 
(defun zb:zebu (&key (compiled t))
  "Load the Zebu parser"
  (let ((defaults 
         (make-pathname :type (first (if compiled
                                         *load-binary-pathname-types*
                                         *load-source-pathname-types*))
                        :defaults (if compiled
                                      *load-pathname* ; *ZEBU-binary-directory*
                                      *ZEBU-directory*))))
    (dolist (name '("zebu-aux"
                    "zmg-dom"
                    "zebu-mg-hierarchy"
                    "zebu-loader"
                    "zebu-driver"
                    "zebu-actions"))
      (let ((file (make-pathname :name name :defaults defaults))) 
        (if (probe-file file)
            (require name file)
            (require name (make-pathname
                           :name name
                           :type (car *load-source-pathname-types*)
                           :defaults *ZEBU-directory*)))))
    (format t "~%;;; Zebu (Version ~a) loaded!~%" zb:*zebu-version*)
    (values)))


;;----------------------------------------------------------------------------;
;; zb:zebu-compiler
;;-----------------
;; load the Zebu Compiler
;; 
(defun zb:zebu-compiler (&key (compiled t) (verbose t))
  "Load the Zebu Compiler"
  (zb::zebu :compiled compiled)
  (push ':ZEBU *features*)
  (let ((defaults 
         (make-pathname :type (first (if compiled
                                         *load-binary-pathname-types*
                                         *load-source-pathname-types*))
                        :defaults (if compiled
                                      *load-pathname* ; *ZEBU-binary-directory*
                                      *ZEBU-directory*)))
        (*load-verbose* verbose))
    (dolist (name '("zebu-kb-domain" "zebu-regex"
                    "zebu-oset" "zebu-g-symbol" "zebu-loadgram"
                    "zebu-generator" "zebu-lr0-sets"
                    "zebu-empty-st" "zebu-first"
                    "zebu-follow" "zebu-tables"
                    "zebu-slr" "zebu-closure"
                    "zebu-lalr1" "zebu-dump" "zebu-compile"
                    "zebu-printers"))
      (unless (member name *modules* :test #'string=)
        (load (make-pathname :name name :defaults defaults)))) 
    (ZB:zebu-compile-file
     (make-pathname :name "zebu-mg" :type "zb" :defaults *ZEBU-directory*)
     :output-file (make-pathname :name "zebu-mg" :defaults *ZEBU-binary-directory*)
     :verbose verbose
     :compile-domain compiled)
    (zb::zebu-load-file
     (make-pathname :name "zebu-mg" :type "tab" :defaults *ZEBU-binary-directory*))
    (format t "~%;;; Zebu Compiler (Version ~a) loaded!~%" zb:*zebu-version*)
    (values)))


(defun zb:zebu-rr (&key (compiled t))
  "Load the rewrite-rule module"
  (zb::zebu)
  (let ((path
         (make-pathname :type (first (if compiled
                                         *load-binary-pathname-types*
                                         *load-source-pathname-types*))
                        :defaults (if compiled
                                      *load-pathname* ; *ZEBU-binary-directory*
                                      *ZEBU-directory*))))
    (dolist (name '("zebu-kb-domain" "zebu-tree-attributes"
                    "zebra-debug"))
      (unless (member name *modules* :test #'string=)
        (load (make-pathname :name name :defaults path))))
    (values)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; You may want to omit this and rather import only a subset of the 
;; symbols or use package "ZEBU" in another package than the CL-USER
;; package.

(use-package (find-package "ZEBU"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               A few Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#||
(load "zebu-init.lisp")
;; load the compiler
#+(and :MSWINDOWS :ALLEGRO) 
(zb:zebu-compiler  :compiled  nil)

#-(and :MSWINDOWS :ALLEGRO)
(zb:zebu-compiler)

(setq *ZEBU-test-directory*
(make-pathname :directory (append (pathname-directory *ZEBU-directory*)
(list "test"))))
(setq *ZEBU-test-binary-directory*
(make-pathname :directory (append (pathname-directory *ZEBU-test-directory*)
(list "binary"))))

(unless (probe-file *ZEBU-test-binary-directory*)
#+LUCID
(shell (format nil "mkdir ~a" (namestring *ZEBU-test-binary-directory*)))
#+MCL
(create-file *ZEBU-test-binary-directory* :if-exists nil)
#+(and WINDOWS ACL3.0)
(create-directory *ZEBU-test-binary-directory*)  
)

(zebu-compile-file (merge-pathnames
(make-pathname :name "ex1" :type "zb") *ZEBU-test-directory*)
:output-file
(merge-pathnames
(make-pathname :name "ex1" :type "tab")
*ZEBU-test-binary-directory*))

(zb:zebu-load-file (merge-pathnames
(make-pathname :name "ex1" :type "tab")
*ZEBU-test-binary-directory*))

(setq zebu:*current-grammar* (zb:find-grammar "ex1"))
(list-parser '(1 "+" 1))
(equal (read-parser "1 + 1") (list-parser '(1 "+" 1)))
(read-parser "1.0 * 1")
(read-parser "1.0 * 1/33")
(read-parser "1.0 * a1")
(read-parser "1.0 * .3")
(read-parser "1.0 * 12.3")

||#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             End of zebu-init.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
