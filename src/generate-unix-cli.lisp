;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               generate-unix-cli.lisp
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

(in-package "CL-USER")
(cd "/home/pjb/src/pjb/lse-cl/src/")
(pushnew (pwd) asdf:*central-registry* :test 'equal)



(setf *print-right-margin* 80
      *print-pretty* t
      *print-case* :downcase)

(pushnew :developing           *features*)
(pushnew :lse-case-insensitive *features*)
(pushnew :lse-unix             *features*)


#+ (and ccl linux) (asdf:run-shell-command "rm -rf /home/pjb/.cache/common-lisp/kuiper.lan.informatimago.com/ccl-1.7-f94-linux-amd64/home/pjb/src/git/pjb/lse-cl/src/")
#+ (and ccl darwin) (asdf:run-shell-command "rm -rf /Users/pjb/.cache/common-lisp/triton.lan.informatimago.com/ccl-1.7-f94-macosx-ppc32/home/pjb/src/git/pjb/lse-cl/src/")
#+ (and clisp linux) (asdf:run-shell-command "rm -rf /home/pjb/.cache/common-lisp/kuiper.lan.informatimago.com/clisp-2.49-unix/home/pjb/src/git/pjb/lse-cl/src/")
#+ (and clisp darwin) (asdf:run-shell-command "rm -rf /Users/pjb/.cache/common-lisp/triton.lan.informatimago.com/clisp-2.49-unix/home/pjb/src/git/pjb/lse-cl/src/")


(ql:quickload :com.informatimago.lse.unix-cli)


(defparameter *system-licenses*
  '(("cl-ppcre" . "BSD-2")
    ("split-sequence" . :unknown)
    ("terminfo" . "MIT")))

(defun asdf-system-name (system)
  (slot-value system 'asdf::name))

(defun asdf-system-license (system-name)
  (let ((system  (asdf:find-system system-name)))
    (if (slot-boundp system 'asdf::licence)
        (slot-value system 'asdf::licence)
        (or (cdr (assoc system-name *system-licenses* :test 'string-equal))
            :unknown))))

(defun system-depends-on (system)
  (delete (string-downcase system)
          (let ((system (asdf:find-system system)))
           (delete-duplicates
            (mapcar 'string-downcase
                    (mapcan (lambda (x) (copy-seq (rest x)))
                            (asdf:component-depends-on 'asdf:load-op system)))
            :test 'string=))
          :test 'string=))

(defun system-depends-on/recursive (system)
  (delete-duplicates
   (com.informatimago.common-lisp.cesarum.utility:compute-closure 
    (function system-depends-on)
    (list (string-downcase system)))
   :test 'string=))

(print (let ((system :com.informatimago.lse.unix-cli))
         (mapcar (lambda (system)
                   (cons system
                         (asdf-system-license system)))
                 (system-depends-on/recursive system))))
(terpri)
(finish-output)


#+ccl (progn (terpri) (princ "ccl:save-application will exit.") (terpri) (finish-output))
#+ccl (ccl:save-application (cond
                              ((string-equal (machine-type) "Power Macintosh") "lse-ccl-ppc")
                              ((string-equal (machine-type) "x86_64")          "lse-ccl-x86_64")
                              (t                                               "lse-ccl-x86"))
                            :toplevel-function (function com.informatimago.lse.unix-cli:main)
                            :init-file nil
                            :error-handler :quit-quitely
                            ;; :application-class ccl:lisp-development-system
                            ;; :clear-clos-cache t
                            :purify nil
                            ;; :impurify t
                            :mode #o755
                            :prepend-kernel t
                            ;; :native t
                            ) 

#+clisp (ext:saveinitmem  (cond
                            ((string-equal (machine-type) "Power Macintosh") "lse-clisp-ppc")
                            ((string-equal (machine-type) "x86_64")          "lse-clisp-x86_64")
                            (t                                               "lse-clisp-x86"))
                         :quiet t
                         :verbose t
                         :norc t
                         :init-function (lambda ()
                                          (ext:exit (handler-case
                                                        (com.informatimago.lse.unix-cli:main)
                                                      (error ()
                                                        1))))
                         :script t
                         :documentation "Système & Interpréteur L.S.E"
                         :start-package "COMMON-LISP-USER"
                         :keep-global-handlers nil
                         :executable t) 
#|
    (cd "/home/pjb/src/pjb/lse-cl/src/")
    (load "generate-unix-cli.lisp")
|#
;;;; THE END ;;;;

