;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               manifest.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Check the licenses of the dependencies, write a manifest.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-14 <PJB> Extracted from generate-unix-cli.lisp
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

(defpackage "COM.INFORMATIMAGO.MANIFEST"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY")
  (:export "ASDF-SYSTEM-NAME"
           "ASDF-SYSTEM-LICENSE"
           "SYSTEM-DEPENDS-ON"
           "SYSTEM-DEPENDS-ON/RECURSIVE"
           "DISTRIBUTION"
           "SYSTEM-RELEASE"
           "EXECUTABLE-NAME"
           "EXECUTABLE-FILENAME"
           "DATE"
           "WRITE-MANIFEST"))
(in-package "COM.INFORMATIMAGO.MANIFEST")


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
   (compute-closure 
    (function system-depends-on)
    (list (string-downcase system)))
   :test 'string=))



;; kuiper          Linux kuiper 2.6.38-gentoo-r6-pjb-c9 #2 SMP Wed Jul 13 00:23:08 CEST 2011 x86_64 Intel(R) Core(TM) i7 CPU 950 @ 3.07GHz GenuineIntel GNU/Linux
;; hubble          Linux hubble 2.6.34-gentoo-r1-d3 #5 SMP PREEMPT Mon Sep 6 13:17:41 CEST 2010 i686 QEMU Virtual CPU version 0.13.0 GenuineIntel GNU/Linux
;; voyager         Linux voyager.informatimago.com 2.6.18-6-k7 #1 SMP Mon Oct 13 16:52:47 UTC 2008 i686 GNU/Linux
;; galatea         Darwin galatea.lan.informatimago.com 11.3.0 Darwin Kernel Version 11.3.0: Thu Jan 12 18:48:32 PST 2012; root:xnu-1699.24.23~1/RELEASE_I386 i386
;; neuron          Darwin neuron.intergruas.com 9.8.0 Darwin Kernel Version 9.8.0: Wed Jul 15 16:55:01 PDT 2009; root:xnu-1228.15.4~1/RELEASE_I386 i386

(defun distribution ()
  "RETURN: (system distrib release)
System and distrib are keywords, release is a string."
  (values
   (let ((path (format nil "distribution-~8,36r.txt" (random (expt 2 32)))))
     (unwind-protect
          (if (zerop (asdf:run-shell-command (format nil "distribution > ~S" path)))
              (with-open-file (file path)
                (let ((*package* (find-package "KEYWORD")))
                  (list (read file) (read file) (read-line file))))
              #+(and ccl windows-target)
              '(:cygwin :unknown "1.7.11,0.260,5,3")
              #-(and ccl windows-target)
              (list :unknown :unknown :unknown))
       (ignore-errors (delete-file path))))))



(defun system-release ()
  (values
   (let ((path (format nil "/tmp/uname-~8,36r.txt" (random (expt 2 32)))))
     (if (zerop (asdf:run-shell-command (format nil "uname -r > ~S" path)))
         (with-open-file (file path) (read-line file))
         "unknown"))))


(defun executable-name (base)
  (format nil "~A-~A-~A-~A"
          base
          (or (cdr (assoc (lisp-implementation-type)
                          '(("Clozure Common Lisp" . "ccl")
                            ("CLISP"               . "clisp")
                            ("CMU Common Lisp"     . "cmucl")
                            ("SBCL"                . "sbcl"))
                          :test (function string-equal)))
              "unknown")
          (format nil "~(~{~A-~A-~A~}~)" (distribution))
          #-(and)
          (progn
            #+darwin (concatenate 'string "darwin" (system-release))
            #+linux  "linux"
            #+win32  "win32"
            #-(or darwin linux win32) "unknown")
          (or (cdr (assoc (machine-type)
                          '(("Power Macintosh" . "ppc")
                            ("x86_64"          . "x86_64")
                            ("x64"             . "x86_64")
                            ("x86"             . "x86")
                            ("i686"            . "i686")
                            ("i386"            . "i686"))
                          :test (function string-equal))))))

(defun executable-filename (base)
  (format nil "~A~A" (executable-name base)
          #+(or windows win32) ".exe"
          #-(or windows win32) ""))




(defun date ()
  (multiple-value-bind (se mi ho da mo ye dow dls tz)
      (decode-universal-time (get-universal-time))
    (declare (ignore dow dls))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D ~:[+~;-~]~4,'0:D"
            ye mo da ho mi se (minusp tz) (abs (* 100 tz)))))


(defun write-manifest (program-name system)
  (let ((base   (executable-name     program-name))
        (exec   (executable-filename program-name)))
    (with-open-file (*standard-output*  (format nil "~A.manifest" base)
                                         :direction :output
                                         :if-does-not-exist :create
                                         :if-exists :supersede)
      (format t "Manifest for ~A~%~V,,,'-<~>~2%" exec
              (+ (length "Manifest for ") (length exec)))
      (let* ((entries '(date
                        lisp-implementation-type
                        lisp-implementation-version
                        machine-type
                        machine-version
                        machine-instance
                        distribution
                        system-release))
             (width (reduce 'max entries :key (lambda (x) (length (string x))))))
        (dolist (fun entries)
          (format t "~(~VA~) : ~A~%" width fun (funcall fun))))
      (terpri)
      (let* ((entries      (sort (mapcar (lambda (system)
                                           (list system (asdf-system-license system)))
                                         (system-depends-on/recursive system))
                                 'string< :key 'first))
             (system-width  (reduce 'max entries :key (lambda (x) (length (first x)))))
             (license-width (reduce 'max entries :key (lambda (x) (length (string (second x)))))))
        (format t "~:(~VA~)  ~:(~A~)~%~V,,,'-<~>  ~V,,,'-<~>~%"
                  system-width 'system "license"
                  system-width license-width)
        (loop
          :for (system license) :in entries
          :do (format t "~VA  ~A~%" system-width system license))
        (format t "~V,,,'-<~>  ~V,,,'-<~>~%" system-width license-width))
      (terpri)))
  (values))


;;;; THE END ;;;;
