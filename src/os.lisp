;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               os.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file contains some OS function that are implementation dependant.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-02 <PJB> Created.
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

(in-package "COM.INFORMATIMAGO.LSE.OS")

(defun getenv (var)
  #+ccl           (ccl::getenv var)
  #+clisp         (ext:getenv var)
  #+CMU           (cdr (assoc var ext:*environment-list* :test #'string=))
  #+ecl           (ext:getenv var)
  #+SBCL          (sb-ext:posix-getenv var)
  #+Allegro       (sys:getenv var)
  #+Lispworks     (lispworks:environment-variable var)
  #-(or ccl
        clisp
        cmu
        ecl
        sbcl
        allegro
        lispworks) (iolib.syscalls:getenv var))


(progn
  #+ecl #.(cl:and (cl:< ext:+ecl-version-number+ 100601)
                  '(ffi:clines "#include <sys/types.h>" "#include <unistd.h>"))
  (defun getuid ()
    #+ccl                     (ccl::getuid)

    #+(and clisp #.(cl:with-standard-io-syntax
                     (cl:if (cl:some (cl:lambda (com.informatimago.lse.os::s)
                                       (cl:ignore-errors (cl:read-from-string
                                                          com.informatimago.lse.os::s)) )
                                     '("posix:uid" "LINUX:getuid"))
                            '(:and) '(:or))))
    #.(with-standard-io-syntax
        (some (lambda (s) (ignore-errors (read-from-string s)))
              '("posix:uid" "LINUX:getuid")))
     #+(and (or cmu scl) unix) (unix:unix-getuid)
     #+ecl #.(cl:if (cl:< ext:+ecl-version-number+ 100601)
                    '(ffi:c-inline () () :int "getuid()" :one-liner t)
                    '(ext::getuid))
     #+(and sbcl unix)         (sb-unix:unix-getuid)
     #+allegro                 (excl.osi:getuid)
     #-(or ccl
           (and clisp #.(cl:with-standard-io-syntax
                          (cl:if (cl:some (cl:lambda (com.informatimago.lse.os::s)
                                            (cl:ignore-errors (cl:read-from-string
                                                               com.informatimago.lse.os::s)) )
                                          '("posix:uid" "LINUX:getuid"))
                                 '(:and) '(:or))))
           (and (or cmu scl) unix)
           ecl
           (and sbcl unix)
           allegro)
     (let ((uid-string
            (with-output-to-string (*verbose-out*)
              (run-shell-command "id -ur"))))
       (with-input-from-string (stream uid-string)
         (read-line stream)
         (handler-case (parse-integer (read-line stream))
           (error () (error "Unable to find out user ID")))))))



(defvar *verbose-out* nil)

(defun run-shell-command (control-string &rest arguments)
  "Interpolate ARGS into CONTROL-STRING as if by FORMAT, and
synchronously execute the result using a Bourne-compatible shell, with
output to *VERBOSE-OUT*.  Returns the shell's exit code."
  #-ccl (asdf:run-shell-command control-string arguments)
  #+ccl
  (let ((command (apply #'format nil control-string arguments)))
    ;; (asdf-message "; $ ~A~%" command)
    (nth-value 1
               (ccl:external-process-status
                (ccl:run-program #+windows-target "C:/cygwin/bin/sh"
                                 #-windows-target "/bin/sh"
                                 (list "-c" command)
                                 :input nil :output *verbose-out*
                                 :wait t)))))




;;;-----------------------------------------------------------
;;; FD streams
;;;-----------------------------------------------------------

(defun make-fd-stream (fd &key (input t) (output nil)
                       (element-type 'character)
                       (external-format :default)
                       (output-buffering) name pathname (timeout nil))
  "Creates a new STREAM from the given POSIX file descriptor."
  #+ccl   (declare (ignore pathname name))
  #+clisp (declare (ignore name pathname timeout))
  #+cmu   (declare (ignore external-format))
  (check-type output-buffering (member nil :none :line :full))
  (assert (or input output) () "At least one of INPUT or OUTPUT must be true.")
  (let ((output-buffering (or output-buffering :none)))
    #+ccl  (declare (ignorable output-buffering))
    
    #+ccl  (let ((stream (ccl::make-fd-stream
                          fd
                          :direction (cond
                                       ((and input output) :io)
                                       (input              :input)
                                       (output             :output))
                          :element-type element-type
                          :basic t
                          :sharing :lock
                          :encoding          (etypecase external-format
                                               (CCL:EXTERNAL-FORMAT
                                                (ccl:external-format-character-encoding
                                                 external-format))
                                               (list
                                                (first external-format))
                                               (t
                                                (if (eql :default external-format)
                                                    :iso-8859-1
                                                    external-format)))
                          :line-termination (etypecase external-format
                                              (CCL:EXTERNAL-FORMAT
                                               (ccl:external-format-line-termination
                                                external-format))
                                              (list
                                               (second external-format))
                                              (t
                                               (if (eql :default external-format)
                                                   :unix
                                                   external-format)))
                          :input-timeout timeout
                          :output-timeout timeout
                          )))
             (setf (ccl::stream-external-format stream) external-format)
             stream)
    
    #+clisp (ext:make-stream fd
                             :direction (cond
                                          ((and input output) :io)
                                          (input              :input)
                                          (output             :output))
                             :element-type element-type
                             :external-format external-format
                             :buffered output-buffering)
    
    #+cmu   (system:make-fd-stream fd
                                   :input input
                                   :output output
                                   :element-type element-type
                                   :buffering output-buffering
                                   :name name
                                   :pathname pathname
                                   :timeout timeout)

    #+sbcl  (SB-SYS:MAKE-FD-STREAM fd
                                   :input input
                                   :output output
                                   :element-type element-type
                                   :buffering output-buffering
                                   :timeout timeout
                                   :name name
                                   :file pathname)
    #-(or ccl clisp cmu sbcl)
    (error "~S is not implemented for ~A ~A"
           'make-fd-stream
           (lisp-implementation-type)
           (lisp-implementation-version))))


(defun fd-stream-p (object)
  "Predicate indicating whether OBJECT is a stream with an underlying
POSIX file descriptor.
If not implemented, return NIL, so that no stream is a fd-stream.
"
  (or #+ccl   (not (not (or (ccl::stream-device object :input)
                            (ccl::stream-device object :output))))
      #+clisp (values (ignore-errors (and (ext:stream-handles object) t)))
      #+cmu   (system:fd-stream-p object)
      #+ecl   (file-stream-p object)
      #+sbcl  (sb-sys:fd-stream-p object)))


(defun fd-stream-fd (stream)
  "Returns the POSIX file descriptors under the STREAM.
PRE: (fd-stream-p stream)"
  (or #+(or ccl clisp)
      (flet ((in-out-fd (input-fd output-fd)
               (cond
                 ((null input-fd)  output-fd)
                 ((null output-fd) input-fd)
                 ((= input-fd output-fd) input-fd)
                 (t (list input-fd output-fd)))))
        (or #+ccl   (in-out-fd (ccl::stream-device stream :input)
                               (ccl::stream-device stream :output))
            #+clisp (multiple-value-bind (input-fd output-fd)
                        (ext:stream-handles stream)
                      (in-out-fd input-fd output-fd))))
      #+cmu   (system:fd-stream-fd stream)
      #+ecl   (ext:file-stream-fd  stream)
      #+sbcl  (sb-sys:fd-stream-fd stream)))


;;;; THE END ;;;;
