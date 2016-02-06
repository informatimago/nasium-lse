;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               patch-cffi-uffi.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Patch to CFFI and  to UFFI to load GNU LD scripts.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-03 <PJB> Extracted from terminfo.lisp
;;;;BUGS
;;;;LEGAL
;;;;    GPL2
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
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
;;;;**************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))

(defpackage "COM.INFORMATIMAGO.CLEXT.GNU-LD-SCRIPT"
  (:use "COMMON-LISP")
  (:export "FOREIGN-LIBRARY-IF-GNU-LD-SCRIPT")
  (:documentation "
This package export a function that returns the first library
specified in a GNU ld script.
"))
(in-package "COM.INFORMATIMAGO.CLEXT.GNU-LD-SCRIPT")


(defun foreign-library-if-gnu-ld-script (libpath error)
  "
LIBPATH: Pathname to the supposed GNU ld script.
ERROR:   A condition that is signaled with  ERROR if LIBPATH is not a
         GNU ld script.
RETURN:  A pathname to a library.
"
  (let ((newpath
         (with-open-file (stream libpath
                                 :direction :input
                                 :element-type 'character
                                 :external-format :default
                                 :if-does-not-exist nil)
           (when stream
             (let ((buffer (make-array 80
                                       :element-type 'character
                                       :fill-pointer 80)))
               (setf (fill-pointer buffer) (read-sequence buffer stream))
               (when (search "GNU ld script" buffer)
                 (file-position stream 0)
                 (loop
                   (let ((line (read-line stream nil nil)))
                     (cond
                       ((null line) (return nil))
                       ((and (< 5 (length line))
                             (string= "GROUP" (subseq line 0 5)))
                        (let* ((left  (position #\( line))
                               (ppos  (and left (position #\space line
                                                          :start (1+ left)
                                                          :test (function char/=))))
                               (right (and ppos (position #\space line :start ppos))))
                          (return 
                            (and right (subseq line ppos right))))))))))))))
    (if newpath
        newpath
        (error error))))


#+cffi
(in-package "CFFI")

#+cffi
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unintern 'foreign-library-if-gnu-ld-script)) ; if the patch is already there.

#+cffi
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'com.informatimago.clext.gnu-ld-script:foreign-library-if-gnu-ld-script))

#+cffi
(defun load-foreign-library-path (name path &optional search-path)
  "Tries to load PATH using %LOAD-FOREIGN-LIBRARY which should try and
find it using the OS's usual methods. If that fails we try to find it
ourselves."
  (handler-case
      (values (handler-case (%load-foreign-library name path)
                (error (err)
                  (%load-foreign-library
                   name
                   (foreign-library-if-gnu-ld-script path err))))
              (pathname path))
    (error (error)
      (if-let (file (find-file path (append search-path
                                            *foreign-library-directories*)))
              (handler-case
                  (values (handler-case (%load-foreign-library name (native-namestring file))
                            (error (err)
                              (%load-foreign-library
                               name
                               (foreign-library-if-gnu-ld-script
                                (native-namestring file)
                                err))))
                          file)
                (simple-error (error)
                  (report-simple-error name error)))
              (report-simple-error name error)))))


#+#.(cl:if (cl:find-package "UFFI")  '(:and) '(:or))
(in-package "UFFI")

#+#.(cl:if (cl:find-package "UFFI")  '(:and) '(:or))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unintern 'foreign-library-if-gnu-ld-script)) ; if the patch is already there.


#+#.(cl:if (cl:find-package "UFFI")  '(:and) '(:or))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import 'com.informatimago.clext.gnu-ld-script:foreign-library-if-gnu-ld-script))

#+#.(cl:if (cl:find-package "UFFI")  '(:and) '(:or))
(defun load-foreign-library (filename &key module supporting-libraries
                             force-load)
  (declare (ignorable module supporting-libraries))

  (flet ((load-failure ()
           (error "Unable to load foreign library \"~A\"." filename)))
    (declare (ignorable #'load-failure))
    (when (and filename (or (null (pathname-directory filename))
                            (probe-file filename)))
      (if (pathnamep filename)    ;; ensure filename is a string to check if already loaded
          (setq filename (namestring (if (null (pathname-directory filename))
                                         filename
                                         ;; lispworks treats as UNC, so use truename
                                         #+(and lispworks mswindows) (truename filename)
                                         #-(and lispworks mswindows) filename))))

      (if (and (not force-load)
               (find filename *loaded-libraries* :test #'string-equal))
          t ;; return T, but don't reload library
          (flet  ((load-foreign-library (filename)
                    (progn
                      #+cmu
                      (let ((type (pathname-type (parse-namestring filename))))
                        (if (string-equal type "so")
                            (unless
                                (sys::load-object-file filename)
                              (load-failure))
                            (alien:load-foreign filename
                                                :libraries
                                                (convert-supporting-libraries-to-string
                                                 supporting-libraries))))
                      #+scl
                      (alien:load-foreign filename
                                          :libraries
                                          (convert-supporting-libraries-to-string
                                           supporting-libraries))
                      #+sbcl
                      (handler-case (sb-alien::load-1-foreign filename)
                        (sb-int:unsupported-operator (c)
                          (if (fboundp (intern "LOAD-SHARED-OBJECT" :sb-alien))
                              (funcall (intern "LOAD-SHARED-OBJECT" :sb-alien) filename)
                              (error c))))

                      #+lispworks (fli:register-module module :real-name filename
                                                       :connection-style :immediate)
                      #+allegro (load filename)
                      #+openmcl (ccl:open-shared-library filename)
                      #+digitool (ccl:add-to-shared-library-search-path filename t)

                      (push filename *loaded-libraries*)
                      t)))
            (handler-case (load-foreign-library filename)
              (error (err)
                (load-foreign-library (foreign-library-if-gnu-ld-script filename err)))))))))

;;;; THE END ;;;;;
