;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               terminfo.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-26 <PJB> Created.
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


;;;---------------------------------------------------------------------
;;; A patch to CFFI
;;;---------------------------------------------------------------------

(in-package "CFFI")

(defun load-foreign-library-if-gnu-ld-script (libname libpath error)
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
        (cffi::%load-foreign-library libname newpath)
        (error error))))


(defun load-foreign-library-path (name path &optional search-path)
  "Tries to load PATH using %LOAD-FOREIGN-LIBRARY which should try and
find it using the OS's usual methods. If that fails we try to find it
ourselves."
  (handler-case
      (values (handler-case (%load-foreign-library name path)
                (error (err)
                  (LOAD-FOREIGN-LIBRARY-IF-GNU-LD-SCRIPT name path err)))
              (pathname path))
    (error (error)
      (if-let (file (find-file path (append search-path
                                            *foreign-library-directories*)))
              (handler-case
                  (values (handler-case (%load-foreign-library name (native-namestring file))
                            (error (err)
                              (LOAD-FOREIGN-LIBRARY-IF-GNU-LD-SCRIPT name (native-namestring file) err)))
                           file)
                (simple-error (error)
                  (report-simple-error name error)))
              (report-simple-error name error)))))


;;;---------------------------------------------------------------------
;; (in-package "COM.INFORMATIMAGO.LSE.UNIX-CLI")
(in-package "COMMON-LISP-USER")



;; (untrace cffi::%load-foreign-library
;;        cffi::load-foreign-library-if-gnu-ld-script
;;        cffi::load-foreign-library-path
;;        cffi::find-file)


(cffi:define-foreign-library curses (:unix "libncurses.so"))
(cffi:use-foreign-library curses)

(cffi:defcfun ("setupterm" :library curses)
    :int "Initialize terminfo."
    (term (:pointer :char))
    (fildes :int)
    (errret (:pointer :int)))

(cffi:defcfun ("tigetflag" :library curses) :int             "Get flag."   (capname :string))
(cffi:defcfun ("tigetnum"  :library curses) :int             "Get num."    (capname :string))
(cffi:defcfun ("tigetstr"  :library curses) (:pointer :char) "Get string." (capname :string))

(cffi:defcvar ("boolnames" :library curses :read-only t)
    :pointer)


;; (cffi:foreign-string-to-lisp
;;  (cffi:mem-aref *boolnames* ':pointer 0))
;; 
;; 
;;               char *boolnames[], *boolcodes[], *boolfnames[]
;; 
;;               char *numnames[], *numcodes[], *numfnames[]
;; 
;;               char *strnames[], *strcodes[], *strfnames[]




;; (setupterm (cffi:null-pointer) 1 (cffi:null-pointer))
;; 
;; (print (list
;;         (iolib.syscalls:getenv "TERM")
;;         (tigetflag "eo")
;;         (tigetnum  "cols")
;;         (tigetnum  "lines")))
;; 
;; (let* ((capname "cub1")
;;        (pointer (tigetstr capname))
;;        (address (cffi:pointer-address pointer)))
;;   (case address
;;     (-1 (error "~S is not a string capability." capname))
;;     (0  (error "~S is absent from the terminal description" capname))
;;     (otherwise (cffi:foreign-string-to-lisp pointer))))


