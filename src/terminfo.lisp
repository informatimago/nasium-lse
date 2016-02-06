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
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
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

;;;---------------------------------------------------------------------
;; (in-package "COM.INFORMATIMAGO.LSE.CLI")
(in-package "COMMON-LISP-USER")



;; (untrace cffi::%load-foreign-library
;;        cffi::load-foreign-library-if-gnu-ld-script
;;        cffi::load-foreign-library-path
;;        cffi::find-file)


(pushnew #P"/usr/lib/" cffi:*foreign-library-directories*
         :test  (function equalp))
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
;;         (getenv "TERM")
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


