;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               main.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    The main LSE system program.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Extracted from loader.lisp
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(in-package "COM.INFORMATIMAGO.LSE")


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

;;;; THE END ;;;;
