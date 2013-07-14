;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               zebu-parser.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    LSE Parser using Zebu.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Extracted from loader.lisp
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2013
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

(in-package "COM.INFORMATIMAGO.LSE")

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

(defun parse-lse (scanner)
  (handler-case
      (lr-parse (progn
                  (advance-line)
                  (lambda (parser-data)
                    (scan-next-token scanner parser-data)))
                (lambda (msg) (error "ERREUR: ~A" msg))
                (find-grammar "LSE"))
    (error (err)
      (format t "Parsing error ~A" err)
      (signal err))))


;;;; THE END ;;;;
