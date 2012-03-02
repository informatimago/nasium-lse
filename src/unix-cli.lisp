;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               unix-cli.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Unix Command Line Interface.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-15 <PJB> Created
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

(in-package "COM.INFORMATIMAGO.LSE.UNIX-CLI")

(defparameter *tape-banner* "
------------------------------------------------------------------------ 
\\     ooo                oooooooooo       oooooooooo                    \\      
 \\    ooo               oooooooooooo      oooooooooo      ooooooooooo    \\
  \\   ooo               oooo     ooo      ooo                             \\
   \\  ooo                oooo             oooooo               o oo        \\
    > ooo                  oooo           oooooo          ooo o    o        >
   /......................................................................./        
  /   ooo              ooo     oooo       ooo             oooo oo oo      /         
 /    oooooooooo  ooo  oooooooooooo  ooo  oooooooooo  ooo oooo  oo oo    / 
/     oooooooooo  ooo   oooooooooo   ooo  oooooooooo  ooo  o oooo oo    / 
------------------------------------------------------------------------ 
")

(defparameter *unix-banner* "

L.S.E.
VERSION ~A-UNIX
COPYRIGHT 1984 - 2012 PASCAL BOURGUIGNON

DISTRIBUE SELON LES TERMES DE LA LICENCE AGPLv3.

Ce programme est livré avec ABSOLUMENT AUCUNE GARANTIE; pour plus de
détails utilisez la commande DO GARANTIE.  Ce logiciel est libre, et
vous êtes les bienvenus pour redistribuer sous certaines conditions;
utilisez la commande DO COPIE pour plus de détails.

Tapez AI pour avoir de l'aide.


BONJOUR     ~8A

")





(defun locale-terminal-encoding ()
  "Returns the terminal encoding specified by the locale(7)."
  (dolist (var '("LC_ALL" "LC_MESSAGES" "LC_CTYPE")
               :iso-8859-1) ; some random default…
    (let* ((val (getenv var))
           (dot (position #\. val)))
      (when (and dot (< dot (1- (length val))))
        (return (intern (string-upcase (subseq val (1+ dot))) "KEYWORD"))))))


(defun set-terminal-encoding (encoding)
  #-(and ccl (not swank)) (declare (ignore encoding))
  #+(and ccl (not swank))
  (mapc (lambda (stream)
          (setf (ccl::stream-external-format stream)
                (ccl:make-external-format :domain nil
                                          :character-encoding encoding
                                          ;; :line-termination line-termination
                                          )))
        (list (two-way-stream-input-stream  *terminal-io*)
              (two-way-stream-output-stream *terminal-io*)))
  (values))


;; (setf ccl:*default-external-format*           :unix
;;       ccl:*default-file-character-encoding*   :utf-8
;;       ccl:*default-line-termination*          :unix
;;       ccl:*default-socket-character-encoding* :utf-8)



(defgeneric stream-input-stream (stream)
  (:method ((stream stream))
    stream)
  (:method ((stream concatenated-stream))
    (stream-input-stream (first (concatenated-stream-streams stream))))
  (:method ((stream echo-stream))
    (stream-input-stream (echo-stream-input-stream stream)))
  (:method ((stream synonym-stream))
    (stream-input-stream (symbol-value (synonym-stream-symbol stream))))
  (:method ((stream two-way-stream))
    (stream-input-stream (two-way-stream-input-stream stream))))

(defgeneric stream-output-stream (stream)
  (:method ((stream stream))
    stream)
  (:method ((stream broadcast-stream))
    (stream-output-stream (first (broadcast-stream-streams stream))))
  (:method ((stream echo-stream))
    (stream-input-stream (echo-stream-output-stream stream)))
  (:method ((stream synonym-stream))
    (stream-input-stream (symbol-value (synonym-stream-symbol stream))))
  (:method ((stream two-way-stream))
    (stream-input-stream (two-way-stream-output-stream stream))))



(defun main (&optional args)
  (declare (ignore args))
  (let ((encoding (locale-terminal-encoding)))
    (set-terminal-encoding encoding)
    (let* ((terminal (make-instance
                         (progn
                           #+swank (if (typep (stream-output-stream *terminal-io*)
                                              'swank-backend::slime-output-stream)
                                       'swank-terminal
                                       'terminfo-terminal)
                           #-swank 'terminfo-terminal)
                         #-swank :terminfo #-swank (terminfo:set-terminal)
                         :input  (stream-input-stream  *terminal-io*)
                         :output (stream-output-stream *terminal-io*)))
           (task     (make-instance 'task
                         :state :active
                         :case-insensitive t
                         :upcase-output nil
                         :dectech nil
                         :unicode #+swank (eql encoding :utf-8) #-swank nil
                         :terminal terminal)))
      (setf *task* task)
      (terminal-initialize terminal)
      (unwind-protect
           (progn
             (io-format *task* "~A" *tape-banner*)
             (io-format *task* "~?" *unix-banner*  (list *version* (subseq (dat) 9)))
             (command-repl *task*))
        (task-close-all-files *task*)
        (terminal-finalize terminal))))
  0)


;;;; THE END ;;;;
