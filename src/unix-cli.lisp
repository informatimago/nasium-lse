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
DISTRIBUE SELON LES TERMES DE LA LICENCE GPL 3.

BONJOUR     ~8A

")






(defun getenv (var)
  (iolib.syscalls:getenv var))

(defun unicode-terminal-p ()
  (let ((ctype (getenv "LC_CTYPE")))
    (not (null (search  ".UTF-8" ctype)))))


(defun main (&optional args)
  (declare (ignore args))
  (let* ((terminal (make-instance
                       #+swank
                       (if (typep *standard-output*
                                  'swank-backend::slime-output-stream)
                           'swank-terminal
                           'standard-terminal)
                     #-swank 'standard-terminal
                     :input *standard-input*
                     :output *standard-output*))
         (*task* (make-instance 'task
                     :state :active
                     :case-insensitive t
                     :upcase-output nil
                     :dectech nil
                     :unicode (unicode-terminal-p)
                     :terminal terminal)))
    (terminal-initialize terminal)
    (unwind-protect
         (progn
           (io-format *task* "~A" *tape-banner*)
           (io-format *task* "~?" *unix-banner*  (list *version* (subseq (dat) 9)))
           (command-repl *task*))
      (task-close-all-files *task*)
      (terminal-finalize terminal)))
  0)


;;;; THE END ;;;;
