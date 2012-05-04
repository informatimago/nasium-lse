;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               cli.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Command Line Interface.
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.LSE.CLI")

(defvar *default-program-name* "lse")

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


(defparameter *cli-banner* "
Ce programme est livré avec ABSOLUMENT AUCUNE GARANTIE; pour plus de
détails utilisez la commande DO GARANTIE.  Ce logiciel est libre, et
vous avez le droit de le redistribuer sous certaines conditions;
utilisez la commande DO LICENSE pour plus de détails.

Tapez AI pour avoir de l'aide.

BONJOUR     ~8A

")





(defun locale-terminal-encoding ()
  "Returns the terminal encoding specified by the locale(7)."
  #+(and ccl windows-target)
  :iso-8859-1
  ;; ccl doesn't support :windows-1252.
  ;; (intern (format nil "WINDOWS-~A" (#_GetACP)) "KEYWORD")
  #-(and ccl windows-target)
  (dolist (var '("LC_ALL" "LC_CTYPE" "LANG")
               :iso-8859-1) ; some random default…
    (let* ((val (getenv var))
           (dot (position #\. val))
           (at  (position #\@ val :start (or dot (length val)))))
      (when (and dot (< dot (1- (length val))))
        (return (intern (let ((name (string-upcase (subseq val (1+ dot)
                                                           (or at (length val))))))
                          (if (and (prefixp "ISO" name) (not (prefixp "ISO-" name)))
                              (concatenate 'string "ISO-" (subseq name 3))
                              name))
                        "KEYWORD"))))))


(defun set-terminal-encoding (encoding)
  #-(and ccl (not swank)) (declare (ignore encoding))
  #+(and ccl (not swank))
  (mapc (lambda (stream)
          (setf (ccl::stream-external-format stream)
                (ccl:make-external-format :domain nil
                                          :character-encoding encoding
                                          :line-termination
                                          #+unix :unix
                                          #+windows :windows
                                          #-(or unix windows) :unix)))
        (list (two-way-stream-input-stream  *terminal-io*)
              (two-way-stream-output-stream *terminal-io*)))
  (values))


(defun main (&optional args)
  (push #P "/usr/local/lib/" cffi:*foreign-library-directories*)
  (setf *program-name* (or (program-name) *default-program-name*))
  (setf *options* (make-default-options))
  (let ((encoding (locale-terminal-encoding)))
    (set-terminal-encoding encoding)
    (or (parse-options (or args (arguments)))
        (let* ((terminal-class (progn
                                 #+swank
                                 (cond
                                   ((typep (stream-output-stream *terminal-io*)
                                           'swank-backend::slime-output-stream)
                                    'swank-terminal)
                                   ((member (getenv "TERM") '("emacs" "dumb")
                                            :test (function string=))
                                    'standard-terminal)
                                   (t #+unix 'unix-terminal
                                      #-unix 'standard-terminal))
                                 #-swank
                                 (cond
                                   ((member (getenv "TERM") '("emacs" "dumb")
                                            :test (function string=))
                                    'standard-terminal)
                                   (t #+unix 'unix-terminal
                                      #-unix 'standard-terminal))))
               (terminal (make-instance terminal-class
                             :input-stream  (stream-input-stream  *terminal-io*)
                             :output-stream (stream-output-stream *terminal-io*)))
               (task     (make-instance 'task
                             :state :active
                             :case-insensitive t
                             :upcase-output nil
                             :unicode (eql encoding :utf-8)
                             :arrows  (if (eql encoding :utf-8)
                                          :unicode-halfwidth
                                          nil) 
                             :terminal terminal)))
          (setf *task* task) ; to help debugging, we keep the task in the global binding.
          (apply-options *options* *task*)
          (terminal-initialize terminal)
          (unwind-protect
               (let* ((old-debugger-hook *debugger-hook*)
                      (*debugger-hook*
                       (lambda (condition debugger-hook)
                         ;; We shouldn't come here.
                         (when debugger-hook
                           (terminal-finalize terminal))
                         (opt-format *debug-io* "~%My advice: exit after debugging.~%")
                         (when old-debugger-hook
                           (funcall old-debugger-hook condition debugger-hook)))))
                 (io-format *task* "~A" *tape-banner*)
                 (io-format *task* "~?" *title-banner* (list (version)))
                 (io-format *task* "~?" *cli-banner*   (list (subseq (dat) 9)))
                 (command-repl *task*))
            (task-close-all-files *task*)
            (terminal-finalize terminal))
          ex-ok))))



;;;; THE END ;;;;
