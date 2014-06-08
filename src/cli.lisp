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
                                          (if (boolean-enval "LSE_TELNET" nil)
                                              :windows
                                              (or
                                               #+unix :unix
                                               #+windows :windows
                                               #-(or unix windows) :unix)))))
        (list (two-way-stream-input-stream  *terminal-io*)
              (two-way-stream-output-stream *terminal-io*)))
  (values))


(defun set-lse-root (&optional (root (getenv "LSE_ROOT")))
  (when root
    (setf *lse-root* (truename (pathname root))
          *current-directory* *lse-root*
          *current-shelf*     *lse-root*
          *default-pathname-defaults* *lse-root*)))


(defun main (&optional args)
  (handler-case 
      (progn
        (push #P "/usr/local/lib/" cffi:*foreign-library-directories*)
        (setf *program-name* (or (program-name) *default-program-name*))
        (setf *options* (make-default-options))
        (set-lse-root)
        (let ((encoding (locale-terminal-encoding)))
          (set-terminal-encoding encoding)
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
                                                       :ascii) 
                                          :terminal terminal))
                 #-(and) (*trace-output* (make-broadcast-stream)))
            (setf *task* task) ; to help debugging, we keep the task in the global binding.
            (or (parse-options (or args (arguments)) nil nil nil)
                ;; parse-option may call show-bindings which calls
                ;; apply-options, so we need  *task*.
                (progn
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
                         (with-pager *task*  
                           (io-format *task* "~A" *tape-banner*)
                           (io-format *task* "~?" *title-banner* (list (version) *copyright*))
                           (io-format *task* "~?" *cli-banner*   (list (subseq (dat) 9))))
                         (command-repl *task*))
		    (format *error-output* "~&~A~%" 'step-1)
		    (finish-output *error-output*)
                    (task-close-all-files *task*)
		    (format *error-output* "~&~A~%" 'step-2)
		    (finish-output *error-output*)
                    (terminal-finalize terminal)
		    (format *error-output* "~&~A~%" 'step-3)
		    (format *error-output* "~&~{~A~%~}" (bt:all-threads))
		    (finish-output *error-output*)
		    ))
                ex-ok))))
    (error (err)
      (format *error-output* "~&~A~%" err)
      (finish-output *error-output*)
      ex-software)))



(defun test-repl (task)
  (let ((*task* task)
        (*print-case* :upcase))
    (pret task)
    (io-format task "________________________________________")
    (io-carriage-return task)
    (io-format task "Hello 1: ")

    (io-line-feed task 2)
    (io-carriage-return task)

    (io-format task "________________________________________")
    (io-carriage-return task)
    (io-format task "Hello 2: ")
    
    (let ((line (terminal-read-string (task-terminal task))))
      (io-format task "Lu 1: ~S~%" line))

    (io-format task "~%")
    
    (io-format task "________________________________________")
    (io-carriage-return task)
    (io-format task "Hello 3: ")

    (io-line-feed task 2)
    (io-carriage-return task)

    (io-format task "________________________________________")
    (io-carriage-return task)
    (io-format task "Hello 4: ")

    ;; (let ((line (terminal-read-string (task-terminal task))))
    ;;   (io-format task "Lu 21: ~S~%" line))
    
    (let ((line (io-read-line task :beep t)))
      (io-format task "Lu 22: ~S~%" line))
    ))

(defun test (&optional args)
  (push #P"/usr/local/lib/" cffi:*foreign-library-directories*)
  (setf *program-name* (or (program-name) *default-program-name*))
  (setf *options* (make-default-options))
  (set-lse-root)
  (let ((encoding (locale-terminal-encoding)))
    (set-terminal-encoding encoding)
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
                                      :ascii) 
                         :terminal terminal)))
      (setf *task* task) ; to help debugging, we keep the task in the global binding.
      (or (parse-options (or args (arguments)) nil nil nil)
          ;; parse-option may call show-bindings which calls
          ;; apply-options, so we need  *task*.
          (progn
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
                   (io-format *task* "Test shell~%")
                   (test-repl *task*))
              (task-close-all-files *task*)
              (terminal-finalize terminal)))
          ex-ok))))



;;;; THE END ;;;;
