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



(defun shebang-line-p (line)
  (and (stringp line)
       (< 2 (length line))
       (string= "#!" line :end2 2)))

(defun shebang-program (shebang-line)
  (let* ((start (or (position #\space shebang-line :start 2 :test (function char/=)) 2))
         (end   (position #\space shebang-line :start start)))
    (subseq shebang-line start (or end (length shebang-line)))))

(defun parse-script (path &optional interpreter-path)
  "
PATH:             POSIX path (STRING) to the script.

INTERPRETER-PATH: POSIX path (STRING) to the expected interpreter.

RETURN:           Upon success: STREAM; OPTIONS
                  otherwise: NIL; NIL.

DO:               Parse a script file at the given PATH.

        Scripts should contain:

            - the shebang line with the bare path to the 
              interpreter,

            - zero or more lines containing options or comments
              (options start with a dash, several options can be
              given on the same line separated by spaces; comment
              start with an octothorpe, till the end of line).

            - optionally empty lines or comment lines

            - a lse program.

        For example:

            #!/usr/local/bin/lse
            # comment
            --modern-mode
            1 afficher 'hello'
            2 terminer
"
  (let (stream arguments)
    (flet ((fail (reason)
             (declare (ignorable reason))
             ;; #+debugging (format *error-output* "~&Script fails for ~A~%" reason)
             (when stream (close stream))
             (return-from parse-script (values nil nil))))
      (handler-case
          (progn
            (setf stream (open path :direction :input :if-does-not-exist nil))
            (unless stream (fail 'no-file))
            (let ((shebang (read-line stream nil nil)))
              (unless (and (shebang-line-p shebang)
                           (or (null interpreter-path)
                               (string= interpreter-path (shebang-program shebang))))
                (fail (list 'bad-shebang
                            (shebang-line-p shebang)
                            (or (null interpreter-path)
                                (string= interpreter-path (shebang-program shebang)))))))
            (loop
              :with state = :newline
              :for ch = (peek-char nil stream nil nil)
              :do (case ch
                    ((#\space)
                     (setf state :inline)
                     (read-char stream))
                    ((#\newline)
                     (setf state :newline)
                     (read-char stream))
                    ((#\#)
                     (setf state :inline)
                     (read-line stream))
                    ((#\-)
                     (setf state :inline)
                     (push (coerce (loop
                                     :for ch = (read-char stream nil nil)
                                     :while (and ch (or (alphanumericp ch)
                                                        (char= #\- ch)))
                                     :collect ch
                                     :finally (when (char= #\Newline ch)
                                                (setf state :newline)))
                                   'string)
                           arguments))
                    (otherwise
                     (if (eq state :newline)
                         (loop-finish)
                         (fail (list 'bad-char ch))))))
            (values stream (nreverse arguments)))
        (error (err)
          (fail err))))))


(defun process-argument (argument remaining)
  "
DO:        This function is called by PARSE-OPTIONS when an unexpected
           option is found.

           When the command is used interactively, no non-option
           argument should be given.

           When the command is used as an interpreter for a script,
           the user should give no option on the shebang line, only --
           to prevent the lisp implementation to parse options..

           It is expected the command is invoked by the kernel by
           passing the shebang options, the (relative) path to the
           script, and the actual command line arguments.

           See PARSE-SCRIPT for the expected format for the script
           file.

           Therefore this function expects:

               [--] script-file {script-arguments}

           The script file is checked, and if invalid, an error is
           signaled.    Otherwise the script stream and script
           arguments are stored in *options*.

ARGUMENT:  The argument that is not an option.

REMAINING: A list of command line arguments remaining to be processed.

RETURN:    A list of remaining command line arguments to be parsed by
           PARSE-OPTIONS.
"
  (let* ((arguments (if (string= "--" argument)
                        remaining
                        (cons argument remaining)))
         (script-path (pop arguments)))
    (multiple-value-bind (script new-arguments) (parse-script script-path)
      (if script
          (progn
            (setf (options-script-stream    *options*) script
                  (options-script-arguments *options*) arguments)
            
            new-arguments)
          (error "arguments invalides : ~S" (cons argument remaining))))))



(defun call-with-terminal (terminal thunk)
  (terminal-initialize terminal)
  (unwind-protect
       (let* ((old-debugger-hook *debugger-hook*)
              (*debugger-hook*
                (lambda (condition debugger-hook)
                  ;; We shouldn't come here.
                  (when debugger-hook
                    (terminal-finalize terminal))
                  (opt-format *debug-io* "~%My advice: exit after debugging.~%")
                  (unwind-protect
                       (when old-debugger-hook
                         (funcall old-debugger-hook condition debugger-hook))
                    (when debugger-hook
                      (terminal-initialize terminal))))))
         (funcall thunk))
    (terminal-finalize terminal)))

(defmacro with-terminal (terminal &body body)
  `(call-with-terminal ,terminal (lambda () ,@body)))


(defun script (options task terminal)
  "
DO:     Execute the script specified in options.
RETURN: EX-OK or EX-SOFTWARE when the script fails.
"
  (apply-options options task)
  (with-terminal terminal
    (unwind-protect
         (if (command-run-script task (options-script-stream options))
             (or (command-status task) EX-OK)
             EX-SOFTWARE)
      (io-finish-output task)
      (task-close-all-files task))))


(defun interactive (options task terminal)
  "
DO:     Perform the interactive lse interactions.
RETURN: EX-OK
"
  ;; parse-options may call show-bindings which calls
  ;; apply-options, so we need  *task*.
  (apply-options options task)
  (with-terminal terminal
    (unwind-protect
         (progn
           (with-pager task  
             (io-format task "~A" *tape-banner*)
             (io-format task "~?" *title-banner* (list (long-version) *copyright*))
             (io-format task "~?" *cli-banner*   (list (subseq (dat) 9))))
           (command-repl task)
           EX-OK)
      (io-finish-output task)
      (task-close-all-files task))))


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
            (or (parse-options (or args (arguments)) nil (function process-argument) nil)
                (progn
                  (if (options-script-stream *options*)
                      (script      *options* task terminal)
                      (interactive *options* task terminal))
                  ex-ok)))))
    (error (err)
      (format *error-output* "~&ERREUR: ~A~%" err)
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
