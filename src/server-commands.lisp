;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               server-commands.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines the server commands.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Extracted from server.lisp.
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

(in-package "COM.INFORMATIMAGO.LSE.SERVER")

(declaim (ftype (function ())
                server-start-listening
                server-stop-listening
                server-stop-log)
         (ftype (function (t))
                server-start-log
                server-kill-console-class
                server-kill-console
                server-console
                server-console-list
                server-create-xterm-console-on-display))


(defstruct console
  (class 'xterm      :type (member xterm socket))
  (number 0          :type (integer 0 99))

  (state  :limbo     :type (member :limbo :configuration :sleeping :awake))
  (date   "00/00/00" :type string))


;; console numbers: from 00 to 99.
;; socket consoles: from 00 to (- 99 max-number).
;; xterm  consoles: from max-number to 99.
;; ==> 0<=max-number<=89, we reserve 10 xterm consoles.


(defparameter *prompts*
  '((:limbo         . "EMULSE LIMBO> ")
    (:configuration . "EMULSE CONF> ")
    (:sleeping      . "EMULSE SOMMEIL> ")
    (:awake         . "EMULSE> ")))

(defmethod console-prompt ((console console))
  (cdr (assoc (console-state console) *prompts*)))





(defstruct configuration
  (max-number           0   :type (integer 0 89))
  (connection-enabled   nil :type boolean)
  (connection-log       nil) ;; nil, or path of log file, or :standard-output
  (filters              ()  :type list) ; remote IP address filter.
  (file                 nil) ;; nil, or path of configuration file.
  (statements           ()  :type list)) ; configuration expressions.


(defun configuration-add-statement (configuration statement)
  (setf (configuration-statements configuration) 
        (nconc (configuration-statements configuration)  (list statement))))


(defparameter *configuration* (make-configuration)
  "The current EMULSE SYSTEM configuration")




(defparameter *command* nil
  "The current configuration command (sexp).")

(defun store ()
  "Store the current *command* into the current *configuration*."
  (configuration-add-statement *configuration* *command*))



(defparameter *commands* '()
  "A list of couples (pattern command-function).")


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-command-function (pattern body)
    `(lambda (bindings)
       (let ((*command* ',pattern)
             ,@(mapcar (lambda (var) `(,var (cdr (assoc ',var bindings))))
                       (collect-variables pattern)))
         ,@body))))



(defmacro defcommand (pattern &body body)
  ;; Note: we keep the *commands* in the order of defcommands.
  `(let ((command (find ',pattern *commands* 
                        :key (function car)
                        :test (function equal))))
     (if command
         (setf (cdr command)   ,(make-command-function pattern body))
         (setf *commands* (nconc *commands* (list (list ',pattern
                                                        ,(make-command-function pattern body))))))))


(defun parse-command (command)
  "Parse the COMMAND expressions and calls the corresponding command function."
  (match-case* command *commands*))



;; (defvar *debugging* nil)
;; 
;; 
;; (defun configuration-repl (&key (debugging *debugging*))
;;   (catch :configuration-repl-exit
;;     (loop
;;      (format t "~&~A " *prompt*) (finish-output)
;;      (let ((sexp (read *standard-input* nil +eof+)))
;;        (if sexp
;;          (if debugging
;;            (parse-command sexp)
;;            (handler-case (parse-command sexp)
;;              (error (err)
;;                     (apply (function format) *error-output*
;;                            (simple-condition-format-control err)
;;                            (simple-condition-format-arguments err)))))
;;          (throw :configuration-repl-exit nil))))))
;; 
;; 
;; (defun configuration-repl-start ()
;;   (format t "~&~A " *prompt*) 
;;   (finish-output))


(defparameter +eof+ (gensym))

(defun configuration-repl-input (line)
  (let ((sexp (read-from-string line nil +eof+)))
    (unless (eq +eof+ sexp)
      (catch :configuration-repl-exit
        (handler-case (parse-command sexp)
          (error (err)
            (princ err *error-output*)))))))




;;----------------------------------------------------------------------
;; Configuration Commands
;;----------------------------------------------------------------------


(defcommand (connections max-number (?x n))
    (unless (and (integerp n) (<= 0 n 89))
      (error "Invalid maximum number of connection: ~S" n))
  (setf (configuration-max-number *configuration*) n)
  (store))


    
(defcommand (connections enable)
  (unless (configuration-connection-enabled *configuration*)
    (server-start-listening))
  (setf (configuration-connection-enabled *configuration*) t)
  (store))


(defcommand (connections disable)
  (when (configuration-connection-enabled *configuration*)
    (server-stop-listening))
  (setf (configuration-connection-enabled *configuration*) nil)
  (store))


(defcommand (connection log none)
  (server-stop-log)
  (store))


(defcommand (connection log (?? (?x file)))
  (if file
    (let (stream)
      (unless (and (stringp file)
                   (ensure-directories-exist file)
                   (setf stream (open file :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :append)))
        (error "Cannot open the log file ~S" file))
      (server-start-log stream)
      (store))
    (progn (server-start-log *trace-output*)
           (store))))


(defcommand (filter (?x cmd) (?x dir) all)
  (unless (member cmd '(insert append delete))
    (error "Invalid filter command: ~S (expected one of: insert append delete)" 
           cmd))
  (unless (member dir '(allow deny))
    (error "Invalid filter direction: ~S (expected one of: allow deny)" dir))
  (case cmd
    ((insert) 
     (push (list dir 'all) (configuration-filters *configuration*)))
    ((append) 
     (setf (configuration-filters *configuration*)
           (nconc (configuration-filters *configuration*)
                  (list (list dir 'all)))))
    ((delete)
     (setf (configuration-filters *configuration*)
           (delete (list dir 'all)  (configuration-filters *configuration*)
                   :test (function equal)))))
  (store))


(defcommand (filter (?x cmd) (?x dir) (?x ip) (?? (?x bits)))
  (setf bits (or bits 32))
  (unless (member cmd '(insert append delete))
    (error "Invalid filter command: ~S (expected one of: insert append delete)" 
           cmd))
  (unless (member dir '(allow deny))
    (error "Invalid filter direction: ~S (expected one of: allow deny)" dir))
  (unless (ipv4-address-p ip)
    (error "Invalid IPv4 address: ~S (expected a string like: \"192.168.0.1\")"
           ip))
  (unless (and (integerp bits) (<= 0 bits 32))
    (error
     "Invalid network mask bits: ~S (expected an integer between 0 and 32)" 
     bits))
  (case cmd
    ((insert) 
     (push (list dir ip bits) (configuration-filters *configuration*)))
    ((append) 
     (setf (configuration-filters *configuration*)
           (nconc (configuration-filters *configuration*)
                  (list (list dir ip bits)))))
    ((delete)
     (setf (configuration-filters *configuration*)
           (delete (list dir ip bits)  (configuration-filters *configuration*)
                   :test (function equal)))))
  (store))


(defcommand (filter flush)
  (setf (configuration-filters *configuration*) nil)
  (store))


(defcommand (filter list (?? (?x dir)))
  (setf dir (or dir 'all))
  (unless (member dir '(all deny allow))
    (error "Invalid filter direction: ~S (expected one of: all allow deny)" 
           dir))
  (dolist (filter (configuration-filters *configuration*))
    (when (or (eq dir 'all) (eq dir (first filter)))
      (print filter)))
  (terpri))


(defcommand (console kill all (?? (?x cclass)))
  (setf cclass (or cclass 'all))
  (unless (member cclass '(all xterm socket))
    (error "Invalid console class: ~S (expected one of: all xterm socket)" 
           cclass))
  (server-kill-console-class cclass)
  (store))


(defcommand (console kill (?x n))
  (unless (and (integerp n) (<= 0 n 99))
    (error "Invalid console number: ~S (expected an integer between 0 and 99)"
           n))
  (unless (server-console n)
    (error "There is no console ~D active." n))
  (server-kill-console n))


(defun print-console (console &optional (stream t))
  (format stream "Console ~2,'0D  ~(~12A~)  ~12A ~12A~%"
          (console-number console)
          (console-class console)
          (console-state console)
          (console-date console)))


(defcommand (console list (?? (?x cclass)))
  (setf cclass (or cclass 'all))
  (unless (member cclass '(all xterm socket))
    (error "Invalid console class: ~S (expected one of: all xterm socket)" 
           cclass))
  (dolist (console (server-console-list cclass))
    (when (or (eq 'all cclass) (eq cclass (console-class console)))
      (print-console console))))


(defcommand (console create xterm (?? display (?x display)))
  (setf display (or display ":0.0"))
  (let ((console (server-create-xterm-console-on-display display)))
    (when console
      (print-console console)
      (store))))


(defcommand (configuration save (?? (?x file)))
  (setf file (or file (configuration-file *configuration*)))
  (print `(saving  to ,file))
  (if file
      (progn
        (ensure-directories-exist file)
        (with-open-stream (stream (open file :direction :output 
                                        :if-does-not-exist :create
                                        :if-exists :supersede))
          
          (dolist (statement (configuration-statements *configuration*))
            (print statement stream)))
        (setf (configuration-file *configuration*) file))
      (dolist (statement (configuration-statements *configuration*))
        (print statement))))


(defcommand (configuration load (?? (?x file)))
  (setf file (or file (configuration-file *configuration*)))
  (print `(loading from ,file))
;;  (server-repl)
  (unless file
    (error "Please specify a configuration file."))
  (setf *configuration*
        (let ((*configuration* (make-configuration))
              (*read-eval* nil)
              (+eof+ (gensym)))
          (declare (special *configuration*))
          (with-open-file (stream file :direction :input
                                  :if-does-not-exist :error)
            (loop for sexp = (read stream nil +eof+ )
                  until (eq sexp +eof+)
                  do (parse-command sexp)))
          *configuration*)))


(defcommand (configuration print)
  (parse-command '(configuration save)))


(defmacro handling-errors (&body body)
  `(handler-case (progn ,@body)
     (error (err)
            (apply (function format) *error-output*
                   (simple-condition-format-control err)
                   (simple-condition-format-arguments err)))))


(defun server-repl ()
  (do ((hist 1 (1+ hist))
       (+eof+ (gensym)))
      (nil)
    (format t "~%~A[~D]> " (package-name *package*) hist)
    (handling-errors
     (setf +++ ++   ++ +   + -   - (read *standard-input* nil +eof+))
     (when (or (eq - +eof+)
               (member - '((quit)(exit)(continue)) :test (function equal)))
       (return-from server-repl))
     (setf /// //   // /   / (multiple-value-list (eval -)))
     (setf *** **   ** *   * (first /))
     (format t "~& --> ~{~S~^ ;~%     ~}~%" /))))


(defcommand (repl)
  (server-repl))


(defcommand (help)
    (format t "~(~:{~A~%~}~)" *commands*))



(defcommand (quit)
    (setf (console-state (client-console *client*)) :limbo)
  (throw :configuration-repl-exit nil))
  


;;;; THE END ;;;;


