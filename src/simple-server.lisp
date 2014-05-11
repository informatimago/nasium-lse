;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               simple-server.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a simple server.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Added this header.
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

(in-package "COM.INFORMATIMAGO.LSE")

#||
(progn
;;(load "/local/html/local/arauko-pmatch.lisp")
(load "/local/html/local/arauko-server.lisp")
(server))
||#

(defun server-start-listening ())
(defun server-stop-listening ())

(defun server-start-log (log-stream))
(defun server-stop-log ())

(defun server-kill-console-class (cclass))
(defun server-kill-console       (console-num))
(defun server-console (console-num))
(defun server-console-list (cclass))

(defun REGISTER-WORKER (pid))

(defconstant +cr+    13)
(defconstant +bs+     8)
(defconstant +del+  127)

(defvar +eof+       (gensym))
(defvar *debugging* nil)


(defstruct console
  (class 'xterm      :type (member xterm socket))
  (number 0          :type (integer 0 99))
  (state  "LIMBO"    :type string)
  (date   "00/00/00" :type string))



(defun list-insert-separator (list separator)
  "
RETURN:  A list composed of all the elements in `list'
         with `separator' in-between.
EXAMPLE: (list-insert-separator '(a b (d e f)  c) 'x)
         ==> (a x b x (d e f) x c)
"
  (do ((result (if list (list (car list))))
       (list (cdr list) (cdr list)))
      ((null list) (nreverse result))
    (push separator result)
    (push (car list) result)))


(defmacro while (condition &body body) `(do () ((not ,condition)) ,@body))


(defun char-or-string-p (object)
  (or (characterp object) (stringp object)))


(defun pjb-unsplit-string (string-list &rest separator)
  "Does the inverse than pjb-split-string. If no separator is provided 
then a simple space is used."
  (cond
   ((null separator)         (setq separator " "))
   ((/= 1 (length separator)) 
    (error "pjb-unsplit-string: Too many separator arguments."))
   ((not (char-or-string-p (car separator)))
    (error "pjb-unsplit-string: separator must be a string or a char."))
   (t (setq separator (car separator))))
  (apply 'concatenate 'string
         (mapcar (lambda (object)
                   (if (stringp object) 
                     object
                     (format nil "~A" object)))
                 (list-insert-separator string-list separator))))


(defun pjb-split-string (string &optional separators)
  "
note:   current implementation only accepts as separators
        a string containing only one character.
"
  (setq separators (or separators " ")
        string (string string))
  (let ((sep (aref separators 0))
        (chunks  '())
        (position 0)
        (nextpos  0)
        (strlen   (length string)) )
    (while (<= position strlen)
      (while (and (< nextpos strlen)
                  (char/= sep (aref string nextpos)))
        (setq nextpos (1+ nextpos)))
      (setq chunks (cons (subseq string position nextpos) chunks))
      (setq position (1+ nextpos))
      (setq nextpos  position) )
    (nreverse chunks)))


(defun ipv4-address-p (address)
  "
PRE:     (or (string address) (symbol address))
RETURN:  Whether ADDRESS as the aaa.bbb.ccc.ddd IPv4 address format.
"
  (let ((bytes (pjb-split-string (string address) ".")))
    (and (= 4 (length bytes))
         (block :convert
           (nreverse
            (mapcar (lambda (byte)
                      (multiple-value-bind (val eaten) (read-from-string byte)
                        (if (and (= eaten (length byte)) (integerp val) 
                                 (<= 0 val 255))
                          val
                          (return-from :convert nil))))
                    (pjb-split-string address ".")))))))



#||


;; console numbers: from 00 to 99.
;; socket consoles: from 00 to (- 99 max-number).
;; xterm  consoles: from max-number to 99.
;; ==> 0<=max-number<=89, we reserve 10 xterm consoles.


(defparameter *prompt* "EMULSE>")


(defstruct configuration
  (max-number           0   :type (integer 0 89))
  (connection-enabled   nil :type boolean)
  (connection-log       nil) ;; nil, path of log file or :standard-output
  (filters              ()  :type list)
  (file                 nil) ;; nil, or path of configuration file.
  (statements           ()  :type list)
  );;configuration


(defun configuration-add-statement (configuration statement)
  (setf (configuration-statements configuration) 
        (nconc (configuration-statements configuration)  (list statement))))
        

(defparameter *configuration* (make-configuration)
  "The current EMULSE SYSTEM configuration")


(defparameter *commands* '())


(defmacro defcommand (pattern &body body)
  `(let ((command (find ',pattern *commands* 
                        :key (function car)
                        :test (function equal))))
     (if command
         (setf (cdr command) ',body)
         (push (cons ',pattern ',body) *commands*))))


(defmacro parse-command (command)
  (let ((cmdvar (gensym "COMMAND")))
    `(let ((,cmdvar ,command))
       (macrolet
         ((store () '(configuration-add-statement *configuration* ,cmdvar)))
         (match-case ,cmdvar
           ,@(reverse *commands*)
           (otherwise (error "Invalid command: ~S" ,cmdvar)))))));;parse-command


(defcommand (connections max-number (?x n))
  (unless (and (integerp n) (<= 0 n 89))
    (error "Invalid maximum number of connection: ~S" n))
  (setf (configuration-max-number *configuration*) n)
  (store));;connections

    
(defcommand (connections enable)
  (unless (configuration-connection-enabled *configuration*)
    (server-start-listening))
  (setf (configuration-connection-enabled *configuration*) t)
  (store));;connections


(defcommand (connections disable)
  (when (configuration-connection-enabled *configuration*)
    (server-stop-listening))
  (setf (configuration-connection-enabled *configuration*) nil)
  (store));;connections


(defcommand (connection log none)
  (server-stop-log)
  (store))


(defcommand (connection log (?? (?x FILE)))
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
           (store))));;connection


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
  (store));;filter


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
  (store));;filter


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
  (terpri));;filter


(defcommand (console kill all (?? (?x cclass)))
  (setf cclass (or cclass 'all))
  (unless (member cclass '(all xterm socket))
    (error "Invalid console class: ~S (expected one of: all xterm socket)" 
           cclass))
  (server-kill-consoles cclass)
  (store));;console


(defcommand (console kill (?x n))
  (unless (and (integerp n) (<= 0 n 99))
    (error "Invalid console number: ~S (expected an integer between 0 and 99)"
           n))
  (unless (server-console n)
    (error "There is no console ~D active." n))
  (server-kill-console n));;console


(defun print-console (console &optional (stream t))
  (format stream "Console ~2,'0D  ~[xterm ~;socket~]  ~12A ~12A~%"
          (console-number console)
          (eq 'socket (console-class console))
          (console-state console)
          (console-date console)));;print-console


(defcommand (console list (?? (?x cclass)))
  (setf cclass (or cclass 'all))
  (unless (member cclass '(all xterm socket))
    (error "Invalid console class: ~S (expected one of: all xterm socket)" 
           cclass))
  (dolist (console (server-console-list cclass))
    (when (or (eq 'all cclass) (eq cclass (console-class console)))
      (print-console console))));;console


(defcommand (console create xterm (?? display (?x display)))
  (setf display (or display ":0.0"))
  (let ((console (server-create-xterm-console-on-display display)))
    (when console
      (print-console console)
      (store))));;console


(defcommand (configuration save (?? (?x FILE)))
  (setf file (or file (configuration-file *configuration*)))
  (print `(saving  to ,file))
;;  (server-repl)
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
      (print statement))));;configuration


(defcommand (configuration load (?? (?x FILE)))
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
                  do (parse-one-command sexp)))
          *configuration*)));;configuration


(defcommand (configuration print)
  (parse-one-command '(configuration save)))



(defun server-repl ()
  (do ((hist 1 (1+ hist))
       (+eof+ (gensym)))
      (nil)
    (format t "~%~A[~D]> " (package-name *package*) hist)
    (handling-errors
     (setf +++ ++   ++ +   + -   - (read *standard-input* nil +eof+))  
     (when (or (member - '((quit)(exit)(continue)) :test (function equal)))
       (return-from server-repl))
     (setf /// //   // /   / (multiple-value-list (eval -)))
     (setf *** **   ** *   * (first /))
     (format t "~& --> ~{~S~^ ;~%     ~}~%" /))));;server-repl



(defcommand (repl)
  (server-repl))


(defcommand (help)
  (dolist (command *commands*) (print (first command))))


(defcommand (quit)
  (throw :configuration-repl-exit nil))


(defun parse-one-command (command)  
  "This must be after all the DEFCOMMAND to gather them!"
  (parse-command command))




(defun configuration-repl (&key (debugging *debugging*))
  (catch :configuration-repl-exit
    (loop
     (format t "~&~A " *prompt*) (finish-output)
     (let ((sexp (read *standard-input* nil +eof+)))
       (if sexp
         (if debugging
           (parse-one-command sexp)
           (HANDLER-CASE (parse-one-command sexp)
             (ERROR (ERR)
                    (apply (function format) *error-output*
                           (simple-condition-format-control err)
                           (simple-condition-format-arguments err)))))
         (throw :configuration-repl-exit nil))))));;configuration-repl


(defun configuration-repl-start ()
  (format t "~&~A " *prompt*) 
  (finish-output))


(defun configuration-repl-input (line)
  (let ((sexp (read-from-string line nil +eof+)))
    (unless (eq +eof+ sexp)
      (if *debugging*
        (parse-one-command sexp)
        (HANDLER-CASE (parse-one-command sexp)
          (ERROR (ERR)
                 (apply (function format) *error-output*
                        (simple-condition-format-control err)
                        (simple-condition-format-arguments err)))))
      (configuration-repl-start))));;configuration-repl-input
  

(when nil
  (load "loader.lisp")
  (configuration-repl :debugging t)
  (filter append allow "127.0.0.1")
  (filter append deny all)
  (connections max-number 40)
  (connections enable)
  (configuration save "/tmp/server.conf")
  (repl)
  )



||#



(defmacro handling-errors (&body body)
  `(HANDLER-CASE (progn ,@body)
     (ERROR (ERR)
            (apply (function format) *error-output*
                   (simple-condition-format-control err)
                   (simple-condition-format-arguments err)))))



(let ((hist 0))
  (defun repl-start ()
    (format *standard-output* "~%~A[~D]> " (package-name *package*) (incf hist))
    (finish-output *standard-output*)))


(defun repl-input (task line)
  (handling-errors
     (setf +++ ++   ++ +   + -   - (read-from-string line))
     (if (member - '((quit)(exit)) :test (function equal))
       (iotask-dequeue task)
       (progn
         (setf /// //   // /   / (multiple-value-list (eval -)))
         (setf *** **   ** *   * (first /))
         (format *standard-output* "~& --> ~{~S~^ ;~%     ~}~%" /)
         (repl-start)
         /))))

(defstruct iotask  stream process-event name)


(defparameter *iotasks*   '())
(defparameter *bon-grain* '()
  "Sublist of *iotask* which can be handled by socket:socket-wait.")
(defparameter *ivray*     '() 
  "Sublist of *iotask* which cannot be handled by socket:socket-wait.")


(defun iotask-enqueue (stream process-event &optional name)
  (let ((task (make-iotask :stream stream 
                           :process-event process-event
                           :name name)))
    (push task *iotasks*)
    (handler-case (socket:socket-status (iotask-stream task) 0)
      (error     ()                           (push task *ivray*))
      (:no-error (s n) (declare (ignore s n)) (push task *bon-grain*)))
    ));;iotask-enqueue



(defun iotask-dequeue (task)
  (setf *iotasks*   (delete task *iotasks*))
  (setf *bon-grain* (delete task *bon-grain*))
  (setf *ivray*     (delete task *ivray*)))


(defun iotask-poll-loop ()
  (loop ;; each 0.1 seconds, see second argument of socket-status.
   (when (null *iotasks*) (return))
   (map nil 
        (lambda (task status)
          (when status (funcall (iotask-process-event task) task status)))
        *ivray*
        (mapcar (lambda (task)
                  (let ((stream (iotask-stream task)))
                    (cond
                     ((input-stream-p stream)  
                      (if (listen stream)
                        :input
                        (if (output-stream-p stream) :output nil)))
                     ((output-stream-p stream) :output)
                     (t  nil))))
                *ivray*))
   (map nil
        (lambda (task status)
          (when status (funcall (iotask-process-event task) task status)))
        *bon-grain*
        (socket:socket-status
         (mapcar (function iotask-stream) *bon-grain*) 0.1))
   ));;iotask-poll-loop


(defun make-buffered-discipline (process-input)
  (lambda (task event)
    (when (member event '(:input :io :error))
      (funcall process-input task (read-line (iotask-stream task))))))


(defun make-keyboard-discipline (process-input)
  (let ((buffer (make-array '(128) :element-type 'character :fill-pointer 0)))
    (lambda (task event)
      (when (eq :input event)
        (let* ((ich (read-char (iotask-stream task)))
               (ch  (system::input-character-char ich)))
          (cond 
           ((null ch))
           ((= (char-code ch) +CR+)
            (terpri)
            (funcall process-input 
                     task (subseq buffer 0 (fill-pointer buffer)))
            (setf (fill-pointer buffer) 0))
           ((or (= (char-code ch) +BS+) (= (char-code ch) +DEL+))
            (when (< 0 (fill-pointer buffer))
              (princ (code-char +BS+))
              (princ " ")
              (princ (code-char +BS+))
              (decf (fill-pointer buffer))))
           (t
            (princ ch)
            (vector-push ch buffer))))
        (finish-output)))));;make-keyboard-discipline



(defun server-input (task line)
  (if (string-equal "(QUIT)" line)
    (iotask-dequeue task)
    (configuration-repl-input line)));;server-input


(defun make-xterm-io-stream (&key display)
  (let* ((pipe (with-open-stream (s (ext:make-pipe-input-stream
                                     "mktemp /tmp/clisp-x-io-XXXXXX"))
                 (read-line s)))
         (title "CLISP I/O")
         tty-name xio
         (clos::*warn-if-gf-already-called* nil)
         (font "-*-console-medium-r-normal-*-16-*-*-*-*-*-*-*")
         ;; "-dec-terminal-bold-r-normal-*-14-*-*-*-*-*-dec-dectech"
         )
    (ext:shell  (format nil "rm -f ~S; mknod ~S p; xterm ~:[~;~:*-display ~S~] -fg green -bg black -fn '~A' -n ~S -T ~S -e 'tty >> ~S ; cat ~S' &" 
         pipe pipe display font title title pipe pipe))
    (setq tty-name (with-open-file (s pipe :direction :input) (read-line s))
          xio (make-two-way-stream
               (open tty-name :direction :input)
               (open tty-name :direction :output)))
    (defmethod close :after ((x (eql xio)) &rest junk)
      (declare (ignore x junk))
      (with-open-file (s pipe :direction :output)
        (write-line (TEXT "Bye.") s))
      (delete-file pipe)
      (close (two-way-stream-input-stream xio))
      (close (two-way-stream-output-stream xio))
      (let ((clos::*warn-if-gf-already-called* nil))
        (remove-method #'close (find-method #'close '(:after) `((eql ,xio))))))
    xio));;make-xterm-io-stream


(defun server-main (&key display) 
  (if (or display (find-package "SWANK"))
    (let* ((xterm-io (make-xterm-io-stream :display display))
           (*standard-output* xterm-io)
           (*standard-input*  xterm-io)
           (*error-output*    xterm-io)
           (*terminal-io*     xterm-io)
           (*query-io*        xterm-io)
           (*debug-io*        xterm-io))
      (iotask-enqueue *standard-input*
                      (make-buffered-discipline (function server-input))
                      "xterm")
      (configuration-repl-start)
      (iotask-poll-loop))
    (ext:with-keyboard
     (let ((*standard-input* ext:*keyboard-input*))
       (iotask-enqueue ext:*keyboard-input* 
                       (make-keyboard-discipline (function server-input))
                       "keyboard")
       (configuration-repl-start)
       (iotask-poll-loop)))));;server-main
  

(defvar *external-format* (ext:make-encoding 
                           :charset 'charset:iso-8859-1
                           :line-terminator :unix))


(defun make-pipe ()
  "RETURN: two interconnected IPC streams.
This would be a couple of pipes, but since we don't have internal pipes
in clisp, we do it with a socket."
  (let ((lsock (socket:socket-server))
        (asock)
        (bsock))
    (setf asock (socket:socket-connect (socket:socket-server-port lsock)
                                       (socket:socket-server-host lsock)
                                       :ELEMENT-TYPE 'character
                                       :EXTERNAL-FORMAT *external-format*
                                       :BUFFERED nil
                                       :TIMEOUT 5))
    (if (socket:socket-wait lsock 5)
      (progn
        (setf bsock (socket:socket-accept lsock 
                                          :ELEMENT-TYPE 'character
                                          :EXTERNAL-FORMAT *external-format*
                                          :BUFFERED nil
                                          :TIMEOUT 5))
        (if bsock
          (values asock bsock)
          (values nil nil)))
      (values nil nil))));;make-pipe
                                       
   

(defparameter +server-port+ 15000)

(defun server ()
  (let ((lsock (socket:socket-server +server-port+)))
    (unwind-protect
        (loop
         (when (socket:socket-wait lsock 0)
           (let ((remote (socket:socket-accept lsock
                                               :element-type 'character
                                               ;; :external-format 
                                               :buffered t
                                               :timeout 1)))
             (when remote
               ;; got an incoming connection, let's fork a worker
               ;; but first, create a socket and connect to it to be
               ;; able to communicate with this worker.
               (let ((pid (linux:fork)))
                 (cond
                  ((< pid 0) ;; error
                   (error "Could not fork a worker."))
                  ((= pid 0) ;; child
                   (let ((*standard-input* remote)
                         (*standard-output* remote))
                     (iotask-enqueue *standard-input*
                                     (make-buffered-discipline
                                      (function repl-input))
                                     "remote")
                     (repl-start)
                     (iotask-poll-loop)))
                  (t ;; parent
                   (register-worker pid)
                   (format t "~&forked child ~D~%" pid))))
               ))))
      (close lsock))));;server


#||
(progn (ext:with-keyboard 
        (socket:socket-status (list ext:*keyboard-input*) nil) 
        (unread-char  (system::input-character-char 
                       (read-char ext:*keyboard-input*))
                      *standard-input*))
       (print (read-line)))
||#




;; Local Variables:
;; eval: (cl-indent 'pmatch:match-case  1
;; eval: (cl-indent 'match-case  1)
;; End:

;;;; THE END ;;;;
