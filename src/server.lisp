;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               server.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    XXX
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2005-08-24 <PJB> Added this header.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal Bourguignon 2005 - 2014
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
;;;;****************************************************************************
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LSE.SERVER")

(defvar *server* nil "For debugging we keep the current server instance in this global.")
(defvar *client* nil "Bound to the current client while running configuration or lse commands.")
(defvar *server-port* 15001 "Default port the server will listen to.")


(defvar *limbo-banner* "
EMULSE :  L.S.E.  [ EMULATION MITRA-15 ]
VERSION : ~A
~A

CONSOLE NO. ~2,'0D
"
  "A format control string taking a version and a console number arguments,
displayed at the start of the EMULSE Limbo session.")


(defvar *limbo-help*
  "
Commandes du sous-système Limbo de EMULSE :

DECONNECTER    Ferme la connexion.
LSE            Entre dans le sous-systeme L.S.E.
CONFIGURER     Configure le server EMULSE.
")



(defun current-date ()
  (multiple-value-bind (se mi ho da mo ye) (decode-universal-time (get-universal-time))
    (declare (ignore se mi ho))
    (format nil "~2,'0D/~2,'0D/~2,'0D" da mo (mod ye 100))))



(defclass lse-console-client (client)
  ((console :initarg :console :initform nil :accessor client-console)
   (task    :initarg :task    :initform nil :accessor client-task)))


(defmethod client-send-initial-message ((client lse-console-client))
  (client-send-response client "~%~?~%" *limbo-banner*
                        (list (version) *copyright* (console-number (client-console client)))))


(defmethod client-send-prompt ((client lse-console-client))
  (client-send-response client "~%~V,,,'_A~C~A" 79 "" #\Return
                        (console-prompt (client-console client))))



(defmethod client-receive-message ((client lse-console-client) message)
  (let ((text (message-text message))
        (*client* client))
    (format t "LSE received ~S (state ~s) ~%" text (console-state (client-console client)))
    (ecase (console-state (client-console client))
      (:limbo
       (cond
         ((or (string-equal text "de") (string-equal text "deconnecter"))
          (server-remove-client (client-server client) client)
          (return-from client-receive-message))
         ((or (string-equal text "ls") (string-equal text "lse"))
          (setf (console-state (client-console client)) :sleeping))
         ((or (string-equal text "co") (string-equal text "configurer"))
          (setf (console-state (client-console client)) :configuration)
          (client-send-response client "~&Tapez: help~%pour obtenir la liste des commandes de configuration.~%"))
         (t
          (client-send-response client "~%~A~%" *limbo-help*))))
      (:configuration
       (client-send-response client "~A"
                             (with-output-to-string (*standard-output*)
                               (let ((*error-output* *standard-output*)
                                     (*standard-input* (make-string-input-stream ""))
                                     (*terminal-io*    (make-two-way-stream *standard-input*
                                                                            *standard-output*))
                                     (*query-io*       *terminal-io*))
                                 (configuration-repl-input text)))))
      ((:sleeping :awake)
       (client-send-response client "~A"
                             (with-output-to-string (*standard-output*)
                               (let ((*error-output* *standard-output*)
                                     (*standard-input* (make-string-input-stream ""))
                                     (*terminal-io*    (make-two-way-stream *standard-input*
                                                                            *standard-output*))
                                     (*query-io*       *terminal-io*))
                                 (command-eval-line (client-task client) text)))))))
  (client-send-prompt client))




(defun main (&key (port *server-port*))
  (let* ((*event-base* (make-instance 'event-base))
         (encoding :ascii)) ;; TODO
    (setf *server* (start-server
                    (make-instance 'tcp-ipv4-end-point :interface #(127 0 0 1) :port port)
                    :name "test"
                    ;; :data-received-callback (lambda (data server client)
                    ;;                           (logger :data-received-callback :debug
                    ;;                                   "Received data ~S from ~A~%" data client))
                    :client-class 'lse-console-client
                    :client-connected
                    (lambda (server client client-socket)
                      (declare (ignore client-socket))
                      (logger :client-connected :info "Client connected ~A~%" client)
                      (setf (client-console client)
                            (make-console :class 'socket
                                          :number 0
                                          :date (current-date))
                            (client-task client)
                            (make-instance 'task
                                :state :sleeping
                                :case-insensitive t
                                :upcase-output nil
                                :unicode (eql encoding :utf-8)
                                :arrows (if (eql encoding :utf-8)
                                            :unicode-halfwidth
                                            :ascii)
                                :terminal (make-instance 'standard-terminal
                                              :input-stream (make-synonym-stream '*standard-input*)
                                              :output-stream (make-synonym-stream '*standard-output*))))
                      (client-send-initial-message client)
                      (client-send-prompt client))))
    (unwind-protect
         (event-loop)
      (close-server *server*)
      (setf *server* nil))))

;;----------------------------------------------------------------------


(defun server-start-listening ())
(defun server-stop-listening ())

(defun server-start-log (log-stream))
(defun server-stop-log ())

(defun server-kill-console-class (cclass))
(defun server-kill-console       (console-num))
(defun server-console (console-num))
(defun server-console-list (cclass))
(defun server-create-xterm-console-on-display (display))



;;----------------------------------------------------------------------


(defun char-or-string-p (object)
  (or (characterp object) (stringp object)))


(defun ipv4-address-p (address)
  "
PRE:     (or (string address) (symbol address))
RETURN:  Whether ADDRESS as the aaa.bbb.ccc.ddd IPv4 address format.
"
  (let ((bytes (split-string (string address) ".")))
    (and (= 4 (length bytes))
         (block :convert
           (nreverse
            (mapcar (lambda (byte)
                      (multiple-value-bind (val eaten) (read-from-string byte)
                        (if (and (= eaten (length byte)) (integerp val)
                                 (<= 0 val 255))
                          val
                          (return-from :convert nil))))
                    (split-string address ".")))))))







;; (defun make-buffered-discipline (process-input)
;;   (lambda (task event)
;;     (when (member event '(:input :error))
;;       (funcall process-input task (read-line (iotask-stream task))))))


;; (defun make-keyboard-discipline (process-input)
;;   (let ((buffer (make-array '(128) :element-type 'character :fill-pointer 0)))
;;     (lambda (task event)
;;       (when (eq :input event)
;;         (let* ((ich (read-char (iotask-stream task)))
;;                (ch  #+clisp (system::input-character-char ich)
;;                     #-clisp ich))
;;           (cond
;;            ((null ch))
;;            ((= (char-code ch) +cr+)
;;             (terpri)
;;             (funcall process-input
;;                      task (subseq buffer 0 (fill-pointer buffer)))
;;             (setf (fill-pointer buffer) 0))
;;            ((or (= (char-code ch) +bs+) (= (char-code ch) +del+))
;;             (when (< 0 (fill-pointer buffer))
;;               (princ (code-char +bs+))
;;               (princ " ")
;;               (princ (code-char +bs+))
;;               (decf (fill-pointer buffer))))
;;            (t
;;             (princ ch)
;;             (vector-push ch buffer))))
;;         (finish-output)))))



;; (defun server-input (task line)
;;   (if (string-equal "(QUIT)" line)
;;     (iotask-dequeue task)
;;     (configuration-repl-input line)))


;; (defun server-main (&key display)
;;   (if (or display (find-package "SWANK"))
;;     (let* ((xterm-io (make-xterm-io-stream :display display))
;;            (*standard-output* xterm-io)
;;            (*standard-input*  xterm-io)
;;            (*error-output*    xterm-io)
;;            (*terminal-io*     xterm-io)
;;            (*query-io*        xterm-io)
;;            (*debug-io*        xterm-io))
;;       (iotask-enqueue *standard-input*
;;                       (make-buffered-discipline (function server-input))
;;                       "xterm")
;;       (configuration-repl-start)
;;       (iotask-poll-loop))
;;     #+clisp (ext:with-keyboard
;;                 (let ((*standard-input* ext:*keyboard-input*))
;;                   (iotask-enqueue ext:*keyboard-input*
;;                                   (make-keyboard-discipline (function server-input))
;;                                   "keyboard")
;;                   (configuration-repl-start)
;;                   (iotask-poll-loop)))
;;     #-clisp (progn
;;               (iotask-enqueue *standard-input*
;;                               (make-keyboard-discipline (function server-input))
;;                               "keyboard")
;;               (configuration-repl-start)
;;               (iotask-poll-loop))))





(defun make-xterm-io-stream (&key display)
  #+clisp
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
        (write-line (text "Bye.") s))
      (delete-file pipe)
      (close (two-way-stream-input-stream xio))
      (close (two-way-stream-output-stream xio))
      (let ((clos::*warn-if-gf-already-called* nil))
        (remove-method #'close (find-method #'close '(:after) `((eql ,xio))))))
    xio)
  #-clisp
  (error "no xterm in ~A" (lisp-implementation-type)))



(defvar *external-format*
  #+clisp (ext:make-encoding
           :charset 'charset:iso-8859-1
           :line-terminator :unix)
  #-clisp :iso-8859-1)


(defun make-pipe ()
  "RETURN: two interconnected IPC streams.
This would be a couple of pipes, but since we don't have internal pipes
in clisp, we do it with a socket."
  ;; (let ((lsock (lse-sock:socket-server))
  ;;       (asock)
  ;;       (bsock))
  ;;   (setf asock (lse-sock:socket-connect (lse-sock:socket-server-port lsock)
  ;;                                      (lse-sock:socket-server-host lsock)
  ;;                                      :element-type 'character
  ;;                                      :external-format *external-format*
  ;;                                      :buffered nil
  ;;                                      :timeout 5))
  ;;   (if (lse-sock:socket-wait lsock 5)
  ;;     (progn
  ;;       (setf bsock (lse-sock:socket-accept lsock
  ;;                                         :element-type 'character
  ;;                                         :external-format *external-format*
  ;;                                         :buffered nil
  ;;                                         :timeout 5))
  ;;       (if bsock
  ;;         (values asock bsock)
  ;;         (values nil nil)))
  ;;     (values nil nil)))
  )




(defun server ()
  ;; (let ((lsock (lse-sock:socket-server +server-port+)))
  ;;   (unwind-protect
  ;;       (loop
  ;;        (when (lse-sock:socket-wait lsock 0)
  ;;          (let ((remote (lse-sock:socket-accept lsock
  ;;                                              :element-type 'character
  ;;                                              ;; :external-format
  ;;                                              :buffered t
  ;;                                              :timeout 1)))
  ;;            (when remote
  ;;              ;; got an incoming connection, let's fork a worker
  ;;              ;; but first, create a socket and connect to it to be
  ;;              ;; able to communicate with this worker.
  ;;              (let ((pid #+linux(linux:fork) #-linux 0))
  ;;                (cond
  ;;                 ((< pid 0) ;; error
  ;;                  (error "Could not fork a worker."))
  ;;                 ((= pid 0) ;; child
  ;;                  )
  ;;                 (t ;; parent
  ;;                  (register-worker pid)
  ;;                  (format t "~& "))))
  ;;              ))))
  ;;     (close lsock)))
  )



;;;; THE END ;;;;
