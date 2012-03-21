;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               iolib-utils.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    IOLib utilities.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Created.
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

(defpackage "COM.INFORMATIMAGO.IOLIB.UTILS"
  (:use "COMMON-LISP"
        "IOLIB"
        "IOLIB.MULTIPLEX"
        "COM.INFORMATIMAGO.LOGGER"
        "COM.INFORMATIMAGO.SIGNAL"
        "COM.INFORMATIMAGO.IOLIB.END-POINT"
        "COM.INFORMATIMAGO.IOLIB.MESSAGE")
  (:export "*EVENT-BASE*" "EVENT-BASE"
           "*SOCKET*" "EVENT-LOOP"
           "MAKE-CONNECT-HANDLER" "MAKE-LISTENER-HANDLER"
           "MAKE-DISCONNECTOR-MAKER"
           "MAKE-READER-MAKER" "MAKE-WRITER-MAKER"
           "CLIENT-SOCKET")
  (:documentation "
This package exports iolib utilities, functions to make handlers.
"))
(in-package "COM.INFORMATIMAGO.IOLIB.UTILS")



;; Event handler
(defvar *event-base*            nil
  "The IOLib event loop object.")

;; (setf *event-base* (make-instance 'event-base))

(defvar *socket* nil
  "The current socket")


#+developing
(defvar *backtrace* nil)


(defun event-loop ()
  "
Run the event dispatch loop.
"
  (signal-handler-bind
   ((+sigpipe+ (lambda (signum)
                 (logger :com.informatimago.client :debug "~%*** Got signal ~D for ~S ***~%"
                         signum *socket*)
                 (when *socket* (sigpipe *socket* signum)))))
   (catching-signals
    (list +SIGPIPE+)
    (handler-case
        (progn
          ;; let ((iolib.multiplex::*minimum-event-loop-step* 0.0d0)
          ;;      (iolib.multiplex::*maximum-event-loop-step* nil))
          #+developing
          (handler-bind
              ((condition (lambda (condi)
                            (invoke-debugger condi)
                            (setf *backtrace* (backtrace))
                            (signal condi))))
            (event-dispatch *event-base*)) 

          #-developing
          (event-dispatch *event-base*)) ; keep accepting connections forever.

      (socket-connection-reset-error ()
        (logger :com.informatimago.iolib.util :error "Caught unexpected connection reset by peer!~%"))

      (hangup ()
        (logger :com.informatimago.iolib.util :error "Caught unexpected hangup!  Client closed connection on write!~%"))

      (end-of-file ()
        (logger :com.informatimago.iolib.util :error "Caught unexpected end-of-file!  Client closed connection on read!~%"))
      
      (error (err)
        (logger :com.informatimago.iolib.util :error "Exiting for ~A~%" err))

      (condition (condi)
        (logger :com.informatimago.iolib.util :warn "Exiting for ~A~%" condi)))))
  
  (finish-output *error-output*)
  (finish-output *trace-output*)
  (finish-output *standard-output*)
  (values))


(defgeneric client-socket (client))


(defun make-connect-handler (socket rejector acceptor disconnector-maker reader-maker writer-maker conclusion)
    "

Make a :write handler that will process the connection on the SOCKET
once established, call ACCEPTOR with it, and when the acceptor returns
a non-null object, set two io handlers made from the READER-MAKER and
the WRITER-MAKER, passing them the disconnector made by
DISCONNECTOR-MAKER, and finally the CONCLUSION is called.
If the connection cannot be established, the REJECTOR is called instead.

SOCKET:             The IOLib active socket.
REJECTOR:           (function client-socket condition)
ACCEPTOR:           (function client-socket) -> client
DISCONNECTOR-MAKER: (function client client-socket)
READER-MAKER:       (function client client-socket disconnector)
WRITER-MAKER:       (function client client-socket disconnector)
CONCLUSION:         (function client client-socket)

Example:

 (let ((socket (make-socket ...)))
   (set-io-handler *event-base*
                   (socket-os-fd socket)
                   :write (make-connect-handler socket
                                                rejector acceptor
                                                disconnector-maker reader-maker writer-maker
                                                conclusion))
   (connect socket remote-address :port remote-port :wait 0))

"
  (lambda (fd event exception)
    (declare (ignorable event exception))
    (logger :com.informatimago.iolib.util.handler :debug "Connect handler ~S ~S ~S~%"
            fd event exception)
    (let ((errno (socket-option fd :error)))
      (if (zerop errno)
          (let ((client (funcall acceptor socket)))
            (when client
              (let ((disconnector (funcall disconnector-maker client socket)))
                (set-io-handler *event-base*
                                (socket-os-fd (client-socket client))
                                :read (funcall reader-maker client socket disconnector))
                (set-io-handler *event-base*
                                (socket-os-fd (client-socket client))
                                :write (funcall writer-maker client socket disconnector))
                (funcall conclusion client socket))))
          (funcall rejector socket (IOLIB.SYSCALLS:make-syscall-error errno "connect" fd nil))))))



(defun make-listener-handler (socket acceptor disconnector-maker reader-maker writer-maker conclusion)
    "
Make a listener handler that accepts a new client from the SOCKET,
calls ACCEPTOR with it, and when the acceptor returns a non-null
object, sets two io handlers made from the READER-MAKER and the
WRITER-MAKER, passing them the disconnector made by
DISCONNECTOR-MAKER, and finally the CONCLUSION is called.

SOCKET:             The IOLib passive socket.
ACCEPTOR:           (function client-socket) -> client
DISCONNECTOR-MAKER: (function client client-socket)
READER-MAKER:       (function client client-socket disconnector)
WRITER-MAKER:       (function client client-socket disconnector)
CONCLUSION:         (function client client-socket)
"
  (lambda (fd event exception)
    (declare (ignorable fd event exception))
    (logger :com.informatimago.iolib.util.handler :debug "Listen handler ~S ~S ~S~%"
            fd event exception)
    ;; do a blocking accept, returning nil if no socket
    (let ((client-socket (accept-connection socket :wait t)))
      (when client-socket
        (let ((client (funcall acceptor client-socket)))
          (when client
            (let ((disconnector (funcall disconnector-maker client client-socket)))
              (set-io-handler *event-base*
                              (socket-os-fd client-socket)
                              :read (funcall reader-maker client client-socket disconnector))
              (set-io-handler *event-base*
                              (socket-os-fd client-socket)
                              :write (funcall writer-maker client client-socket disconnector))
              (funcall conclusion client client-socket))))))))



(defmethod make-disconnector-maker (closer)
  "
Make a disconnector.  This disconnector will remove the io handler requested, and
when the socket is closed, calls the CLOSER.

CLOSER:             (function client client-socket)
"
  (lambda (client client-socket)
    (lambda (who port &rest events)
      ;; When this function is called, it can be told which callback to remove, if
      ;; no callbacks are specified, all of them are removed! The socket can be
      ;; additionally told to be closed.
      (logger :com.informatimago.iolib.util.handler :debug "Disconnector ~A ~A ~S~%" who port events)
      (let ((fd (socket-os-fd  client-socket)))
        (if (not (intersection '(:read :write :error) events))
            (remove-fd-handlers *event-base* fd :read t :write t :error t)
            (progn
              (when (member :read events)
                (remove-fd-handlers *event-base* fd :read t))
              (when (member :write events)
                (remove-fd-handlers *event-base* fd :write t))
              (when (member :error events)
                (remove-fd-handlers *event-base* fd :error t))))
        (logger :com.informatimago.iolib.util.handler :debug "   removed fd handlers ~A~%" fd))
      ;; and finally if we're asked to close the socket, we do so here
      (when (member :close events)
        (close client-socket)
        (funcall closer client client-socket))
      (logger :com.informatimago.iolib.util.handler :debug "   done~%"))))



(defun make-reader-maker (receiver)
  "
Make a maker of a read handler.
When bytes are ready to be received, the RECEIVER function is called.

RECEIVER:           (function  client client-socket disconnector)
"
  (lambda (client client-socket disconnector)
    (lambda (fd event exception)
      (declare (ignorable fd event exception))
      (logger :com.informatimago.iolib.util.handler :debug "Read handler ~S ~S ~S~%"
              fd event exception)
      (handler-case
          (funcall receiver client client-socket disconnector)

        (socket-connection-reset-error ()
          ;; Handle the client sending a reset.
          (logger :com.informatimago.iolib.util :warn "Client ~A: connection reset by peer.~%"
                  (socket-remote-end-point client-socket))
          (funcall disconnector
                   (remote-host client-socket)
                   (remote-port client-socket)
                   :close))

        (end-of-file ()
          ;; When we get an end of file, that doesn't necessarily
          ;; mean the client went away, it could just mean that
          ;; the client performed a shutdown on the write end of
          ;; its socket and it is expecting the data stored in
          ;; the server to be written to it.  However, if there
          ;; is nothing left to write and our read end is closed,
          ;; we shall consider it that the client went away and
          ;; close the connection.
          (logger :com.informatimago.iolib.util :warn "Client ~A produced end-of-file on a read.~%"
                  (socket-remote-end-point client-socket))
          (funcall disconnector
                   (remote-host client-socket)
                   (remote-port client-socket)
                   ;; :read
                   :close))))))



(defun make-writer-maker (sender)
  "
Make a maker of a write handler.
When bytes are ready to be received, the SENDER function is called.

SENDER:             (function  client client-socket disconnector)
"
  (lambda (client client-socket disconnector)
    (lambda (fd event exception)
      (declare (ignorable fd event exception))
      ;; (logger :com.informatimago.iolib.util.handler :debug "Write handler ~S ~S ~S~%"
      ;;         fd event exception)
      (handler-case
          (funcall sender client client-socket disconnector)

        (socket-connection-reset-error ()
          ;; If for some reaon the client resets the network connection,
          ;; we'll get this signal.
          (logger :com.informatimago.iolib.util :warn "Client ~A: connection reset by peer.~%"
                  (socket-remote-end-point client-socket))
          (funcall disconnector
                   (remote-host client-socket)
                   (remote-port client-socket)
                   :close))

        (isys:ewouldblock ()
          ;; Sometimes this happens on a write even though it
          ;; might have been marked as ready. Also we might have
          ;; asked to write on an unknown status socket. Ignore
          ;; it and we will try again later.
          (logger :com.informatimago.iolib.util :warn "WRITE-SOME-BYTES: EWOULDBLOCK~%"))

        (isys:epipe ()
          ;; In this server, if the client doesn't accept data,
          ;; it also means it will never send us data again. So
          ;; close the connection for good.
          (logger :com.informatimago.iolib.util "Client ~A got hangup on write.~%"
                  (socket-remote-end-point client-socket))
          (funcall disconnector
                   (remote-host client-socket)
                   (remote-port client-socket)
                   :close))))))

;;;; THE END ;;;;
