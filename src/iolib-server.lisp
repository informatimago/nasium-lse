;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               iolib-server.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file define a generic server using IOLIB.
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.IOLIB.SERVER"
  (:use "COMMON-LISP"
        "SPLIT-SEQUENCE"
        "ALEXANDRIA"
        "IOLIB"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.QUEUE"
        "COM.INFORMATIMAGO.LOGGER"
        "COM.INFORMATIMAGO.IOLIB.MESSAGE"
        "COM.INFORMATIMAGO.IOLIB.END-POINT"
        "COM.INFORMATIMAGO.IOLIB.UTILS")
  (:shadowing-import-from "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
                          "STRING-DESIGNATOR")
  (:export "ENQUEUE-MESSAGE"
           "+CLIENT-INPUT-BUFFER-SIZE+"
           "CLIENT" "CLIENT-SERVER" "CLIENT-STATE" "CLIENT-SOCKET" "CLIENT-LOCAL-END-POINT"
           "CLIENT-REMOTE-END-POINT" 
           "CLIENT-RESPONSE-QUEUE-LENGTH"
           "CLIENT-RECEIVE-MESSAGE"
           "RECEIVE-BYTES"
           "CLIENT-SEND-RESPONSE"
           "SEND-BYTES"
           "CLIENT-HELLO-MESSAGE"
           "CLIENT-CLOSE"
           "SERVER"
           "SERVER-NAME" "SERVER-STATE" "SERVER-SOCKET" "SERVER-CLIENTS"
           "SERVER-DATA-INPUT-QUEUE" "SERVER-DATA-RECEIVED-CALLBACK"
           "SERVER-ADD-CLIENT"
           "SERVER-REMOVE-CLIENT"
           "START-SERVER"
           "CLOSE-SERVER"
           "SERVER-STATE"
           ;; "RECEIVE-DATA"
           ;; "RECEIVE-COMMAND"
           "SERVER-RECEIVE-MESSAGE"
           ;; "SERVER-SEND-RESPONSE"
           )
  (:documentation "

"))
(in-package "COM.INFORMATIMAGO.IOLIB.SERVER")

(defgeneric enqueue-message (sender message &key front))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CLIENT
;;;
;;; Representation of the client in the server.

(defconstant +client-input-buffer-size+ 4096)


(defclass client ()
  ((server
    :initarg :server
    :reader client-server
    :documentation "The server of this client.")
   
   (state
    :initform :open
    :reader client-state
    :type (member :open :closed)
    :documentation "Indicates whether the client is closed.")
   
   (socket
    :initarg :socket
    :accessor client-socket
    :documentation "The IOLib active socket of the client.")
   (local-end-point
    :reader client-local-end-point
    :documentation "The remote end-point of the client socket.")
   (remote-end-point
    :reader client-remote-end-point
    :documentation "The remote end-point of the client socket.")

   ;; (acknowledge-sender)
   (response-sender)

   (last-index
    :initform 0
    :type (unsigned-byte 16)
    :documentation "The index of the last message sent.")
   
   (output-buffer
    :initform nil
    :documentation "The packet currently being sent.")
   (output-index
    :initform 0
    :documentation "The index of the next byte in the packet to send.")
   
   (receptor)

   (input-buffer
    :initform (make-array +client-input-buffer-size+
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)
    :documentation "The socket input buffer."))
  
  (:documentation "Represents a client in the server."))


(defmethod print-object ((client client) stream)
  (print-unreadable-object (client stream :identity *print-escape* :type t)
    (ignore-errors
      (format stream
              (if *print-escape*
                  ":REMOTE-END-POINT ~S"
                  "~A")
              (client-remote-end-point client))))
  client)


(defmethod initialize-instance :after ((client client) &key)
  (with-slots (remote-end-point local-end-point ;; acknowledge-sender
                                response-sender receptor) client
    (setf remote-end-point (socket-remote-end-point (client-socket client))
          local-end-point  (socket-local-end-point  (client-socket client))
          response-sender (make-instance 'sender
                              :data-ready    (lambda ()
                                               (with-slots (socket) client
                                                 (when socket
                                                   (send-to socket #() :dont-wait t))))
                              :process-error (lambda (condition)
                                               (logger :com.informatimago.iolib.server :error "Error while sending ~A to ~A:~% ~A~%"
                                                       (ignore-errors (message-data (slot-value (slot-value client 'response-sender) 'message)))
                                                       client
                                                       condition)))
          receptor (make-instance 'receptor
                       :receive-message (lambda (message)
                                          (client-receive-message client message))
                       :process-error (lambda (condition)
                                        (logger :com.informatimago.iolib.server :error "Error while receiving ~A from ~A:~% ~A~%"
                                                (ignore-errors (message-data (slot-value (slot-value client 'response-sender) 'message)))
                                                client
                                                condition))))))


(defmethod client-response-queue-length ((client client))
  (with-slots (response-sender) client
    (with-slots (queue) response-sender
      (queue-length queue))))


(defmethod client-receive-message ((client client) message)
  (logger :com.informatimago.iolib.server :info "Client ~A received message ~A~%"
          client message)
  ;; (send-acknowledge client (message-index message) "Got it.")
  (server-receive-message (client-server client) client message))


(defmethod receive-bytes ((client client))
  (with-slots (socket input-buffer receptor) client
    (multiple-value-bind (buffer bytes-read)
        (receive-from socket :buffer input-buffer)
      (declare (ignore buffer))
      (if (zerop bytes-read)
          (error 'end-of-file)
          (receptor-receive-bytes receptor input-buffer bytes-read)))))


(defmethod client-send-response ((client client) (message message) &rest arguments)
  (declare (ignore arguments))
  (with-slots (response-sender last-index) client
    (enqueue-message response-sender message)))



(defmethod client-send-response ((client client) (control-string string) &rest arguments)
  (with-slots (last-index) client
    (client-send-response
     client
     (make-instance 'line-message
         :text (UNSPLIT-STRING
                (split-sequence #\newline (apply (function format) nil control-string arguments))
                #.(coerce (vector (code-char 13) (code-char 10)) 'string))
         :index (incf last-index)
         :message-sent (lambda (message)
                         (logger :com.informatimago.iolib.server :info
                                 "Sent RESPONSE ~A to ~A~%"
                                 (message-data message) client))))))


(defmethod send-bytes ((client client))
  (with-slots (socket output-buffer output-index response-sender) client
    (when socket
      (when (null output-buffer)
        (setf output-buffer (get-next-packet response-sender)
              output-index 0))
      (when output-buffer
        (if (< output-index (length output-buffer))
            (let ((written-bytes (send-to socket output-buffer
                                          :start output-index
                                          :end (length output-buffer))))
              (logger :com.informatimago.iolib.server :debug "Wrote ~A bytes to ~A~%" written-bytes
                      (client-remote-end-point client))
              (incf output-index written-bytes))
            (progn
              (setf output-buffer nil)
              (send-bytes client)))))))



;; (defmethod send-eof ((client client))
;;   "
;; Add an EOF marker to the output buffers of the CLIENT.
;; When the send-byte method reaches this marker, the client will be closed.
;; "
;;   (queue-enqueue (client-output-queue client) :eof))


(defmethod client-hello-message ((client client) name)
  (format nil "~A ~A ~S"
          (server-name (client-server client))
          (ensure-hostname (local-host (client-socket client)))
          name))


(defmethod client-close ((client client))
  (setf (slot-value client 'state) :closed)
  (when (socket-os-fd (client-socket client))
   (ignore-errors (remove-fd-handlers *event-base*
                                      (socket-os-fd (client-socket client))
                                      :read t :write t :error t))
   (shutdown (client-socket client) :read t :write t)
   (close (client-socket client)))
  (values))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SERVER
;;;

(defclass server ()
  ((name
    :initarg :name
    :accessor server-name
    :initform "Generic Server"
    :documentation "A user label for identification and display purposes.")

   (state
    :initform :listening
    :reader server-state
    :type (member :listening :closed)
    :documentation "A keyword indicating the state of the server.")
   
   (socket
    :accessor server-socket 
    :documentation "The IOLib passive (listening) socket of the server.")

   (clients
    :accessor server-clients
    :initform '()
    :documentation "A list of clients.")

   (data-input-queue
    :accessor server-data-input-queue
    :initform (make-queue)
    :documentation "A FIFO holding lists of (client data-message).")

   (data-received-callback
    :accessor server-data-received-callback
    :initform nil
    :initarg :data-received-callback
    :documentation "A function called when data is received."))
  
  (:documentation "An server listens for incoming connections
and then receive data messages from clients, and sends answers to them."))


(defmethod print-object ((server server) stream)
  (print-unreadable-object (server stream :identity *print-escape* :type t)
    (format stream (if *print-escape*
                       ":name ~S :state ~S"
                       "~A [~A]")
            (server-name server)
            (server-state server)))
  server)


(defmethod server-add-client ((server server) (client client))
  (pushnew client (server-clients server)))

(defmethod server-remove-client ((server server) (client client))
  (client-close client)
  (deletef (server-clients server) client)
  (logger :com.informatimago.iolib.server :notice "Client disconnected ~A" client))



(defmethod start-server (local-end-point
                         &key name tls
                         client-connected
                         data-received-callback
                         (client-class 'client))
  "
DO:                     Creates a new server (running in a separate thread if possible).

RETURN:                 The server object.


LOCAL-END-POINT:        The local address:port wher ethe server will listen from.

NAME:                   A string naming the server.

TLS:                    -- Not implemented yet.

CLIENT-CONNECTED:       A (function server client) called once the client is connected.

CLIENT-CLASS:           The class of client to instanciate.  Should be subclass of CLIENT.

DATA-RECEIVED-CALLBACK: A (function data server client) called everytime data is received.


NOTE:                   The CLIENT-RECEIVE-MESSAGE method is called
                        first, and the method on CLIENT calls
                        SERVER-RECEIVE-MESSAGE which calls the
                        DATA-RECEIVED-CALLBACK.  If
                        CLIENT-RECEIVE-MESSAGE is overriden by a
                        subclass, it may not CALL-NEXT-METHOD, and
                        therefore DATA-RECEIVED-CALLBACK may not be
                        called.
"
  (declare (ignorable tls)) ; tls is not implemented yet
  (let ((server (make-instance 'server
                    :name name
                    :data-received-callback data-received-callback)))
    (with-slots (socket) server
      (setf socket (listen-to-local-end-point local-end-point))

      (set-io-handler *event-base*
                      (socket-os-fd socket)
                      :read  
                      (make-listener-handler
                       socket
                       
                       (lambda (client-socket)      ; acceptor ; must return the client, or nil.
                         (let ((client (make-instance client-class
                                           :server server
                                           :socket client-socket)))
                           (server-add-client server client)
                           (logger :com.informatimago.iolib.server :notice "Accepted a client from ~A~%"
                                   (client-remote-end-point client))
                           client))

                       (make-disconnector-maker
                        (lambda (client client-socket)
                          (declare (ignore client-socket))
                          (logger :com.informatimago.iolib.server :notice "Closing connection to ~A~%"
                                  (client-remote-end-point client))
                          (server-remove-client server client)))
                       
                       (make-reader-maker
                        (lambda (client client-socket disconnector)
                          (declare (ignore client-socket disconnector))
                          ;; Read however much we are able.
                          (receive-bytes client)))

                       (make-writer-maker
                        (lambda (client client-socket disconnector)
                          (declare (ignore client-socket disconnector))
                          (send-bytes client)
                          ;; If we see we're out of data to write
                          ;; then close the connection, we're done. 
                          ;; (when (eql :eof (queue-first-element (client-output-queue client)))
                          ;;   (funcall disconnector
                          ;;            (remote-host client-socket)
                          ;;            (remote-port client-socket)
                          ;;            :close))
                          ))

                       (lambda (client client-socket) ; conclusion
                         (logger :com.informatimago.iolib.server :info "Conclusion for client ~A" client)
                         (when client-connected (funcall client-connected server client client-socket))))))
    server))


(defmethod close-server ((server server))
  (setf (slot-value server 'state) :closed)
  (dolist (client (copy-list (server-clients server)))
    (server-remove-client server client))
  (shutdown (server-socket server) :read t :write t)
  (close (server-socket server))
  (values))


(defmethod server-state ((server server))
  (with-slots (data-input-queue clients state) server
    (list state
          (queue-length data-input-queue)
          (mapcar (lambda (client)
                    (list
                     (client-remote-end-point client)
                     (client-response-queue-length client)))
                  clients))))


;; (defmethod receive-data ((server server))
;;   (with-slots (data-input-queue) server
;;     (if (queue-empty-p data-input-queue)
;;         (values nil nil nil)
;;         (destructuring-bind (client data) (queue-dequeue data-input-queue)
;;           (values data client t)))))
;; 
;; 
;; (defmethod receive-command ((server server))
;;   (with-slots (command-input-queue) server
;;     (if (queue-empty-p command-input-queue)
;;         (values nil nil nil)
;;         (destructuring-bind (client command) (queue-dequeue command-input-queue)
;;           (values command client t)))))


(defmethod server-receive-message ((server server) (client client) message)
  (flet ((receive (callback queue data)
           (unless (and callback (funcall callback data server client))
             (queue-enqueue queue (list client data)))))
    (with-slots (data-received-callback data-input-queue) server
      (receive data-received-callback data-input-queue (message-data message)))))


;; (defmethod server-send-response ((server server) (client client) response &key sent-callback)
;;   (assert (member client (server-clients server)))
;;   (client-send-response client "~A" (with-standard-io-syntax (prin1-to-string response)))
;;   ;; (with-slots (response-sender last-index) client
;;   ;;   (enqueue-message response-sender
;;   ;;                    (make-instance 'line-message
;;   ;;                        :text 
;;   ;;                        :index (incf last-index)
;;   ;;                        :message-sent (lambda (message)
;;   ;;                                        (declare (ignore message))
;;   ;;                                        (when sent-callback
;;   ;;                                          (funcall sent-callback response server client))))))
;;   )




;;;; THE END ;;;;
