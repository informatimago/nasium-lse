;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               iolib-server.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file define a generic client using IOLIB.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Created.
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

(defpackage "COM.INFORMATIMAGO.IOLIB.CLIENT"
  (:use "COMMON-LISP"
        "IOLIB"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.QUEUE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.DFA"
        "COM.INFORMATIMAGO.LOGGER"
        "COM.INFORMATIMAGO.IOLIB.UTILS"
        "COM.INFORMATIMAGO.IOLIB.END-POINT"
        "COM.INFORMATIMAGO.IOLIB.MESSAGE")
  (:export

   )
  (:documentation "

"))
(in-package "COM.INFORMATIMAGO.IOLIB.CLIENT")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Generic Client Side
;;;


(defconstant +session-input-buffer-size+ 4096)

(defconstant +acknowledge-backlog+ 8)



(defclass session ()
  ((name
    :initarg :name
    :initform "Generic Client"
    :accessor session-name
    :documentation "A user label for identification and display purposes.")

   (lo-dfa
    :reader session-lo-dfa
    :documentation "The low-level client DFA.")
   
   (remote-end-point
    :initarg :remote-end-point
    :accessor session-remote-end-point)
   (socket
    :initform nil
    :documentation "The IOLib active socket of the session.") 

   ;; Sending:
   (sender)

   (output-buffer
    :initform nil
    :documentation "The packet currently being sent.")
   (output-index
    :initform 0
    :documentation "The index of the next byte in the packet to send.")

   (last-index
    :initform 0
    :type (unsigned-byte 16)
    :documentation "The index of the last message sent.")
   
   (unacknowledged-messages
    :initform (make-queue)
    :documentation "The messages sent but not acknowledged yet.")

   ;; Receiving:
   (receptor)
   
   (input-buffer
    :initform (make-array +session-input-buffer-size+
                          :element-type '(unsigned-byte 8)
                          :initial-element 0)
    :documentation "The session input buffer.")

   (input-queue
    :initform (make-queue)
    :documentation "A FIFO holding the received responses.")

   (response-received-callback
    :initarg :response-received-callback
    :accessor session-response-received-callback
    :initform nil
    :documentation "A (FUNCTION data session) called when a response is received from the server."))
  
  (:documentation "
A session allows a client to connect to a server and send data
and receive answers.
"))




;; (state
;;  :initform :closed
;;  :reader session-state
;;  :type (member :connected :connecting :closed)
;;  :documentation "A keyword indicating the state of the session.")




(defmethod print-object ((session session) stream)
  (if *print-escape*
   (print-unreadable-object (session stream :identity t :type t)
     (ignore-errors
       (format stream ":name ~S :state ~S"
               (session-name session)
               (session-state session))))
   (format stream "#<>"))
  session)


(defmethod initialize-instance :after ((session session) &key)
  (logger :com.informatimago.iolib.client :debug "INITIALIZE-INSTANCE ~S~%" SESSION)
  (with-slots (sender receptor lo-dfa) session
    (setf sender (make-instance 'sender
                   :process-error (lambda (condition)
                                    (logger :com.informatimago.iolib.client :error "Error while sending ~S to ~A:~% ~A~%"
                                            (message-data (slot-value (slot-value session 'response-sender) 'message))
                                            session
                                            condition))
                   :data-ready (lambda ()
                                 (send-bytes session)
                                 ;; (with-slots (socket) session
                                 ;;   (send-to socket #() :dont-wait t))
                                 ))
          receptor (make-instance 'receptor
                     :receive-message (lambda (message)
                                        (logger :com.informatimago.iolib.client :debug
                                                "Received message ~S" message)
                                        (session-receive-message session message))
                     :process-error   (lambda (err)
                                        (logger :com.informatimago.iolib.client :error
                                                "Receptor got error ~A" err)))
          lo-dfa (make-session-dfa session))))

(defmethod sigpipe ((session session) signum)
  (declare (ignore signum))
  ;; The server disconnected.
  (session-disconnect session))


(defmethod send-bytes ((session session))
  (with-slots (socket output-buffer output-index sender) session
    (when socket
     (when (null output-buffer)
       (setf output-buffer (get-next-packet sender)
             output-index 0))
     (when output-buffer
       (if (< output-index (length output-buffer))
           (let ((wrote-bytes (send-to socket output-buffer
                                       :start output-index
                                       :end (length output-buffer))
                   ;; (handler-case
                   ;;     
                   ;;   #+ecl
                   ;;   (ext:UNIX-SIGNAL-RECEIVED (err)
                   ;;     (error (make-condition 'isys:epipe
                   ;;                            :syscall "unknown"
                   ;;                            :code (ext:unix-signal-received-code err)
                   ;;                            :message "SIGPIPE"
                   ;;                            :handle nil
                   ;;                            :handle2 nil
                   ;;                            :identifier nil))))
                   ))
             (logger :com.informatimago.iolib.client :debug "Wrote ~A bytes to ~A~%"
                     wrote-bytes
                     (session-remote-end-point session))
             (incf output-index wrote-bytes))
          
           (progn
             (setf output-buffer nil)
             (send-bytes session)))))))



(defmethod receive-bytes ((session session))
  (with-slots (socket input-buffer receptor) session
    (when socket
      (handler-case
          (restart-case
              (multiple-value-bind (buffer bytes-read) (receive-from socket
                                                                     :buffer input-buffer
                                                                     :start 0
                                                                     :end (length input-buffer))
                (declare (ignore buffer))
                (logger :com.informatimago.iolib.client :debug "RECEIVE-BYTES received ~D bytes for ~S" bytes-read session)
                (if (zerop bytes-read)
                    (error 'end-of-file)
                    (receptor-receive-bytes receptor input-buffer bytes-read)))
            (IOLIB.SYSCALLS:eintr () nil))
        (IOLIB.SYSCALLS:EWOULDBLOCK () nil)))))






(define-condition socket-address-in-use-error/address (socket-address-in-use-error)
  ((address :initarg :address
            :initform +IPV4-UNSPECIFIED+
            :accessor socket-address-in-use-error-address
            :accessor address-of))
  (:report (lambda (condition stream)
             (format stream "~A: Address ~A already in use"
                     (IOLIB.SYSCALLS::SYSCALL-OF condition)
                     (address-OF condition)))))


(defmethod perform-open-session ((session session))
  (with-slots (socket remote-end-point) session
    ;; TODO: check connection with nodelay for asynchronous connection...
    (when (END-POINT-ADDRESS-UNSPECIFIED-P remote-end-point)
      (error "Please specify a remote address, ~A is unspecified."
             remote-end-point))
    (handler-case
        (setf socket (connect-to-remote-end-point remote-end-point))
      (iomux:poll-timeout (err)
        (return-from perform-open-session nil))
      (SOCKET-CONNECTION-REFUSED-ERROR (err)
        (return-from perform-open-session nil))
      (socket-address-in-use-error (err)
        (error 'socket-address-in-use-error/address
               :address remote-end-point
               :code (slot-value err 'IOLIB.SYSCALLS::code)
               :handle (slot-value err 'IOLIB.SYSCALLS::handle)
               :handle2 (slot-value err 'IOLIB.SYSCALLS::handle2)
               :identifier (slot-value err 'IOLIB.SYSCALLS::identifier)
               :message (slot-value err 'IOLIB.SYSCALLS::message)
               :syscall (slot-value err 'IOLIB.SYSCALLS::syscall))))
    (let ((disconnector
           (funcall (make-disconnector-maker
                     (lambda (session socket)
                       (declare (ignore socket))
                       (session-disconnect (session-lo-dfa session))))
                    session
                    socket)))
      (set-io-handler *event-base*
                      (socket-os-fd socket)
                      :read
                      (funcall (make-reader-maker
                                (lambda (session socket disconnector)
                                  (declare (ignore socket disconnector))
                                  (logger :com.informatimago.iolib.client :debug "Reader handler ~S ~S ~S" session socket disconnector)
                                  (assert (eql socket  (slot-value session 'socket)))
                                  (receive-bytes session)))
                               session
                               socket
                               disconnector))
      (set-io-handler *event-base*
                      (socket-os-fd socket)
                      :write
                      (funcall (make-writer-maker (lambda (session socket disconnector)
                                                    (declare (ignore socket disconnector))
                                                    (send-bytes session)))
                               session
                               socket
                               disconnector)))
    t))


(defmethod perform-close-session ((client session))
  (with-slots (socket) client
    ;; TODO: call the disconnector
    ;; TODO: (unregister-with-iolib socket)
    (when socket
     (handler-case
         (progn
           ;; (shutdown socket :read t :write t)
           (close socket))
       (SOCKET-NOT-CONNECTED-ERROR ()
         ;; ignore this error
         nil)))
    (setf socket nil)))




(define-state-machine session-dfa
    :slots   ((session :initarg :session)
              (timer   :accessor session-dfa-timer))
  :initial closed
  :states  ((closed
             (session-open        () (go-to-state connecting))
             (session-disconnect  () (logger :com.informatimago.iolib.client :warn "SESSION-DFA got a SESSION-DISCONNECT while in CLOSED state.")))
            (connecting
             (:entry              ()
                                  (if (perform-open-session session)
                                      (go-to-state connected)
                                      (cannot-open-session (session-lo-dfa session))))
             (cannot-open-session ()
                                  (let ((delay  (random 20)))
                                    (logger :com.informatimago.iolib.client :warn "Cannot connect right now, will try in ~A second~:*~P." delay)
                                    (go-to-state waiting delay)))
             (session-open        () (logger :com.informatimago.iolib.client :warn "SESSION-DFA got a SESSION-OPEN while in CONNECTING state."))
             (session-close       () (perform-close-session session) (go-to-state closed))
             (session-disconnect  () (perform-close-session session) (go-to-state waiting (random 20))))
            (waiting
             (:entry              (delay)
                                  (setf timer (add-timer *event-base* (lambda () (session-timeout dfa)) delay :one-shot t)))
             (:exit               () (remove-timer *event-base* (session-dfa-timer dfa)))
             (session-timeout     () (go-to-state connecting))
             (session-close       () (go-to-state closed))
             (session-disconnect  () (logger :com.informatimago.iolib.client :warn "SESSION-DFA got a SESSION-DISCONNECT while in WAITING state.")))
            (connected
             (:entry              () ;; TODO: activate the protocol FSM
                                  (receive-bytes session))
             (session-close       () (perform-close-session session) (go-to-state closed))
             (session-disconnect  () (perform-close-session session) (go-to-state connecting)))))






(defmethod open-session ((remote-end-point end-point)
                              &key name tls response-received-callback)
  (declare (ignorable tls)); tls not implemented yet.
  (logger :com.informatimago.iolib.client :info "Creating a session to ~A~%" remote-end-point)
  (let ((session (make-instance 'session
                   :name name
                   :remote-end-point remote-end-point
                   :response-received-callback response-received-callback)))
    (logger :com.informatimago.iolib.client :info "Created a session to ~A~%" remote-end-point)
    (session-open (session-lo-dfa session))
    session))


(defmethod close-session ((session session))
  (session-close (session-lo-dfa session)) 
  (values))


(defmethod session-state ((session session))
  (with-slots (input-queue sender) session
    (with-slots (queue) sender
      (list (case (dfa-state (session-lo-dfa session))
              ((closed)    :CLOSED)
              ((connected) :CONNECTED)
              (otherwise   :connecting))
            (queue-length queue)
            (queue-length input-queue)))))



;; (defmethod resend-unacknowledged-messages ((session session))
;;   (with-slots (sender unacknowledged-messages) session
;;     (let ((entry (queue-dequeue unacknowledged-messages)))
;;       (destructuring-bind (message callback data) entry
;;         (declare (ignore callback data))
;;         (enqueue-message sender message :front t)))))


(defmethod %send-message ((session session) message-class data callback &optional index)
  (with-slots (sender unacknowledged-messages last-index) session
    (enqueue-message sender
                     (make-instance message-class
                       :data (with-standard-io-syntax
                                 (prin1-to-string data))
                       :index (cond
                                (index)
                                ((>= last-index #xffff) (setf last-index 1))
                                (t (incf last-index)))
                       :message-sent (lambda (message)
;;;; TODO: add a timer to resend trailing unacknowledged messages
                                       ;; (when (<= +acknowledge-backlog+
                                       ;;           (queue-length unacknowledged-messages))
                                       ;;   (resend-unacknowledged-messages session))
                                       (logger :com.informatimago.iolib.client :debug "Sent ~A ~A"
                                               message-class (message-index message))
                                       (queue-enqueue unacknowledged-messages
                                                      (list message callback data)))))))

(defmethod send-data ((session session) data &key sent-callback)
  (%send-message session 'data-message data sent-callback))

(defmethod send-command ((session session) command &key sent-callback)
  (%send-message session 'command-message command sent-callback))



(defmethod receive-response ((session session))
  (with-slots (input-queue) session
    (if (queue-empty-p input-queue)
        (values nil nil)
        (values (queue-dequeue input-queue) t))))



;; (defmethod session-receive-message ((session session) (ack acknowledge-message))
;;   (with-slots (unacknowledged-messages) session
;;     (logger :com.informatimago.iolib.client :debug "Received ~A ~A"
;;             'acknowledge-message (message-index ack))
;;     (let ((entry (queue-find unacknowledged-messages (message-index ack)
;;                              :key (lambda (entry) (message-index (first entry))))))
;;       (when entry
;;         (queue-delete unacknowledged-messages entry)
;;         (destructuring-bind (message callback data) entry
;;           (declare (ignore message))
;;           (when callback
;;             (funcall callback data session)))))))


;; (defmethod session-receive-message ((session session) (nak non-acknowledge-message))
;;   (with-slots (sender unacknowledged-messages) session
;;     (logger :com.informatimago.iolib.client :debug "Received ~A ~A"
;;             'non-acknowledge-message (message-index nak))
;;     (let ((entry (queue-find unacknowledged-messages (message-index nak)
;;                              :key (lambda (entry) (message-index (first entry))))))
;;       (when entry
;;         (deletef unacknowledged-messages entry)
;;         ;; We will try to send it again.
;;         ;; TODO: Add a counter to avoid trying too much!
;;         ;; TODO: That means the callback should be informed the message couldn't be sent.
;;         (enqueue-message sender (first entry) :front t)
;;         ;; (destructuring-bind (message callback data) entry
;;         ;;   (when callback
;;         ;;     (funcall callback data session :failed)))
;;         ))))


(defmethod session-receive-message ((session session) (message message))
  ;; This should be a response-message; we're processing it as such in anycase.
  (with-slots (input-queue response-received-callback) session
    (logger :com.informatimago.iolib.client :debug "Received ~A ~A"
            (class-name (class-of message)) (message-index message))
    ;; (%send-message session 'acknowledge-message "" nil (message-index message))
    (let ((response (with-standard-io-syntax
                        (read-from-string (message-data message)))))
      (unless (and response-received-callback
                   (funcall response-received-callback response session))
        (queue-enqueue input-queue response)))))


;;;; THE END ;;;
