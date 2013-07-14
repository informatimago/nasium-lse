;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               iolib-message.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Defines a message class for iolib-server and iolib-client.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2013
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

(defpackage "COM.INFORMATIMAGO.IOLIB.MESSAGE"
  (:use "COMMON-LISP"
        "BABEL"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.QUEUE"
        "COM.INFORMATIMAGO.LOGGER")
  (:export "+CARRIAGE-RETURN+" "+LINE-FEED+" "MAKE-BUFFER"
           "BUFFER-LENGTH" "BUFFER-REF" "BUFFER-SUBSEQ" "BUFFER-SEARCH"
           "BUFFER-DELETE-FROM-HEAD" "BUFFER-CLEAR" "BUFFER-APPEND"

           "MESSAGE" "MESSAGE-DATA" "MESSAGE-COMPLETE-P" "MESSAGE-INDEX" "MESSAGE-SENT"
           "GET-NEXT-PACKET" "MESSAGE-PARSE" "LINE-MESSAGE" "MESSAGE-TEXT"
           "RECEPTOR" "PROTOCOL-ERROR" "BACKTRACE"
           "RECEPTOR-RECEIVE-BYTES" "SENDER" "ENQUEUE-MESSAGE")
  (:documentation "

"))
(in-package "COM.INFORMATIMAGO.IOLIB.MESSAGE")


(defconstant +carriage-return+       13 "ASCII code of CR.")
(defconstant +line-feed+             10 "ASCII code of LF.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BUFFER
;;;
;;; The following functionnal abstraction specifies buffers, that are
;;; used to gather bytes and eat messages in FIFO order.
;;;
;;; This implementation moves eaten bytes down the vector, assuming
;;; there won't be a lot of remaining bytes to move.  If this
;;; assumption reveals false, then another implementation, cord-like,
;;; could be written.  

(defun make-buffer (initial-size)
  (make-array initial-size
              :element-type '(unsigned-byte 8)
              :adjustable t
              :fill-pointer 0))

(defun buffer-length (buffer)
  (length buffer))

(defun buffer-ref (buffer index)
  (aref buffer index))

(defun buffer-subseq (buffer start end)
  (let ((sub (make-array (- end start) :element-type '(unsigned-byte 8))))
    (replace sub buffer :start2 start)
    sub))

(defun buffer-search (subsequence buffer)
  (search subsequence buffer))

(defun buffer-delete-from-head (buffer size-to-remove)
  (replace buffer buffer :start2 size-to-remove)
  (decf (fill-pointer buffer) size-to-remove)
  buffer)

(defun buffer-clear (buffer)
  (setf (fill-pointer buffer) 0))

(defun buffer-append (buffer bytes size)
  (let* ((old-size (length buffer))
         (new-size (+ old-size size)))
    (loop :while (< (array-dimension buffer 0) new-size) :do
       (setf buffer (adjust-array buffer
                                  (* 2 (array-dimension buffer 0))
                                  :element-type (array-element-type buffer)
                                  :fill-pointer (fill-pointer buffer))))
    (setf (fill-pointer buffer) new-size)
    (replace buffer bytes :start1 old-size :end2 size)
    buffer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MESSAGE
;;;
;;;


(defclass message ()
  ((data
    :initarg :data
    :initform #()
    :accessor message-data)
   (index
    :initarg :index
    :initform 0
    :type integer
    :accessor message-index
    :documentation "Message index, could be used to match ACK/NAK messages.")
   (completep
    :initform nil
    :accessor message-complete-p)
   (message-sent
    :initform nil
    :initarg :message-sent
    :accessor message-sent
    :documentation "A function taking the message as argument that may be called
when the message has been sent."))
  (:documentation "This is an abstract message class, defining the
common slots and methods.

There are two ways to initialize a message:

- giving a :data vector (byte vector or string depending on the
  subclass), in which case the message object is used to split the
  data into packets with the GET-NEXT-PACKET method, until it returns
  NIL, or

- giving a :text string, which is encoded in UTF-8 into the data.  The
  message object is then used to split the data into packets with the
  GET-NEXT-PACKET method, until it returns NIL, or

- not giving a :data vector, in which case the message object is used
  to gather byte buffers with MESSAGE-PARSE, until it indicates the
  message is complete, in which occurence, the DATA slot is filled
  with the received data (byte vector or string).

The RECEPTOR class below parses the packets to instanciate messages of
the right class, to receive the packets.
"))


(defmethod print-object ((self message) stream)
  (print-unreadable-object (self stream :identity *print-escape* :type t)
    (format stream (if *print-escape*
                       ":DATA ~S :INDEX ~A :COMPLETEP ~A"
                       "[~1@*~A] ~0@*~A ~2@*~:[~;COMPLETE~]")
            (message-data self)
            (message-index self)
            (message-complete-p self)))
  self)


(defgeneric get-next-packet (message)
  (:documentation "Return the next packet (a byte vector) of the message, or NIL."))

(defgeneric message-parse (message buffer)
  (:documentation "
Parses the buffer, gathering data from it.
Return the number of bytes read, and whether the message is complete.
"))

(defmethod initialize-instance :after ((message message) &key (data #() datap))
  (declare (ignore data))
  (setf (slot-value message 'completep) datap))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LINE-MESSAGE
;;;
;;; A line message is one line of text, encoded in UTF-8, terminated with CR LF.
;;;  

(defun xor (a b) (or (and a (not b)) (and (not a) b)))
(defun imply (p q) (or (not p) q))

(defclass line-message (message)
  ())


(defmethod initialize-instance :after ((message line-message) &key (data #() datap) (text "" textp))
  (assert (imply (or datap textp) (xor datap textp)) () ":DATA and :TEXT are mutually exclusive.")
  (cond
    (datap
     (setf (message-data message) (make-buffer (length data)))
     (buffer-append (message-data message) data (length data)))
    (textp
     (let ((data (babel:string-to-octets text :encoding :utf-8)))
       (setf (message-data message) (make-buffer (length data)))
       (buffer-append (message-data message) data (length data))))
    (t
     (setf (message-data message) (make-buffer 8)))))


(defmethod get-next-packet ((message line-message))
  (prog1 (message-data message)
    (setf (message-data message) nil)))


(defmethod message-parse ((message line-message) new-buffer)
  (if (<= 2 (buffer-length new-buffer))
      (let ((crlf (buffer-search #(13 10) new-buffer)))
        (if crlf
            (with-slots (data completep) message
              (buffer-append data (buffer-subseq new-buffer 0 crlf) crlf)
              (setf completep t)
              (values (+ 2 crlf) t))
            (values 0 nil)))
      (values 0 nil)))


(defmethod message-text ((message line-message))
  (let ((data (message-data message)))
    (if data
        (babel:octets-to-string data :encoding :utf-8)
        "")))



;; (defun test-line-message ()
;;   (flet ((generate-message (size)
;;            (let ((text (make-string size)))
;;              (loop
;;                 :with chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZéêèà !\"#$%&'()*+,-./0123456789:;<=>?@"
;;                 :for i :below size
;;                 :do (setf (aref text i)
;;                            (case (random 20)
;;                              ((0) #\newline)
;;                              ((1 2) #\space)
;;                              (otherwise (aref chars (random (length chars)))))))
;;              text)))
;;    (let* ((raw-bytes (generate-message 2000))
;;           (outmsg (make-instance 'data-message :data raw-bytes))
;;           (inpmsg (make-instance 'data-message))
;;           (packets (loop
;;                       :for packet = (get-next-packet outmsg)
;;                       :while packet :collect packet))
;;           (all-packets (apply (function concatenate) 'vector packets)))
;;      #+(and)
;;      (loop
;;         :for packet :in packets
;;         :for header = (octets-to-string (subseq packet 0 2) :encoding :ascii)
;;         :do (logger :imcp.protocol :debug "~A ~5D : ~S~%"
;;                     header (length packet) packet))
;;      (loop
;;         :with chunk-size = 800
;;         :with start = 0
;;         :with end = chunk-size
;;         :while (< start (length all-packets))
;;         :do (let ((bytes (subseq all-packets start (min (length all-packets) end))))
;;               (multiple-value-bind (read-so-far donep) (message-parse inpmsg bytes)
;;                (if (zerop read-so-far)
;;                  (incf end chunk-size)
;;                  (progn
;;                    (incf start read-so-far)
;;                    (incf end   read-so-far)))
;;                (when donep
;;                  (unless (equalp raw-bytes (message-data inpmsg))
;;                    (logger :imcp.protocol :fatal "~&Expected: ~5D ~S~%"
;;                            (length raw-bytes) raw-bytes)
;;                    (logger :imcp.protocol :fatal "~&Got:      ~5D ~S~%"
;;                            (length (message-data inpmsg)) (message-data inpmsg))
;;                    (error
;;                      "Mismatch between raw-bytes (~D bytes) and (message-data inpmsg) (~D bytes): ~D~%"
;;                      (length raw-bytes) (length (message-data inpmsg))
;;                      (mismatch raw-bytes (message-data inpmsg)))))))
;;         :finally (assert (= start (length all-packets))))))
;;   :success)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; RECEPTOR
;;;
;;; A receptor contains a buffer in which received bytes are
;;; accumulated, and from which packets are reconstituted and sent to
;;; a message (of a class determined by the first byte of the
;;; packets).   When the message is complete, it is sent to the
;;; receive-message function.


(defclass receptor ()
  ((state
    :initform :normal
    :type (member :normal :sync-cr :sync-lf))
   (buffer
    :initform (make-buffer 1024))
   (message-index
    :initform -1
    :accessor receptor-message-index)
   (message
    :initform nil
    :reader receptor-message)
   (receive-message
    :initarg :receive-message
    :accessor receptor-receive-message)
   (process-error
    :initarg :process-error
    :accessor receptor-process-error)))


(define-condition protocol-error (error)
  ((buffer       :initarg :buffer
                 :accessor protocol-error-buffer)
   (message-code :initarg :message-code
                 :accessor protocol-error-message-code)
   (message      :initarg :message
                 :accessor protocol-error-message))
  (:report (lambda (condition stream)
               (write-string (protocol-error-message condition) stream))))


(defun backtrace (&optional output)
  (if output
      #+swank
      (swank::call-with-debugging-environment
       (lambda ()
         (mapcar (lambda (frame)
                   (swank::print-frame frame output)
                   (terpri output))
                 (swank::compute-backtrace 0 nil))))
      #-swank
      (progn (princ "#<backtrace unavailable>" output)
             (terpri output))
      (with-output-to-string (output)
        (backtrace output))))


(defmethod receptor-receive-bytes ((receptor receptor) bytes size)
  (with-slots (state buffer message receive-message process-error) receptor
    (logger :com.informatimago.iolib.message :debug "Receptor :state ~S :buffer ~S ~%Receive ~D byte~:*~P ~S~%"
            state buffer size (subseq bytes 0 size))
    (labels ((report-error (error)
               (when (slot-boundp receptor 'process-error)
                 (funcall process-error error)))
             (forward-error (error) (error error))
             (print-backtrace-and-forward-error (error)
               (terpri *error-output*) (terpri *error-output*)
               (princ error)
               (terpri *error-output*) (terpri *error-output*)
               (backtrace *error-output*)
               (terpri *error-output*) (terpri *error-output*)
               (force-output *error-output*)
               (forward-error error)))
      (handler-case
          (handler-bind
              ((protocol-error            (function forward-error))
               (END-OF-INPUT-IN-CHARACTER (function forward-error))
               (error                     (function print-backtrace-and-forward-error)))
            (progn
              (setf buffer (buffer-append buffer bytes size))
              (when (null message)
                (setf message (make-instance 'line-message :index (incf (receptor-message-index receptor)))))
              (when message
                (multiple-value-bind (read-so-far message-complete-p) (message-parse message buffer)
                  (when (plusp read-so-far)
                    (buffer-delete-from-head buffer read-so-far))
                  (when message-complete-p
                    (when (slot-boundp receptor 'receive-message)
                      (funcall receive-message message))
                    (setf message nil)
                    (return-from receptor-receive-bytes (receptor-receive-bytes receptor #() 0)))))))
        (error (err)
               (format *error-output* "Will report-error ~A~%" err)
               (finish-output *error-output*)
               (report-error err))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SENDER
;;;


(defclass sender ()
  ((queue
    :initform (make-queue)
    :documentation "The queue of messages to be sent.")
   (message
    :initform nil
    :initarg :message
    :documentation "The message currently being sent.")
   (data-ready
    :initarg :data-ready
    :accessor sender-data-ready)
   (process-error
    :initarg :process-error
    :accessor sender-process-error)))


(defmethod enqueue-message ((sender sender) message &key front)
  (with-slots (queue data-ready) sender
    (if front
        (queue-requeue queue message)
        (queue-enqueue queue message))
    (when (slot-boundp sender 'data-ready)
      (funcall data-ready))))


(defmethod get-next-packet ((sender sender))
  (with-slots (queue message process-error) sender
    (if message
      (or (get-next-packet message)
          (progn
            (when (message-sent message)
              (funcall (message-sent message) message))
            (setf message nil)
            (get-next-packet sender)))
      (unless (queue-empty-p queue)
        (setf message (queue-dequeue queue))
        (get-next-packet sender)))))


;;;; THE END ;;;;
