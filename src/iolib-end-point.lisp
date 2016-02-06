;;;; -*- mode:lisp; coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               end-point.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines end-points.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-01 <PJB> Created.
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(defpackage "COM.INFORMATIMAGO.IOLIB.END-POINT"
  (:use "COMMON-LISP" "IOLIB" "IOLIB.SOCKETS")
  (:export
   "END-POINT"
   "TCP-IPV4-END-POINT" "TCP-IPV4-END-POINT-INTERFACE" "TCP-IPV4-END-POINT-PORT"
   "UNIX-SOCKET-END-POINT" "UNIX-SOCKET-END-POINT-PATHNAME"
   "PIPE-END-POINT-READ" "PIPE-END-POINT-READ-FILE-DESCRIPTOR" "PIPE-END-POINT-WRITE-FILE-DESCRIPTOR"
   "END-POINT-ADDRESS" "END-POINT-ADDRESS-UNSPECIFIED-P"
   "LISTEN-TO-LOCAL-END-POINT" "CONNECT-TO-REMOTE-END-POINT"
   "SOCKET-LOCAL-END-POINT" "SOCKET-REMOTE-END-POINT"

   "PIPE-END-POINT"
   "PIPE-END-POINT-READ-FILE-DESCRIPTOR"
   "PIPE-END-POINT-WRITE-FILE-DESCRIPTOR")
  
  (:documentation "
This package implements end-points, which are reifications of IP
addresses, unix sockets and pipe pairs.  Methods are defined using
iolib to connect or listen to an end-point (to establish a
communucation channel).
"))
(in-package "COM.INFORMATIMAGO.IOLIB.END-POINT")


(defclass end-point ()
  ()
  (:documentation "An abstract end-point."))

(defgeneric END-POINT-ADDRESS-UNSPECIFIED-P (end-point)
  (:documentation "Whether the end-point has an unspecified address.")
  (:method ((end-point t)) nil))


(defclass tcp-ipv4-end-point (end-point)
  ((interface :type (or string (vector * 4)
                        ;; IPV4-ADDRESS
                        )
              :initarg :interface
              :initform #(0 0 0 0)
              :accessor tcp-ipv4-end-point-interface)
   (port      :type (unsigned-byte 16)
              :initarg :port
              :initform 0
              :accessor tcp-ipv4-end-point-port))
  (:documentation "A TCP/IP v4 end-point."))


(defmethod print-object ((self tcp-ipv4-end-point) stream)
  "Prints a TCP-IPV4-END-POINT."
  (if *print-escape* 
      (print-unreadable-object (self stream :identity nil :type t)
        (ignore-errors
          (with-slots (interface port) self
            (format stream ":INTERFACE ~S :PORT ~S" interface port))))
      (with-slots (interface port) self
        (format stream "~A:~A" (address-to-string (ensure-address interface)) port)))
  self)


(defmethod END-POINT-ADDRESS-UNSPECIFIED-P ((end-point tcp-ipv4-end-point))
  "Predicate whether the END-POINT contains the unspecified address
\(designating any or all interfaces)."
  (address-equal-p +ipv4-unspecified+
                   (tcp-ipv4-end-point-interface end-point)))


(defclass unix-socket-end-point (end-point)
  ((pathname :type (or string pathname)
             :initarg :pathname
             :accessor unix-socket-end-point-pathname))
  (:documentation "A Unix socket end-point."))


(defmethod print-object ((self unix-socket-end-point) stream)
  "Prints a UNIX-SOCKET-END-POINT."
  (if *print-escape*
      (print-unreadable-object (self stream :identity nil :type t)
        (ignore-errors
          (if (slot-boundp self 'pathname)
              (with-slots (pathname) self
                (format stream ":PATHNAME ~S" pathname))
              (format stream ":PATHNAME #<UNBOUND>"))))
      (if (slot-boundp self 'pathname)
          (with-slots (pathname) self
            (format stream "socket:~S" pathname))
          (format stream "socket:#<UNBOUND>")))
  self)


(defclass pipe-end-point (end-point)
  ((read-file-descriptor  :type integer
                          :initarg :read-file-descriptor
                          :accessor pipe-end-point-read-file-descriptor)
   (write-file-descriptor :type integer
                          :initarg :write-file-descriptor
                          :accessor pipe-end-point-write-file-descriptor))
  (:documentation "A Unix pipe-pair end-point."))


(defmethod print-object ((self pipe-end-point) stream)
  "Prints a PIPE-END-POINT."
  (if *print-escape*
      (print-unreadable-object (self stream :identity nil :type t)
        (ignore-errors
          (with-slots (read-file-descriptor write-file-descriptor) self
            (format stream ":READ-FILE-DESCRIPTOR ~S :WRITE-FILE-DESCRIPTOR ~S"
                    read-file-descriptor write-file-descriptor))))
      (with-slots (read-file-descriptor write-file-descriptor) self
        (format stream "pipe:rd=~A:wr=~A"
                (if (slot-boundp self 'read-file-descriptor)
                    read-file-descriptor
                    "#<UNBOUND>")
                (if (slot-boundp self 'write-file-descriptor)
                    write-file-descriptor
                    "#<UNBOUND>"))))
  self)


(defgeneric end-point-address (end-point)
  
  (:documentation "The IOLib address object corresponding to the END-POINT.")
  
  (:method ((end-point tcp-ipv4-end-point))
    (ensure-address (tcp-ipv4-end-point-interface end-point)
                    :family :internet
                    :errorp t))
  
  (:method ((end-point unix-socket-end-point))
    (ensure-address (unix-socket-end-point-pathname end-point)
                    :family :local
                    :errorp t))
  
  (:method ((end-point pipe-end-point))
    (error "There's no iolib address for pipe end-points.")))



(defgeneric socket-local-end-point (socket)
  (:documentation "Given an IOLib socket, returns a corresponding local END-POINT.")

  (:method ((socket local-socket))
    (make-instance 'unix-socket-end-point
        :pathname (or (ignore-errors (local-filename socket))
                      "#<closed unix socket>")))

  (:method ((socket internet-socket))
    (if (socket-ipv6-p socket)
        (error "IPv6 end-points not implemented yet.")
        (make-instance 'tcp-ipv4-end-point
            :interface (or (ignore-errors (address-to-vector (ensure-address (local-host socket))))
                           "#<closed tcp/ipv4 socket>")
            :port (or (ignore-errors (local-port socket))
                      0)))))


(defgeneric socket-remote-end-point (socket)
  (:documentation "Given an IOLib socket, returns a corresponding remote END-POINT.")

  (:method ((socket local-socket))
    (make-instance 'unix-socket-end-point
        :pathname (or (ignore-errors (remote-filename socket))
                      "#<closed unix socket>")))

  (:method ((socket internet-socket))
    (if (socket-ipv6-p socket)
        (error "IPv6 end-points not implemented yet.")
        (make-instance 'tcp-ipv4-end-point
            :interface (or (ignore-errors (address-to-vector (ensure-address (remote-host socket))))
                           "#<closed tcp/ipv4 socket>")
            :port (or (ignore-errors (remote-port socket))
                      0)))))



(defgeneric listen-to-local-end-point (end-point)

  (:documentation "Creates a new iolib socket to listen to the given local end-point.")
  
  (:method ((end-point tcp-ipv4-end-point))
    (make-socket :address-family :internet ; ipv4
                 :type :stream ; tcp
                 :connect :passive ; listen
                 :ipv6 nil ; ipv4
                 :local-host (end-point-address end-point)
                 :local-port (tcp-ipv4-end-point-port end-point)
                 :backlog 5
                 :reuse-address t
                 ;; :keepalive t
                 ;; :nodelay nil
                 ;; :interface "eth0"
                 ;; :input-buffer-size 4096
                 ;; :output-buffer-size 4096
                 ))
  
  (:method ((end-point unix-socket-end-point))
    (make-socket :address-family :local ; unix-socket
                 :type :stream 
                 :connect :passive ; listen
                 :local-filename (end-point-address end-point)
                 :backlog 5
                 :reuse-address t
                 ;; :keepalive t
                 ;; :nodelay nil
                 ;; :input-buffer-size 4096
                 ;; :output-buffer-size 4096
                 ))

  (:method ((end-point pipe-end-point))
    (error "Cannot listen to a pipe.")))



(defgeneric connect-to-remote-end-point (end-point)

  (:documentation "Creates a new iolib socket to listen to the given local end-point.")
  
  (:method ((end-point tcp-ipv4-end-point))
    (let ((socket (make-socket :address-family :internet ; ipv4
                               :type :stream             ; tcp
                               :connect :active          ; connect
                               :ipv6 nil                 ; ipv4
                               :reuse-address t
                               ;; :remote-host (end-point-address end-point)
                               ;; :remote-port (tcp-ipv4-end-point-port end-point)
                               ;; :backlog 5
                               ;; :keepalive t
                               ;; :nodelay nil
                               ;; :interface "eth0"
                               ;; :input-buffer-size 4096
                               ;; :output-buffer-size 4096
                               )))
      (connect socket (end-point-address end-point) 
               :port (tcp-ipv4-end-point-port end-point)
               :wait t
               :timeout 2)
      socket))
  
  (:method ((end-point unix-socket-end-point))
    (make-socket :address-family :local ; unix-socket
                 :type :stream 
                 :connect :active ; connect
                 :remote-filename (end-point-address end-point)
                 ;; :reuse-address t
                 ;; :backlog 5
                 ;; :keepalive t
                 ;; :nodelay nil
                 ;; :input-buffer-size 4096
                 ;; :output-buffer-size 4096
                 ))

  (:method ((end-point pipe-end-point))
    (error "Cannot connect to a pipe.")))

;;;; THE END ;;;;
