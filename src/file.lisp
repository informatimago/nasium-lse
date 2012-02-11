;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               file.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements L.S.E. data files.
;;;;
;;;;    L.S.E data files are record based, and may store a number, a
;;;;    vector of numbers, an array of numbers or a string into each
;;;;    record.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-11 <PJB> Created.
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

(in-package "COM.INFORMATIMAGO.LSE")

(defun peek32 (vector offset)
  (dpb (aref vector offset) (byte 8 24)
       (dpb (aref vector (+ 1 offset)) (byte 8 16)
            (dpb (aref vector (+ 2 offset)) (byte 8 8)
                 (aref vector (+ 3 offset))))))

(defun peek24 (vector offset)
  (dpb (aref vector  offset) (byte 8 16)
       (dpb (aref vector (+ 1 offset)) (byte 8 8)
            (aref vector (+ 2 offset)))))

(defun peek16 (vector offset)
  (dpb (aref vector offset) (byte 8 8)
       (aref vector (+ 1 offset))))

(defun peek8 (vector offset)
  (aref vector offset))


(defun (setf peek32) (new-value vector offset)
  (setf (aref vector      offset)  (ldb (byte 8 24) new-value)
        (aref vector (+ 1 offset)) (ldb (byte 8 16) new-value)
        (aref vector (+ 2 offset)) (ldb (byte 8  8) new-value)
        (aref vector (+ 3 offset)) (ldb (byte 8  0) new-value))
  new-value)

(defun (setf peek24) (new-value vector offset)
  (setf (aref vector      offset)  (ldb (byte 8 16) new-value)
        (aref vector (+ 1 offset)) (ldb (byte 8  8) new-value)
        (aref vector (+ 2 offset)) (ldb (byte 8  0) new-value))
  new-value)

(defun (setf peek16) (new-value vector offset)
  (setf (aref vector      offset)  (ldb (byte 8  8) new-value)
        (aref vector (+ 1 offset)) (ldb (byte 8  0) new-value))
  new-value)

(defun (setf peek8) (new-value vector offset)
  (setf (aref vector offset) (ldb (byte 8 0) new-value))
  new-value)



(defclass file ()
  ((path                       :initarg :path  :accessor file-path)
   (stream       :initform nil :initarg :stream)
   (header       :initform nil)
   (record-table :initform nil)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +type-number+ 1)
  (defconstant +type-vector+ 2)
  (defconstant +type-array+  3)
  (defconstant +type-string+ 4)

  (defconstant +block-size+       1024)
  (defconstant +header-position+  (- +block-size+ 8)))

(defun make-block () (make-array +block-size+ :element-type '(unsigned-byte 8) :initial-element 0))

(defstruct header
  free-list     ; file position of the first block in the free list.
  record-table) ; file position of the record table.



(defun read-block (buffer stream position)
  (file-position stream position)
  (let ((size (read-sequence buffer stream)))
    (= (length buffer) size)))

(defmethod read-header ((file file))
  (let ((buffer (make-array 8 :element-type '(unsigned-byte 8))))
    (with-slots (stream header) file
      (file-position stream +header-position+)
      (assert (= (length buffer) (read-sequence buffer stream)))
      (setf header (make-header :free-list    (peek32 buffer 0)
                                :record-table (peek32 buffer 4))))))

(defmethod write-header ((file file))
  (let ((buffer (make-array 8 :element-type '(unsigned-byte 8))))
    (with-slots (stream header) file
     (file-position stream +header-position+)
     (setf (peek32 buffer 0) (header-free-list    header)
           (peek32 buffer 4) (header-record-table header))
     (write-sequence buffer stream)
     header)))


(defun valid-type-p (type-code)
   (<= +type-number+ type-code +type-string+))

(defmethod build-record-table ((file file))
  (with-slots (stream header record-table) file
    (file-position stream +block-size+)
    (let ((buffer (make-block))
          (table (make-hash-table))
          (free-list 0))
      (loop
        :for position :from +block-size+ :by +block-size+
        :do (if (= +block-size+ (read-sequence buffer stream))
                (let ((record-number (peek24 buffer 0)))
                  (if (or (zerop record-number)
                          (not (valid-type-p (peek8 buffer 3)))) 
                      (progn ; deleted or invalid record, add it to the free-list.
                        (setf (peek32 buffer 0) 0
                              (peek32 buffer 4) free-list
                              free-list position)
                        (fill buffer 0 :start 8)
                        (file-position stream position)
                        (write-sequence buffer stream))
                      ;; valid record
                      (setf (gethash record-number table) position)))
                ;; end of file
                (loop-finish))
        :finally (setf header (make-header :free-list free-list
                                           :record-table position)
                       record-table table)))))


(defmethod read-record-table ((file file))
  (with-slots (stream header record-table) file
    (unless header
      (read-header file))
    (if (zerop (header-record-table header))
        (build-record-table file)
        (let ((table (make-hash-table))
              (buffer (make-block)))
          (loop
            :for position :from (header-record-table header) :by (length buffer)
            :while (read-block buffer stream position)
            :until (loop
                     :for i :below +block-size+ :by 8
                     :for rn = (peek32 buffer i)
                     :for rp = (peek32 buffer (+ i 4))
                     :until (zerop rn)
                     :do (setf (gethash rn table) rp)
                     :finally (return (zerop rn))))
          (setf record-table table)))))

(defmethod write-record-table ((file file))
  (with-slots (stream header record-table) file
    (unless header
      (read-header file))
    (unless (zerop (header-record-table header))
      (build-record-table file))
    (let ((buffer (make-block)))
      (file-position stream (header-record-table header))
      (with-hash-table-iterator (next record-table)
        (loop 
          :until (loop
                   :with done = nil
                   :for i :below +block-size+ :by 8
                   :do (multiple-value-bind (got-it key value) (next)
                         (if got-it
                             (setf (peek32 buffer i)       key
                                   (peek32 buffer (+ i 4)) value)
                             (progn
                               (setf (peek32 buffer i)       0
                                     (peek32 buffer (+ i 4)) 0
                                     done t)
                               (loop-finish))))
                   :finally (progn
                              (write-sequence buffer stream)
                              (return done))))))))

(define-condition file-error-record-already-exists (file-error)
  ((file :initarg :file :accessor file-error-file)
   (record-number :initarg :record-number :accessor file-error-record-number)))

(defmethod %allocate-record ((file file) record-number)
  "Allocates a new record.  Take it from the free list, or if it's empty, from the end of file (record-table)."
  (with-slots (stream header record-table) file
    (when (gethash record-number record-table)
      (error 'file-error-record-already-exists
             :pathname (file-path file)
             :file file
             :record-number record-number))
    (let ((position (if (zerop (header-free-list header))
                        (if (zerop (header-record-table header))
                            (ceiling (file-length stream) +block-size+)
                             (header-record-table header))
                        (let ((buffer (make-block)))
                          (read-block buffer stream (header-free-list header))
                          (prog1 (header-free-list header)
                            (setf (header-free-list header) (peek32 buffer 4)))))))
      (setf (gethash record-number record-table) position))))


(defun lse-data-file-open (filespec &key (if-exists :append) (if-does-not-exist :create))
  (flet ((create ()
           (let ((stream (open filespec
                               :direction :io
                               :element-type '(unsigned-byte 8)
                               :if-exists if-exists
                               :if-does-not-exist :create))
                 (buffer (make-block)))
             (replace buffer (ascii-format nil "L.S.E. Data File~%Version 1.0~2%"))
             (file-position stream 0)
             (write-sequence buffer stream)
             stream)))
    (let ((file (make-instance 'file
                    :path (pathname filespec)
                    :stream (if (probe-file filespec)
                                (case if-exists
                                  ((:append) (open filespec
                                                   :direction :io
                                                   :element-type '(unsigned-byte 8)
                                                   :if-exists if-exists
                                                   :if-does-not-exist :create))
                                  ((nil)     (return-from lse-data-file-open nil))
                                  (otherwise (create)))
                                (ecase if-does-not-exist
                                  ((:create)  (create))
                                  ((:error)   (open filespec
                                                    :direction :io
                                                    :element-type '(unsigned-byte 8)
                                                    :if-exists :error
                                                    :if-does-not-exist :error))
                                  ((nil)     (return-from lse-data-file-open nil)))))))
      (with-slots (header record-table) file
        (setf header (read-header file)
              record-table (read-record-table file)))
      file)))

(defmethod lse-data-file-close ((file file))
  (with-slots (stream) file
    (when stream
      (write-record-table file)
      (write-header file)
      (close stream)
      (setf stream nil))))


(defmethod lse-data-file-open-p ((file file))
  (with-slots (stream) file
    (not (null stream))))



(defmacro gen-ieee-encoding (name type exponent-bits mantissa-bits)
  ;; Thanks to ivan4th (~ivan_iv@nat-msk-01.ti.ru) for correcting an off-by-1
  `(progn
     (defun ,(intern (format nil "~A-TO-IEEE-754" name) (symbol-package name))  (float)
       (multiple-value-bind (mantissa exponent sign) 
           (integer-decode-float float)
         (dpb (if (minusp sign) 1 0)
              (byte 1 ,(1- (+ exponent-bits mantissa-bits)))
              (dpb (+ ,(+ (- (expt 2 (1- exponent-bits)) 2) mantissa-bits)
                      exponent)
                   (byte ,exponent-bits ,(1- mantissa-bits))
                   (ldb (byte ,(1- mantissa-bits) 0) mantissa)))))
     (defun ,(intern (format nil "IEEE-754-TO-~A" name) (symbol-package name))  (ieee)
       (let ((aval (scale-float
                    (coerce
                     (dpb 1 (byte 1 ,(1- mantissa-bits))
                          (ldb (byte ,(1- mantissa-bits) 0) ieee))
                     ,type)
                    (- (ldb (byte ,exponent-bits ,(1- mantissa-bits))
                            ieee) 
                       ,(1- (expt 2 (1- exponent-bits)))
                       ,(1- mantissa-bits)))))
         (if (zerop (ldb (byte 1 ,(1- (+ exponent-bits mantissa-bits))) ieee))
             aval
             (- aval))))))


(gen-ieee-encoding float-32 'single-float  8 24)
;; (gen-ieee-encoding float-64 'double-float 11 53)



(defmethod read-record ((file file) record-number)
  "
RETURN: The data; a status code
"
  (with-slots (stream record-table) file
    (let ((position (gethash record-number record-table)))
      (if (null position)
          (values nil -1)
          (let ((buffer (make-block)))
            (read-block buffer stream position)
            (if (= record-number (peek24 buffer 0))
                (case (peek8 buffer 3)
                  ((#.+type-number+)
                   (values (ieee-754-to-float-32 (peek32 buffer 4)) (1- +type-number+)))
                  ((#.+type-vector+)
                   (let* ((size (peek16 buffer 4))
                          (data (make-array size :element-type 'nombre)))
                     (loop
                       :for i :below size
                       :for j :from 6 :by 4
                       :do (setf (aref data i) (ieee-754-to-float-32 (peek32 buffer j))))
                     (values data (1- +type-vector+))))
                  ((#.+type-array+)
                   (let* ((dim1 (peek16 buffer 4))
                          (dim2 (peek16 buffer 6))
                          (data (make-array (list dim1 dim2) :element-type 'nombre)))
                     (loop
                       :for i :below (* dim1 dim2)
                       :for j :from 8 :by 4
                       :do (setf (row-major-aref data i) (ieee-754-to-float-32 (peek32 buffer j))))
                     (values data (1- +type-array+))))
                  ((#.+type-string+)
                   
                   (let* ((size (peek16 buffer 4))
                          (data (babel:octets-to-string buffer
                                                        :start 6 :end (+ 6 size)
                                                        :encoding :utf-8)))
                     (values data (1- +type-string+))))
                  (otherwise
                   (values nil -1)))
                (values nil -1)))))))

(defmethod write-record ((file file) record-number data)
  (check-type data (or nombre vecteur tableau chaine))
  (with-slots (stream record-table) file
    (let ((position (or (gethash record-number record-table)
                        (%allocate-record file record-number)))
          (buffer (make-block)))
      (file-position stream position)
      (setf (peek24 buffer 0) record-number)
      (etypecase data
        (nombre  (setf (peek8  buffer 3) +type-number+
                       (peek32 buffer 4) (float-32-to-ieee-754 data)))
        (vecteur (assert (<= (length data) #.(truncate (- +block-size+ 6) 4)))
                 (setf (peek8  buffer 3) +type-vector+
                       (peek16 buffer 4) (length data))
                 (loop
                   :for i :below (length data)
                   :for j :from 6 :by 4
                   :do (setf (peek32 buffer j) (float-32-to-ieee-754 (aref data i)))))
        (tableau (assert (<= (array-total-size data) #.(truncate (- +block-size+ 8) 4)))
                 (setf (peek8  buffer 3) +type-array+
                       (peek16 buffer 4) (array-dimension data 0)
                       (peek16 buffer 6) (array-dimension data 1))
                 (loop
                   :for i :below (array-total-size data)
                   :for j :from 8 :by 4
                   :do (setf (peek32 buffer j) (float-32-to-ieee-754 (row-major-aref data i)))))
        (chaine  (let ((octets (string-to-octets data :encoding :utf-8)))
                   (assert (<= (length octets) #. (truncate (- +block-size+ 6))))
                   (setf (peek8  buffer 3) +type-string+
                         (peek16 buffer 4) (length octets))
                   (replace buffer octets :start1 6))))
      (write-sequence buffer stream))))


(defmethod delete-record ((file file) record-number)
  (with-slots (stream record-table header) file
    (let ((position (gethash record-number record-table)))
      (if position
          (let ((buffer (make-block)))
            (read-block buffer stream position)
            (setf (peek32 buffer 0) 0
                  (peek32 buffer 4) (header-free-list header)
                  (header-free-list header) position)
            (remhash record-number record-table)
            (write-sequence buffer stream)
            (values 0))
          (values -1)))))



(defun test/dump-lse-file (path)
  (let ((file (lse-data-file-open path)))
    (unwind-protect
         (loop
           :for rn :from 1 :to 1000
           :do (multiple-value-bind (data code) (read-record file rn)
                 (ecase code
                   (-1 #|no record, skip|#)
                   (0 (format t "~4D: NOMBRE  = ~S~%" rn data))
                   (1 (format t "~4D: VECTEUR = ~S~%" rn data))
                   (2 (format t "~4D: TABLEAU = ~S~%" rn data))
                   (3 (format t "~4D: CHAINE  = ~S~%" rn data)))
                 (finish-output)))
      (lse-data-file-close file))))


(defun test/file ()
  (let ((path  "/tmp/test.don"))
    (let ((file (lse-data-file-open path)))
      (unwind-protect
           (progn
             (write-record file 11 3.1415)
             (write-record file 21 (make-array 4
                                               :element-type 'nombre
                                               :initial-contents '(1.1 2.2 3.3 4.4)))
             (write-record file 31 (make-array '(4 2)
                                               :element-type 'nombre
                                               :initial-contents '((1.1 1.2)
                                                                    (2.1 2.2)
                                                                    (3.1 3.2)
                                                                    (4.1 4.2))))
             (write-record file 40 "Hello world!"))
        (lse-data-file-close file))))
  (test/dump-lse-file  "/tmp/test.don"))


;; (inspect (make-instance 'file :path "/tmp/p.don" :stream (open "/tmp/p.don"
;;                                                                :direction :io
;;                                                                :if-exists :append
;;                                                                :if-does-not-exist :create)))

;;;; THE END ;;;;
