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
;;;;
;;;;    The file format and this implementation doesn't allow multiple
;;;;    simultaneous accesses to the files.  This would be a desirable
;;;;    features to implement multiple-console accesing the same file
;;;;    (eg. for console to console communication).
;;;;
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

(defmethod print-object ((self file) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream ":OPEN ~S :PATH ~S"
            (lse-data-file-open-p self)
            (file-path self)))
  self)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +type-number+ 1)
  (defconstant +type-vector+ 2)
  (defconstant +type-array+  3)
  (defconstant +type-string+ 4)


  ;; On T1600, the strings a limited to 256 characters, so there's no
  ;; point in having bigger blocks.
  ;; On  Mitra-15 strings are unlimited, so we can have bigger blocks.
  ;; See functions.lisp.
  ;; On the other hand, we should not have different block sizes if
  ;; we're not prepared to read files from any block size (to be able
  ;; to exchange files).
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
      (read-block buffer stream +header-position+)
      (setf header (make-header :free-list    (peek32 buffer 0)
                                :record-table (peek32 buffer 4))))))

(defmethod write-header ((file file))
  (let ((buffer (make-array 8 :element-type '(unsigned-byte 8))))
    (with-slots (stream header) file
      (setf (peek32 buffer 0) (header-free-list    header)
            (peek32 buffer 4) (header-record-table header))
      (file-position stream +header-position+)
      (write-sequence buffer stream)
      (force-output stream)
      header)))


(defun valid-type-p (type-code)
   (<= +type-number+ type-code +type-string+))

(defmethod build-record-table ((file file))
  (with-slots (stream header record-table) file
    (let ((buffer (make-block))
          (table (make-hash-table))
          (free-list 0))
      (loop
        :for position :from +block-size+ :by +block-size+
        :while (< position (file-length stream))
        :do (if (read-block buffer stream position)
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
        :finally (force-output stream)
                 (return (setf header (make-header :free-list free-list
                                                   :record-table position)
                               record-table table))))))


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
    (when (zerop (header-record-table header))
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
                              (force-output stream)
                              (return done))))))))

(define-condition lse-file-error (lse-error file-error)
  ;; Note: from file-error we inherit :pathname and file-error-pathname.
  ((file          :initarg :file
                  :accessor file-error-file
                  :initform nil
                  :documentation "The FILE object in error.")
   (record-number :initarg :record-number
                  :accessor file-error-record-number
                  :initform nil
                  :documentation "The record number in error.")))


(define-condition file-error-record-already-exists (lse-file-error)
  ()
  (:report (lambda (err stream)
             (format stream "L'ENREGISTREMENT ~A EXISTE DEJA DANS LE FICHIER ~S."
                     (file-error-record-number err)
                     (file-error-pathname err)))))

(define-condition file-error-file-does-not-exist   (lse-file-error)
  ()
  (:report (lambda (err stream)
             (format stream "LE FICHIER ~S N'EXISTE PAS."
                     (file-error-pathname err)))))

(define-condition file-error-file-is-inaccessible  (lse-file-error)
  ()
  (:report (lambda (err stream)
             (format stream "LE FICHIER ~S EST INACCESSIBLE."
                     (file-error-pathname err)))))

(define-condition file-error-file-already-exists   (lse-file-error)
  ()
  (:report (lambda (err stream)
             (format stream "LE FICHIER ~S EXISTE DEJA."
                     (file-error-pathname err)))))



(defmethod %allocate-record ((file file) record-number)
  "Allocates a new record.  Take it from the free list, or if it's empty, from the end of file (record-table)."
  (with-slots (stream header record-table) file
    (when (gethash record-number record-table)
      (error 'file-error-record-already-exists
             :backtrace (or #+ccl (ccl::backtrace-as-list))
             :pathname (file-path file)
             :file file
             :record-number record-number))
    (let ((position (if (zerop (header-free-list header))
                        (if (zerop (header-record-table header))
                            (* (ceiling (file-length stream) +block-size+)
                               +block-size+)
                            (prog1 (header-record-table header)
                              (setf (header-record-table header) 0)))
                        (let ((buffer (make-block)))
                          (read-block buffer stream (header-free-list header))
                          (prog1 (header-free-list header)
                            (setf (header-free-list header) (peek32 buffer 4)))))))
      (setf (gethash record-number record-table) position))))


(defun lse-data-file-open (filespec &key (if-exists :append) (if-does-not-exist :create))
  (let ((stream (open filespec
                      :direction :io
                      :element-type '(unsigned-byte 8)
                      :if-exists if-exists
                      :if-does-not-exist if-does-not-exist
                      #+ccl :sharing #+ccl :lock)))
    (when stream
      (when (zerop (file-length stream))
        (let ((buffer (make-block)))
          (replace buffer (let ((*newline* ':lf))
                            (ascii-format nil "L.S.E. Data File~%Version: 1.0.1~25%")))
          (file-position stream 0)
          (write-sequence buffer stream)
          (force-output stream)))
      (let ((file (make-instance 'file
                      :path (pathname filespec)
                      :stream stream)))
        (with-slots (header record-table) file
          (setf header (read-header file)
                record-table (read-record-table file)))
        file))))

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
     (defun ,(intern (with-standard-io-syntax (format nil "~A-TO-IEEE-754" name)) (symbol-package name))  (float)
       (multiple-value-bind (mantissa exponent sign) 
           (integer-decode-float float)
         (dpb (if (minusp sign) 1 0)
              (byte 1 ,(1- (+ exponent-bits mantissa-bits)))
              (dpb (+ ,(+ (- (expt 2 (1- exponent-bits)) 2) mantissa-bits)
                      exponent)
                   (byte ,exponent-bits ,(1- mantissa-bits))
                   (ldb (byte ,(1- mantissa-bits) 0) mantissa)))))
     (defun ,(intern (with-standard-io-syntax (format nil "IEEE-754-TO-~A" name)) (symbol-package name))  (ieee)
       (if (zerop ieee)
           0.0f0
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
                 (- aval)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; (gen-ieee-encoding float-64 'double-float 11 53)
  (gen-ieee-encoding float-32 'single-float  8 24))


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

(defparameter *max-record-chaine-size*  (truncate (- +block-size+ 6))
  "Maximum number of UTF-8 bytes in a record.")
(defparameter *max-record-tableau-size* (truncate (- +block-size+ 8) 4)
  "Maximum number of 2D array slots in a record.")
(defparameter *max-record-vecteur-size* (truncate (- +block-size+ 6) 4)
  "Maximum number of 1D vector slots in a record.")

(defmethod write-record ((file file) record-number data)
  (check-type data (or nombre vecteur tableau chaine))
  (with-slots (stream record-table) file
    (let ((position (or (gethash record-number record-table)
                        (%allocate-record file record-number)))
          (buffer (make-block)))
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
                   (assert (<= (length octets) #.(truncate (- +block-size+ 6))))
                   (setf (peek8  buffer 3) +type-string+
                         (peek16 buffer 4) (length octets))
                   (replace buffer octets :start1 6))))
      ;; (format t "p=~8D <-- ~S~%" position (subseq buffer 0 40))
      (file-position stream position)
      (write-sequence buffer stream)
      (force-output stream)
      position)))


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
            (file-position stream position)
            (write-sequence buffer stream)
            (force-output stream)
            (values 0 position))
          (values -1 position)))))



;;;---------------------------------------------------------------------
;;; Test functions
;;;---------------------------------------------------------------------

(defun record-table-as-sorted-list (record-table)
  (sort (let ((pairs '()))
          (maphash (lambda (k v) (push (list k v) pairs)) record-table)
          pairs)
        '< :key 'first))


(defun dump-record-table (file)
  (with-slots (record-table) file
    (cond
      ((null record-table)
       (format t "Record table is NIL.~%"))
      ((zerop (hash-table-count record-table))
       (format t "Record table is empty.~%"))
      (t
       (format t "Record table:~%~:{    rn=~8D p=~8D~%~}"
               (record-table-as-sorted-list record-table))))))


(defun check-record-table (record-table)
  (when record-table
   (let ((record-list (record-table-as-sorted-list record-table)))
     (loop
       :for (rn position) :in record-list
       :do (unless (zerop (mod position +block-size+))
             (format t "ERROR: rn=~8D p=~8D is not a multiple of ~D (remainder= ~D)~%"
                     rn position +block-size+ (mod position +block-size+)))))))


(defun dump-lse-file (path)
  (let ((file (lse-data-file-open path)))
    (unwind-protect
         (with-slots (record-table) file
           (check-record-table record-table)
           (loop
             :for rn :from 1 :to 1000
             :do (multiple-value-bind (data code) (read-record file rn)
                   (ecase code
                     (-1 #|no record, skip|#)
                     (0 (format t "~4D: NOMBRE  = ~S~%" rn data))
                     (1 (format t "~4D: VECTEUR = ~S~%" rn data))
                     (2 (format t "~4D: TABLEAU = ~S~%" rn data))
                     (3 (format t "~4D: CHAINE  = ~S~%" rn data)))
                   (finish-output))))
      (lse-data-file-close file))))


(defun dump-lse-file/low-level (path)
  (with-open-file (stream path)
    (loop :repeat 3
      :do (write-line (read-line stream))))
  (let ((file (lse-data-file-open path)))
    (unwind-protect
         (with-slots (stream header record-table) file
           (flet ((rem-or-nil (p) (let ((r (mod p +block-size+))) (unless (zerop r) r))))
            (format t "Header:~%~
                     ~&    free-list:    ~8D ~@[invalid, remainder=~D~]~%~
                     ~&    record-table: ~8D ~@[invalid, remainder=~D~]~%"
                    (header-free-list    header) (rem-or-nil (header-free-list    header))
                    (header-record-table header) (rem-or-nil (header-record-table header))))
           (dump-record-table file)
           (format t "Records:~%")
           (let ((buffer        (make-block))
                 (table         (make-hash-table)) ; record-number -> position
                 (free-table    (make-hash-table)) ; position -> (position . seen)
                 (invalid-type-count 0)
                 (invalid-data-count 0))
             (loop
               :for position :from +block-size+ :by +block-size+
               :while (or (zerop (header-record-table header))
                          (< position (header-record-table header)))
               :do (if (read-block buffer stream position) 
                       (let ((record-number (peek24 buffer 0)))
                         (if (zerop record-number)
                             (progn
                               (unless (zerop (peek8 buffer 3))
                                 (format t "~8D:  nr=~8D  Invalid record type ~D for free block~%"
                                         position record-number (peek8 buffer 3))
                                 (incf invalid-type-count))
                               (setf (gethash position free-table) (cons (peek32 buffer 4) nil))
                               (format t "~8D:  free block   next position=~8D~%"
                                       position (peek32 buffer 4))
                               (when (gethash position record-table)
                                 (format t "ERROR:  Free block in the record table, at position ~8D~%"
                                         position)))
                             (progn
                               (if (valid-type-p (peek8 buffer 3))
                                   (format t "~8D:  nr=~8D  ~[FREE BLOCK~;NOMBRE~;VECTEUR~;TABLEAU~;CHAINE~:;INVALID~]~%"
                                           position record-number (peek8 buffer 3))
                                   (progn
                                     (format t "~8D:  nr=~8D  Invalid record type ~D~%"
                                             position record-number (peek8 buffer 3))
                                     (incf invalid-type-count)))
                               (setf (gethash record-number table) position)

                               ;; (check-data)
                               ;; (loop
                               ;;   :for rn :from 1 :to 1000
                               ;;   :do (multiple-value-bind (data code) (read-record file rn)
                               ;;         (ecase code
                               ;;           (-1 #|no record, skip|#)
                               ;;           (0 (format t "~4D: NOMBRE  = ~S~%" rn data))
                               ;;           (1 (format t "~4D: VECTEUR = ~S~%" rn data))
                               ;;           (2 (format t "~4D: TABLEAU = ~S~%" rn data))
                               ;;           (3 (format t "~4D: CHAINE  = ~S~%" rn data)))
                               ;;         (finish-output)))
                               )))
                       ;; end of file
                       (progn
                         (format t "~8D:  end of file~%" position)
                         (format t "~8D:  file size~%" (file-length stream))
                         (when (/= position (file-length stream))
                           (format t "WARNING: Incomplete block at end of file.~%"))
                         (loop-finish)))
               :finally (progn
                          (loop
                            :for current = (header-free-list header) :then next
                            :for entry   = (gethash current free-table)
                            :for next    = (car entry)
                            :for seen    = (cdr entry)
                            :while (and current (plusp current))
                            :do (progn
                                  (when seen
                                    (format t "ERROR:  The free list is circular.~%")
                                    (loop-finish))
                                  (when entry
                                    (setf (cdr entry) t)))
                            :finally (maphash (lambda (position entry)
                                                (let ((seen (cdr entry)))
                                                  (unless seen
                                                    (format t "ERROR:  Free block not in free-list, at position ~8D~%"
                                                            position))))
                                              free-table))
                          (unless (equalp table record-table)
                            (format t "ERROR:  The record-table is incorrect.~%")
                            (let ((rt-elements (record-table-as-sorted-list record-table))
                                  (ft-elements (record-table-as-sorted-list table)))
                              (loop
                                :with cur-rt = rt-elements
                                :with cur-ft = ft-elements
                                :initially (format t "Differences between the record table and the file contents:~%")
                                :while (and cur-rt cur-ft)
                                :do (cond
                                      ((= (car cur-rt) (car cur-ft))
                                       (unless (= (cdr cur-rt) (cdr cur-ft))
                                         (format t "    rn=~8D  position in record table: ~8D,  position in file: ~8D~%"
                                                 (car cur-rt) (cdr cur-rt) (cdr cur-ft)))
                                       (pop cur-rt)
                                       (pop cur-ft))
                                      ((< (car cur-rt) (car cur-ft))
                                       (format t "    rn=~8D  is in record table, position: ~8D, but no in the file.~%"
                                               (car cur-rt) (cdr cur-rt))
                                       (pop cur-rt))
                                      (t
                                       (format t "    rn=~8D  is in file table, position: ~8D, but no in the record table.~%"
                                               (car cur-ft) (cdr cur-ft))
                                       (pop cur-ft)))
                                :finally
                                (progn
                                  (loop
                                   :for last-rt :in cur-rt
                                   :do (format t "    rn=~8D  is in record table, position: ~8D, but no in the file.~%"
                                               (car last-rt) (cdr last-rt)))
                                  (loop
                                    :for last-ft :in cur-ft
                                    :do (format t "    rn=~8D  is in file table,  position: ~8D, but no in the file.~%"
                                                (car last-ft) (cdr last-ft)))))))
                          (format t "Number of records:                    ~8D~%" (hash-table-count record-table))
                          (format t "Number of records with invalid type:  ~8D~%" invalid-type-count)
                          (format t "Number of records with invalid data:  ~8D~%" invalid-data-count)))))
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
             (write-record file 400 "Hello world!")
             (write-record file 401 "Félicitation!  45 ¥ ↑∀δ∈D, δ≡ε₂↓")

             (loop
               :for nr :from 100 :to 110
               :do (write-record file nr (format nil "Record ~D: contenu: ~D" nr nr)))
             (loop
               :for nr :from 100 :to 110 :by 2
               :do (delete-record file nr))
             (loop
               :for nr :from 200 :to 210
               :do (progn
                     (write-record file nr (format nil "Record ~D: contenu: ~D" nr nr))
                     (when (zerop (mod nr 3))
                       (delete-record file (- nr 1))))))
        
        (lse-data-file-close file))))
  (dump-lse-file  "/tmp/test.don"))


;; (inspect (make-instance 'file :path "/tmp/p.don" :stream (open "/tmp/p.don"
;;                                                                :direction :io
;;                                                                :if-exists :append
;;                                                                :if-does-not-exist :create)))

;; (dump-lse-file/low-level "/tmp/lse1000/test3.don" )

;;;; THE END ;;;;
