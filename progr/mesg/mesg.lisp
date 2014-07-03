;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               mesg.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    An implementation of the programme MESG written in Lisp.
;;;;    The program mesg.lse is a translation from this program.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2014-06-29 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2014 - 2014
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

#||
(ql:quickload :split-sequence)
(ql:quickload :md5)
(ql:quickload :com.informatimago.lse)
||#

(defpackage "COM.INFORMATIMAGO.LSE.MESG"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.LSE"
        "SPLIT-SEQUENCE"
        "MD5")
  (:export "MESG"))
(in-package "COM.INFORMATIMAGO.LSE.MESG")

;; 1*AUTEUR: PASCAL BOURGUIGNON
;; 2*DESCRIPTION: ECHANGE DE MESSAGE ENTRE UTILISATEURS DU SYSTEME LSE

(defvar *user-file-pathname* #P"~/users.don")
(defvar *mesg-file-pathname* #P"~/mesgs.don")

(defparameter *us*        (code-char 31))
(defparameter *max-users* (truncate *MAX-RECORD-TABLEAU-SIZE* 2))
(defparameter *max-mesgs* (1- *MAX-RECORD-VECTEUR-SIZE*))

;; * MAX 254 SLOTS PER RECORD FOR ARRAYS. 2 SLOT PER USER = 127 USERS.
;; *
;; * #USERS
;; * 1:"USERS|1"
;; * 2:[MAX-RECNO,FREE-LIST-RECNO,NUSER];; * 3:[[UID1,USER1-RECNO],[UID2,USER2-RECNO],[UID3,USER3-RECNO],…]
;; * 4:"U|uid|name|md5pass|MAIL+USER"
;; * 5:next-free
;; * 


(defun intarray (array)
  (let ((size (reduce (function *) (array-dimensions array))))
    (values (array-displacement
             (map-into (make-array size :displaced-to (make-array (array-dimensions array)
                                                                  :element-type 'integer))
                       (function round)
                       (make-array size :displaced-to array))))))

(defun intlist (array)
  (let ((size (reduce (function *) (array-dimensions array))))
    (map 'list (function round)
      (make-array size :displaced-to array))))


(defun file/new-record (file)
  (let ((toc (read-record file 2)))
    (destructuring-bind (max-recno free-list nusers) (intlist toc)
      (declare (ignore nusers))
      (if (zerop free-list)
          (prog1 (incf max-recno)
            (setf (aref toc 0) (coerce max-recno 'nombre))
            (write-record file 2 toc))
          (prog1 free-list
            (let ((new-free-list (read-record file free-list)))
              (setf (aref toc 1) new-free-list)
              (write-record file 2 toc)))))))

(defun file/free-record (file n)
  (let ((toc (read-record file 2))
        (n (round n)))
    (destructuring-bind (max-recno free-list nusers) (intlist toc)
      (declare (ignore nusers))
      (assert (<= 3 n max-recno))
      (write-record file n (coerce free-list 'nombre))
      (setf (aref toc 1) (coerce n 'nombre))
      (write-record file 2 toc)
      (values))))





(defvar *user-file* nil)

(defstruct (user (:type list))
  (type "U") uid name md5pass rights)

(defun users/open ()
  (setf *user-file* (lse-data-file-open *user-file-pathname* :if-does-not-exist :create))
  (let* ((header (read-record *user-file* 1))
         (fields (split-sequence *us* header :remove-empty-subseqs t)))
    (if (null fields)
        (progn
          (write-record *user-file* 1 (format nil "USERS~C1" *us*)) 
          (write-record *user-file* 2 (coerce #(3.0 0.0 0.0) 'vecteur))
          (write-record *user-file* 3 (make-array (list *max-users* 2) :element-type 'nombre)))
        (progn
          (assert (<= 2 (length fields)))
          (assert (string= (first fields) "USERS"))
          (assert (= 1 (parse-integer (second fields)))))))
  (values))


(defun users/close ()
  (lse-data-file-close *user-file*)
  (setf *user-file* nil)
  (values))


(defun users/new-record ()
  (unless *user-file* (users/open))
  (file/new-record *user-file*))

(defun users/free-record (n)
  (unless *user-file* (users/open))
  (file/free-record *user-file* n))



(defun users/query-users ()
  (unless *user-file* (users/open))
  (destructuring-bind (max-recno free-list nusers) (intlist (read-record *user-file* 2))
    (declare (ignore max-recno free-list))
    (values nusers (intarray (read-record *user-file* 3)))))

(defun record-for-uid (nusers users uid)
  (loop
    :for i :below nusers
    :do (when (= uid (aref users i 1))
          (return-from record-for-uid (values (aref users i 0) i))))
  (values nil nusers))


(defun users/query-user/uid (uid)
  (unless *user-file* (users/open))
  (destructuring-bind (max-recno free-list nusers) (intlist (read-record *user-file* 2))
    (declare (ignore max-recno free-list))
    (let* ((users (intarray (read-record *user-file* 3)))
           (recno (record-for-uid nusers users uid)))
      (if recno
          (values (split-sequence *us* (read-record *user-file* recno)) recno)
          (values)))))

(defun users/query-user/name (name)
  (unless *user-file* (users/open))
  (destructuring-bind (max-recno free-list nusers) (intlist (read-record *user-file* 2))
    (declare (ignore max-recno free-list))
    (let ((users (intarray (read-record *user-file* 3))))
      (loop
        :for i :below nusers
        :for recno = (aref users i 0)
        :for user = (split-sequence *us* (read-record *user-file* recno))
        :when (string= name  (user-name user))
          :do (setf (user-uid user) (parse-integer (user-uid user)))
              (return-from users/query-user/name (values user recno)))
      (values))))



(defun new-uid (nusers users)
  (if (zerop nusers)
      1
      (1+ (loop
            :for i :below nusers
            :maximize (round (aref users i 1))))))

(defun string-md5sum (string)
  (format nil "~{~2,'0X~}" (coerce (MD5SUM-STRING string) 'list)))

(defun users/insert-user (name pass rights)
  (unless *user-file* (users/open))
  (when (users/query-user/name name)
    (error "User with name ~S already exists." name))
  (let ((recno (users/new-record)) ; new-record modifies the toc, so it must be before reading it!
        (toc (read-record *user-file* 2)))
    (destructuring-bind (max-recno free-list nusers) (intlist toc)
      (declare (ignore max-recno free-list))
      (unless (< nusers *max-users*)
        (error "Too many users"))
      (let* ((users (read-record *user-file* 3))
             (uid   (new-uid nusers users))
             (record (format nil "U~C~D~C~A~C~A~C~A"
                             *us* uid
                             *us* name
                             *us* (string-md5sum (concatenate 'string name "|" pass))
                             *us* rights)))
        (write-record *user-file* recno record)
        (setf (aref users nusers 0) (coerce recno 'nombre)
              (aref users nusers 1) (coerce uid   'nombre)
              nusers (1+ nusers))
        (setf (aref toc 2) (coerce nusers 'nombre))
        (write-record *user-file* 3 users)
        (write-record *user-file* 2 toc)
        recno))))

(defun users/validate-user (name pass)
  (let ((user (users/query-user/name name)))
    (when user
      (equalp (string-md5sum (concatenate 'string name "|" pass))
              (user-md5pass user)))))

(defun users/delete-user (name)
  (unless *user-file* (users/open))
  (let ((user (users/query-user/name name)))
    (unless user
      (error "No user with name ~S exists." name))
    (let ((toc (read-record *user-file* 2)))
      (destructuring-bind (max-recno free-list nusers) (intlist toc)
        (declare (ignore max-recno free-list))
        (when (zerop nusers)
          (error "INCONSISTENCY: No user exist."))
        (let* ((users (read-record *user-file* 3))
               (uid   (user-uid user)))
          (multiple-value-bind (recno index) (record-for-uid nusers users uid)
            (when recno
              (loop
                :for i :from index :below (1- nusers)
                :do (setf (aref users i 0) (aref users (1+ i) 0)
                          (aref users i 1) (aref users (1+ i) 1))
                :finally (decf nusers)
                         (setf (aref users nusers 0) (coerce recno 'nombre)
                               (aref users nusers 1) (coerce uid 'nombre)))
              (write-record *user-file* 3 users)
              (setf (aref toc 2) (coerce nusers 'nombre))
              (write-record *user-file* 2 toc)
              ;; free-record modifies the toc, so it should be after writing the toc.
              (users/free-record recno))))))))




;; * #MESGS
;; * 1:"MESG|1"
;; * 2:[MAX-RECNO,FREE-LIST-RECNO,NUSER]
;; * 3:[[UID1,USER1-RECNO],[UID2,USER2-RECNO],[UID3,USER3-RECNO],…]
;; * 4:[NMSG,MSG1-RECNO,MSG2-RECNO,…]
;; * 5:next-free
;; * 6:"M|next-record|text"
;; * 7:"M|next-record|text"

(defvar *mesg-file* nil)

(defstruct (mesg (:type list))
  (type "M") next text)

(defun mesgs/open ()
  (setf *mesg-file* (lse-data-file-open *mesg-file-pathname* :if-does-not-exist :create))
  (let* ((header (read-record *mesg-file* 1))
         (fields (split-sequence *us* header :remove-empty-subseqs t)))
    (if (null fields)
        (progn
          (write-record *mesg-file* 1 (format nil "MESGS~C1" *us*)) 
          (write-record *mesg-file* 2 (coerce #(3.0 0.0 0.0) 'vecteur))
          (write-record *mesg-file* 3 (make-array (list *max-users* 2) :element-type 'nombre)))
        (progn
          (assert (<= 2 (length fields)))
          (assert (string= (first fields) "MESGS"))
          (assert (= 1 (parse-integer (second fields)))))))
  (values))


(defun mesgs/close ()
  (lse-data-file-close *mesg-file*)
  (setf *mesg-file* nil)
  (values))


(defun mesgs/new-record ()
  (unless *mesg-file* (mesgs/open))
  (file/new-record *mesg-file*))

(defun mesgs/free-record (n)
  (unless *mesg-file* (mesgs/open))
  (file/free-record *mesg-file* n))


;; (mesgs/query-user/name "PJB")
;; (mesgs/query-user/name "PJBX")

(defun mesgs/query-user/name (name)
  (let ((user (users/query-user/name name)))
    (unless user
      (error "No user named ~S" name))
    (mesgs/query-user/uid (user-uid user))))

(defun mesgs/query-user/uid (uid)
  (unless *mesg-file* (mesgs/open))
  (destructuring-bind (max-recno free-list nusers) (intlist (read-record *mesg-file* 2))
    (declare (ignore max-recno free-list))
    (let ((users (intarray (read-record *mesg-file* 3))))
      (let ((recno (record-for-uid nusers users uid)))
        (if recno
            (values (read-record *mesg-file* recno) recno)
            (values))))))


(defun mesgs/insert-user/name (name)
  (let ((user (users/query-user/name name)))
    (unless user
      (error "No user named ~S" name))
    (mesgs/insert-user/uid (user-uid user))))

(defun mesgs/insert-user/uid (uid)
  (unless *mesg-file* (mesgs/open))
  (when (mesgs/query-user/uid uid)
    (error "User with uid ~S already exists in message file." uid))
  (let ((recno (mesgs/new-record)) ; new-record modifies the toc, so it must be before reading it!
        (toc   (read-record *mesg-file* 2)))
    (destructuring-bind (max-recno free-list nusers) (intlist toc)
      (declare (ignore max-recno free-list))
      (unless (< nusers *max-users*)
        (error "Too many users in message file."))
      (let* ((users  (read-record *mesg-file* 3))
             (record (make-array *max-mesgs* :element-type 'nombre :initial-element 0.0)))
        (write-record *mesg-file* recno record)
        (setf (aref users nusers 0) (coerce recno 'nombre)
              (aref users nusers 1) (coerce uid   'nombre)
              nusers (1+ nusers))
        (setf (aref toc 2) (coerce nusers 'nombre))
        (write-record *mesg-file* 3 users)
        (write-record *mesg-file* 2 toc)
        recno))))

(defun mesgs/delete-user/name (name)
  (let ((user (users/query-user/name name)))
    (unless user
      (error "No user named ~S" name))
    (mesgs/delete-user/uid (user-uid user))))

(defun mesgs/delete-user/uid (uid)
  (unless *mesg-file* (mesgs/open))
  (let ((user (mesgs/query-user/uid uid)))
    (unless user
      (error "User with uid ~S doesn't exist in message file." uid))
    (let ((toc (read-record *mesg-file* 2)))
      (destructuring-bind (max-recno free-list nusers) (intlist toc)
        (declare (ignore max-recno free-list))
        (when (zerop nusers)
          (error "INCONSISTENCY: No user exist in message file."))
        (let* ((users  (read-record *mesg-file* 3)))
          (multiple-value-bind (recno index) (record-for-uid nusers users uid)
            (when recno
              (let ((messages (intarray (read-record *mesg-file* (round recno)))))
                (loop
                  :for i :from (aref messages 0) :downto 1
                  :do (mesgs/delete-message uid i)))
              (loop
                :for i :from index :below (1- nusers)
                :do (setf (aref users i 0) (aref users (1+ i) 0)
                          (aref users i 1) (aref users (1+ i) 1))
                :finally (decf nusers)
                         (setf (aref users nusers 0) recno
                               (aref users nusers 1) (coerce uid 'nombre)))
              (write-record *mesg-file* 3 users)
              (setf (aref toc 2) (coerce nusers 'nombre))
              (write-record *mesg-file* 2 toc)
              ;; free-record modifies the toc, so it should be after writing the toc.
              (mesgs/free-record (round recno)))))))))


;; * 6:"M|next-record|text"
;; * 7:"M|next-record|text"

(defparameter *max-text-per-record* (- *MAX-RECORD-CHAINE-SIZE* (length  "M||99999")))

(defun split-message (message)
  (let* ((message (if (position *us* message)
                      (substitute #\space *us* message)
                      message))
         (len (length message)))
   (if (< len *max-text-per-record*)
       (list message)
       (loop
         :for p :from 0 :by *max-text-per-record*
         :while (< p len)
         :collect (subseq message p (min len (+ p *max-text-per-record*)))))))

(defun store-message (message)
  (loop :with recno = 0
        :for text :in (nreverse (split-message message))
        :for record = (format nil "M~C~D~C~A" *us* recno *us* text)
        :do (setf recno (mesgs/new-record))
            (write-record *mesg-file* recno record)
        :finally (return recno)))

(defun load-message (recno)
  (with-output-to-string (msg)
   (loop
     :until (zerop recno)
     :do (let ((record (split-sequence *us* (read-record *mesg-file* recno))))
           (assert (string= "M" (first record)))
           (princ (third record) msg)
           (setf recno (parse-integer (second record)))))))

(defun delete-message (recno)
  (dolist (recno (loop
                   :until (zerop recno)
                   :collect (prog1 recno
                              (let ((record (split-sequence *us* (read-record *mesg-file* recno))))
                                (assert (string= "M" (first record)))
                                (setf recno (parse-integer (second record)))))))
    (mesgs/free-record recno)))


(defun mesgs/insert-message (uid message)
  (multiple-value-bind (messages mrecno) (mesgs/query-user/uid uid)
    (when (<= *max-mesgs* (round (aref messages 0)))
      (error "Too many messages for user UID ~S" uid))
    (let ((recno (store-message message)))
      (setf (aref messages 0) (1+ (aref messages 0))
            (aref messages (round (aref messages 0))) (coerce recno 'nombre))
      (write-record *mesg-file* mrecno messages))))

(defun mesgs/query-messages (uid)
  "Return the number of message for user UID."
  (let ((messages (mesgs/query-user/uid uid)))
    (if messages
        (values (round (aref messages 0)))
        (values 0))))

(defun mesgs/query-message (uid index)
  "RETURN: the message text of the message at INDEX for the user UID."
  (let ((messages (mesgs/query-user/uid uid)))
    (if messages
        (let ((mcount (round (aref messages 0))))
          (unless (<= 1 index mcount)
            (error "No message at the given index ~S for user UID ~S" index uid))
          (let ((recno (round (aref messages index))))
            (load-message recno)))
        (values))))


(defun mesgs/delete-message (uid index)
  (multiple-value-bind (messages mrecno) (mesgs/query-user/uid uid)
    (if messages
        (let ((mcount (round (aref messages 0))))
          (unless (<= 1 index mcount)
            (error "No message at the given index ~S for user UID ~S" index uid))
          (let ((recno (round (aref messages index))))
            (loop
              :for i :from index :below mcount
              :do (setf (aref messages i) (aref messages (1+ i)))
              :finally (decf (aref messages 0)))
            (write-record *mesg-file* mrecno messages)
            (delete-message recno)))
        (values))))


;; 3* - CREATE A USER ACCOUNT 
;; 4* - SELF CREATE A USER ACCOUNT 
;; 5* - DELETE A USER ACCOUNT (AND ITS MAILBOX)
;; 6* - LIST THE USER ACCOUNTS

(defvar *default-user-rights* "MAIL")
(defvar *current-user* nil)

(defun query (prompt &optional (type 'string))
  (format *query-io* "~&~A" prompt)
  (finish-output *query-io*)
  (ecase type
    ((string)  (read-line *query-io*))
    ((integer) (parse-integer (read-line *query-io*)))))

(defun ensure-list (x)
  (if (listp x) x (list x)))

(defun check-rights (rights)
  (unless *current-user*
    (error "No current user logged in."))
  (let ((user-rights (split-sequence #\+ (user-rights *current-user*))))
    (unless (subsetp (ensure-list rights) user-rights :test (function string=))
      (error "Insufficient access rights."))))

(defun cmd-logout ()
  (setf *current-user* nil)
  (values))

(defun cmd-login ()
  (handler-case
      (progn (format *query-io* "~%User Log In~%")
             (let ((name   (query "User name: "))
                   (pass   (query "Password: ")))
               (if (users/validate-user name pass)
                   (setf *current-user* (users/query-user/name name))
                   (error "Invalid login."))))
    (error (err) (format t "~&~A~%" err)))
  (values))

(defun cmd-create-user-account ()
  (handler-case
      (progn (format *query-io* "~%Create User Account~%")
             (check-rights "USER")
             (let ((name   (query "User name: "))
                   (pass   (query "Password: "))
                   (rights (query "Rights: ")))
               (users/insert-user name pass rights)))
    (error (err) (format t "~&~A~%" err)))
  (values))

(defun cmd-self-create-user-account ()
  (handler-case
      (progn (format *query-io* "~%Self Create User Account~%")
             (let ((name   (query "User name: "))
                   (pass   (query "Password: "))
                   (rights *default-user-rights*))
               (users/insert-user name pass rights)))
    (error (err) (format t "~&~A~%" err)))
  (values))

(defun cmd-delete-user-account ()
  (handler-case
      (progn (format *query-io* "~%Delete User Account~%")
             (check-rights "USER")
             (let ((name   (query "User name: ")))
               (ignore-errors (mesgs/delete-user/name name))
               (users/delete-user name)))
    (error (err) (format t "~&~A~%" err)))
  (values))

(defun cmd-list-user-accounts ()
  (handler-case
      (progn (format *query-io* "~%List User Accounts~%")
             (check-rights "USER")
             (multiple-value-bind (nusers users) (users/query-users)
               (loop
                     :for i :below nusers
                     :for user = (users/query-user/uid (aref users i 1))
                     :do (format t "~4D ~12A ~A~%"
                                 (user-uid user)
                                 (user-name user)
                                 (user-rights user)))))
    (error (err) (format t "~&~A~%" err)))
  (values))


;; 7* - SEND A MESSAGE TO A USER
;; 8* - LIST THE RECEIVED MESSAGE
;; 9* - READ A RECEIVED MESSAGE
;; 10* - RESPOND TO A RECEIVED MESSAGE
;; 11* - DELETE A RECEIVED MESSAGE

(defun edit (title)
  (format *query-io* "~&~A~%" title)
  (format *query-io* "(end with a line containing only a dot).~%")
  (finish-output *query-io*)
  (with-output-to-string (msg)
    (loop :for line = (read-line *query-io* nil nil)
          :while (and line (not (string= "." line)))
          :do (write-line line msg))))

(defun edit-and-send-message (from to &optional subject)
  (let ((subject (or subject (query "Subject: ")))
        (message (edit  "Message: "))
        (date    (dat)))
    (ignore-errors (mesgs/insert-user/uid (user-uid to)))
    (mesgs/insert-message (user-uid to)
                          (format nil "From: ~A~%~
                                                 To: ~A~%~
                                                 Date: ~A~%~
                                                 Subject: ~A~2%~
                                                 ~A~%"
                                  from (user-name to)
                                  date subject message))))

(defun cmd-send-message ()
  (handler-case
      (progn (format *query-io* "~%Send a Message~%")
             (check-rights "MAIL")
             (let* ((from    (user-name *current-user*))
                    (to      (string-upcase (query "To: ")))
                    (to-user (let ((user (users/query-user/name to)))
                               (unless user
                                 (error "No such user named ~S" to))
                               user)))
               (edit-and-send-message from to-user)))
    (error (err) (format t "~&~A~%" err)))
  (values))


(defun message-headers (message)
  (let ((length (search #(#\Newline #\Newline) message)))
    (when length
      (mapcar (lambda (header)
                (let ((colon (position #\: header)))
                  (list (intern (string-upcase (subseq header 0 colon))
                                (load-time-value (find-package "KEYWORD")))
                        (string-trim " " (subseq header (1+ colon))))))
              (split-sequence #\Newline (subseq message 0 length))))))


(defun cmd-list-messages ()
  (handler-case
      (progn (format *query-io* "~%List Messages~%")
             (check-rights "MAIL")
             (let* ((uid    (user-uid *current-user*))
                    (mcount (mesgs/query-messages uid)))
               (loop
                 :for i :from 1 :to mcount
                 :for headers = (ignore-errors (message-headers (mesgs/query-message uid i)))
                 :do (if headers
                          (format t "~3D: ~12A ~18A ~A~%"
                                     i
                                     (second (assoc :from headers))
                                     (second (assoc :date headers))
                                     (second (assoc :subject headers)))
                          (format t "~3D:~%" i)))))
    (error (err) (format t "~&~A~%" err)))
  (values))

(defun cmd-read-message ()
  (handler-case
      (progn (format *query-io* "~%Read Message~%")
             (check-rights "MAIL")
             (let* ((uid    (user-uid *current-user*))
                    (msgno  (query "Message number: " 'integer)))
               (write-string  (mesgs/query-message uid msgno))))
    (error (err) (format t "~&~A~%" err)))
  (values))

(defun cmd-respond-to-message ()
  (handler-case
      (progn (format *query-io* "~%Respond to Message~%")
             (check-rights "MAIL")
             (let* ((uid     (user-uid *current-user*))
                    (from    (user-name *current-user*))
                    (msgno   (query "Message number: " 'integer))
                    (headers  (ignore-errors (message-headers
                                              (mesgs/query-message uid  msgno)))))
               (unless headers
                 (error "Cannot find the sender since there's no header."))
               (let ((sender  (second (assoc :from headers)))
                     (subject (second (assoc :subject headers))))
                 (unless sender
                   (error "Cannot find the sender in the header."))
                 (edit-and-send-message from
                                        (users/query-user/name sender)
                                        (when subject
                                          (format nil "Re: ~A" subject))))))
    (error (err) (format t "~&~A~%" err)))
  (values))

(defun cmd-delete-message ()
  (handler-case
      (progn (format *query-io* "~%Delete a Message~%")
             (check-rights "MAIL")
             (let* ((uid     (user-uid *current-user*))
                    (msgno   (query "Message number: " 'integer)))
               (mesgs/delete-message uid msgno)))
    (error (err) (format t "~&~A~%" err)))
  (values))

#-(and) (progn

          (progn
            (progn (users/close) (delete-file *user-file-pathname*))
            (users/insert-user "PJB" "POPOROPLO" "MAIL+USER")
            (users/insert-user "LSE" "POPOROPLO" "MAIL")
            (users/insert-user "TOTO" "POPOROPLO" "MAIL")
            (users/insert-user "TITI" "POPOROPLO" "MAIL")
            )
                              
          (users/delete-user "PJB")
          (users/delete-user "TOTO")
          (users/delete-user "LSE")
          (users/delete-user "TITI")
          (users/query-user/name "TITI")
          (users/query-user/uid 4)
          (loop :for i :from 1 :to 8 :do (print (read-record *user-file* i)))
          (users/query-user/name "TOTO")
          (users/query-user/uid 2)
          (users/query-user/name "TITI")
          (users/query-user/uid 4)
          (users/validate-user "PJB" "POPOROPLOX")
          (read-record *user-file* 2)
          (read-record *user-file* 4)
          (users/new-record)
          (users/free-record 7)
          (users/open)
          (users/close)
          (users/query-users)


          (progn
            (progn (mesgs/close) (delete-file *mesg-file-pathname*))
            (mesgs/insert-user/name "PJB")
            (mesgs/insert-user/name "TITI")
            )

          (mesgs/free-record 4)
          (mesgs/open)
          (mesgs/close)

          (mesgs/query-user/name "PJB")
          (mesgs/query-user/name "TITI")
          (mesgs/query-user/uid 1)
          (mesgs/query-user/uid 4)
          (mesgs/delete-user/uid 4)
          (mesgs/free-record 4)
          (mesgs/insert-message 1 (com.informatimago.common-lisp.cesarum.file:text-file-contents
                                   "~/tmp/misc/wang"))
          (mesgs/insert-message 4 "Understood.")
          (mesgs/insert-message 1 "
Date: Tue Jul  1 17:22:12 CEST 2014
Subject: test
From: jojo

Hello how do you do?
")
          (mesgs/insert-message 1 "
Date: Tue Jul  1 17:49:10 CEST 2014
Subject: second
From: titi

Oye Hutch!
Que
Tienes un 10-40
Que
Un 10-40
")
          (mesgs/query-messages 1)
          (mesgs/query-message 1 1)
          (mesgs/query-message 1 2)
          (mesgs/query-message 1 3)
          (mesgs/delete-message 1 2)

          (loop :for i :from 1 :to 8 :do (print (read-record *user-file* i)))
          (loop :for i :from 1
                :for rec =  (read-record *mesg-file* i)
                :while rec :do (terpri) (prin1 i) (princ ":") (prin1 rec))


          (record-for-uid (elt (intlist (read-record *mesg-file* 2)) 2)
                          (intarray (read-record *mesg-file* 3))
                          4)

          (intarray (read-record *mesg-file* 3)))

(defun cmd-quit ()
  (throw 'grande-gazongues (values)))


(defun select-operations (operations)
  (let ((rights (split-sequence #\+ (user-rights *current-user*))))
    (remove-if (lambda (operation)
                 (if (null *current-user*)
                     (first operation)
                     (not (subsetp (first operation) rights :test (function string=)))))
               operations)))

(defun mesg ()
  (catch 'grande-gazongues
    (let ((operations '((()       "Create an account for yourself" cmd-self-create-user-account)
                        (()       "Log in"                         cmd-login)
                        (("USER") "Create a user account"          cmd-create-user-account)
                        (("USER") "Delete a user account"          cmd-delete-user-account)
                        (("USER") "List user accounts"             cmd-list-user-accounts)
                        (("MAIL") "Send a message"                 cmd-send-message)
                        (("MAIL") "List your messages"             cmd-list-messages)
                        (("MAIL") "Read a message"                 cmd-read-message)
                        (("MAIL") "Respond to a message"           cmd-respond-to-message)
                        (("MAIL") "Delete a message"               cmd-delete-message)
                        (()       "Quit"                           cmd-quit))))
      (unwind-protect
           (loop
             :for menu = (select-operations operations)
             :do (loop
                   :for i :from 1
                   :for operation :in menu
                   :initially  (format *query-io* "~2%")
                   :do (format *query-io* "~2D) ~A~%" i (second operation)))
                 (handler-case 
                     (let* ((choice (1- (query "Your choice: " 'integer)))
                            (cmd (nth choice menu)))
                       (when cmd
                         (funcall (third cmd))))
                   (error (err)  (format t "~&~A~%" err))))
        (cmd-logout)
        (mesgs/close)
        (users/close)))))


;;;; THE END ;;;;
