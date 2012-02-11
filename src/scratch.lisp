;;;; -*- mode:lisp;coding:utf-8 -*-

(cd #P "/home/pjb/src/pjb/lse-cl/src/")
(push #P"/home/pjb/src/pjb/lse-cl/dependencies/zebu-3.5.5-pjb/" asdf:*central-registry*)
(push #P "/home/pjb/src/pjb/lse-cl/src/"                        asdf:*central-registry*)
(in-package :cl-user)
(asdf-load :com.informatimago.lse)


(in-package "COM.INFORMATIMAGO.LSE")

(directory "/home/pjb/src/pjb/projects/lse/EXEMP/*.lse")
(dolist (src  (directory "/home/pjb/src/pjb/projects/lse/BOURG/*.LSE"))
  (print src)
  (test-scan-file src))

(test-scan-file "SYNTERR.LSE")


(dolist (src  (directory "/home/pjb/src/pjb/projects/lse/BOURG/*.LSE"))
  (print src)
  (test-parse-file src))

(test-parse-file "SYNTERR.LSE")


(compile-line "10 LIRE A,B;C_A*2+B*B/3;TABLEAU T[2];T[1]_A;T[2]_B;AFFICHER C,T;TERMINER")

(compile-line "10 LIRE A,B;SI A=B ALORS C_A*2+B*B/3 SINON DEBUT TABLEAU T[2];T[1]_A;T[2]_B;AFFICHER [/,20X,F3.2]T FIN;AFFICHER A,B;TERMINER")





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


(in-package :cl-user)
(asdf-load :com.informatimago.common-lisp.cesarum)

(in-package :cl-user)
(cd #P "/home/pjb/src/pjb/lse-cl/src/")
(pushnew #P"/home/pjb/src/pjb/lse-cl/dependencies/zebu-3.5.5-pjb/" asdf:*central-registry*)
(pushnew #P "/home/pjb/src/pjb/lse-cl/src/"                        asdf:*central-registry*)
(pushnew :developing *features*)

;;----------------------------------------------------------------------
(in-package :cl-user)
(cd      #P "/home/pjb/src/pjb/lse-cl/src/")
(pushnew #P "/home/pjb/src/pjb/lse-cl/src/"  asdf:*central-registry*)
(pushnew #P "/home/pjb/src/public/rdp/"      asdf:*central-registry*)
(pushnew :developing *features*)
(setf *print-right-margin* 200
      *print-pretty* t
      *print-case* :downcase)
(asdf:run-shell-command "rm -rf /home/pjb/.cache/common-lisp/kuiper.lan.informatimago.com/ccl-1.7-f94-linux-amd64/home/pjb/src/git/pjb/lse-cl/src/")
(in-package :cl-user)
(progn (ignore-errors (delete-package :com.informatimago.lse.server))
       (ignore-errors (delete-package :com.informatimago.lse))
       (ignore-errors (delete-package :com.informatimago.lse.byte-code))
       (ignore-errors (delete-package :com.informatimago.lse.identifiers)))
(asdf-delete-system :com.informatimago.lse)
(ql:quickload       :com.informatimago.lse)
(in-package         :com.informatimago.lse)
;; (com.informatimago.common-lisp.cesarum.package:add-nickname :COM.INFORMATIMAGO.LSE.IDENTIFIERS :id)
;;----------------------------------------------------------------------


(in-package :cl-user)
(cd      #P "/home/pjb/src/pjb/lse-cl/src/")
(pushnew #P "/home/pjb/src/pjb/lse-cl/src/" asdf:*central-registry*)
(pushnew :developing *features*)
(ql:quickload :com.informatimago.lse :verbose t :explain t)
(in-package :com.informatimago.lse)


(in-package :com.informatimago.lse.server)
(main)

(map nil 'print (bt:all-threads))



(in-package :com.informatimago.lse.server)
(setf *print-escape* nil)
*server* 
(server-name *server*)
(read-from-string (babel:octets-to-string (slot-value (first (server-clients *server*)) ' COM.INFORMATIMAGO.IOLIB.SERVER::INPUT-BUFFER) :encoding :utf-8))
(slot-value (first (server-clients *server*)) ' COM.INFORMATIMAGO.IOLIB.SERVER::OUTPUT-BUFFER)
(slot-value (first (server-clients *server*)) ' COM.INFORMATIMAGO.IOLIB.SERVER::OUTPUT-BUFFER)
(import ' COM.INFORMATIMAGO.IOLIB.SERVER:SERVER-NAME)T
(apropos "INPUT-BUFFER")
(trace configuration-repl-input)
(let ((text "(help)"))
 (with-output-to-string (*standard-output*)
   (let ((*error-output* *standard-output*)
         (*standard-input* (make-string-input-stream ""))
         (*terminal-io*    (make-two-way-stream *standard-input*
                                                *standard-output*))
         (*query-io*       *query-io*))
     (configuration-repl-input text))))

(client-send-response  (first (server-clients *server*))  "~%(connections~% max-number (?x n))~%")
(client-send-prompt   (first (server-clients *server*)))
(client-send-response  (first (server-clients *server*))  "~%_______________________________")
(client-send-response  (first (server-clients *server*))  "~CLSE> " (code-char 13))
(format nil  "~%~V,,,'_A~C~A" 79 "" #\Return
                   (console-prompt (client-console   (first (server-clients *server*)))))
"
_______________________________________________________________________________EMULSE LIMBO> "

(console-state (client-console (first (server-clients *server*))))

(let ((scanner (make-instance 'lse-scanner :source "1*COMMENT")))
       (advance-line scanner)
       (write-line (scanner-buffer scanner))
       (print (list 'column= (scanner-column scanner) 'state= (scanner-state scanner)))
       (print (setf (scanner-current-token scanner) (scan-lse-token scanner)))
       (print (list 'column= (scanner-column scanner) 'state= (scanner-state scanner)))
       (print (setf (scanner-current-token scanner) (scan-lse-token scanner)))
       (print (list 'column= (scanner-column scanner) 'state= (scanner-state scanner))))

(with-open-file (src  #P"/home/pjb/src/pjb/lse-cl/TESTCOMP.LSE")
  (let ((scanner (make-instance 'lse-scanner :source src)))
   (loop
     :for line = (readline (slot-value scanner 'stream))
     :do (format t ";; ~A~%" line)
     :while line)))

(in-package :com.informatimago.lse)
(with-open-file (src  #P"/home/pjb/src/pjb/lse-cl/TESTCOMP.LSE")
  (let ((scanner (make-instance 'lse-scanner :source src)))
    (advance-line scanner)
    (scan-next-token scanner)))


(let ((grammar com.informatimago.lse::lse)
      (items '(tok-afficher format expression tok-virgule expression)))
  (let ((increments (make-hash-table))
        (i -1))
    (mapcan (lambda (item)
              (incf i)
              (when (or (non-terminal-p grammar item)
                        (terminal-p grammar item))
                (let ((index  (incf (gethash item increments 0)))
                      (dollar (intern (format nil "$~D" i))))
                  (append
                   (when (= 1 index)
                     (list (list item dollar)))
                   (list (list (intern (format nil "~A.~A' item index"))
                               dollar))))))
            items)))

(in-package :COM.INFORMATIMAGO.LSE)
(test/lse-scanner  #P"/home/pjb/src/pjb/lse-cl/SYNTERR.LSE")
(test/lse-scanner  #P"/home/pjb/src/pjb/lse-cl/TESTCOMP.LSE")

(test-parse-file   #P "/home/pjb/src/pjb/lse-cl/SYNTERR.LSE")
(test-parse-file   #P "/home/pjb/src/pjb/lse/BOURG/BOUR.LSE")
(test-parse-file   #P"/home/pjb/src/pjb/lse-cl/TESTCOMP.LSE")

(test-compile-file #P "/home/pjb/src/pjb/lse/BOURG/BOUR.LSE")
(test-compile-file #P "/home/pjb/src/pjb/lse-cl/TESTCOMP.LSE")


