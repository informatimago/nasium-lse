(cd #P "~/src/pjb/lse-cl/src/")
(push #P"~/src/pjb/lse-cl/dependencies/zebu-3.5.5-pjb/" asdf:*central-registry*)
(push #P "~/src/pjb/lse-cl/src/"                        asdf:*central-registry*)
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
(cd #P "~/src/pjb/lse-cl/src/")
(pushnew #P"~/src/pjb/lse-cl/dependencies/zebu-3.5.5-pjb/" asdf:*central-registry*)
(pushnew #P "~/src/pjb/lse-cl/src/"                        asdf:*central-registry*)
(pushnew :developing *features*)

(asdf:run-shell-command "rm -rf /home/pjb/.cache/common-lisp/kuiper.lan.informatimago.com/ccl-1.7-f94-linux-amd64/home/pjb/src/git/pjb/lse-cl/src/")
(in-package :cl-user)
(asdf-load :com.informatimago.lse)
(com.informatimago.common-lisp.cesarum.package:add-nickname :COM.INFORMATIMAGO.LSE.IDENTIFIERS :id)

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
