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
(cd #P "~/src/pjb/lse-cl/src/")
(pushnew #P"~/src/pjb/lse-cl/dependencies/zebu-3.5.5-pjb/" asdf:*central-registry*)
(pushnew #P "~/src/pjb/lse-cl/src/"                        asdf:*central-registry*)
(pushnew :developing *features*)

(asdf:run-shell-command "rm -rf /home/pjb/.cache/common-lisp/kuiper.lan.informatimago.com/ccl-1.7-f94-linux-amd64/home/pjb/src/git/pjb/lse-cl/src/")
(in-package :cl-user)
(asdf-load :com.informatimago.lse)

(in-package :com.informatimago.lse.server)
(main)

(map nil 'print (bt:all-threads))
