(cd #P "~/src/pjb/lse-cl/src/")
(push #P"~/src/pjb/lse-cl/dependencies/zebu-3.5.5-pjb/" asdf:*central-registry*)
(push #P "~/src/pjb/lse-cl/src/"                        asdf:*central-registry*)
(asdf-load :com.informatimago.lse)


(load  "PACKAGES:COM;INFORMATIMAGO;COMMON-LISP;PMATCH")

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
