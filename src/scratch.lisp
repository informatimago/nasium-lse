;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               scratch.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    XXX
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-23 <PJB> Added this header.
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

(cd #P "/home/pjb/src/pjb/nasium-lse/src/")
(push #P"/home/pjb/src/pjb/nasium-lse/dependencies/zebu-3.5.5-pjb/" asdf:*central-registry*)
(push #P "/home/pjb/src/pjb/nasium-lse/src/"                        asdf:*central-registry*)
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
(cd #P "/home/pjb/src/pjb/nasium-lse/src/")
(pushnew #P"/home/pjb/src/pjb/nasium-lse/dependencies/zebu-3.5.5-pjb/" asdf:*central-registry*)
(pushnew #P "/home/pjb/src/pjb/nasium-lse/src/"                        asdf:*central-registry*)
(pushnew :developing *features*)

;;----------------------------------------------------------------------
(in-package :cl-user)
(cd      #P "/home/pjb/src/pjb/nasium-lse/src/")
(pushnew #P "/home/pjb/src/pjb/nasium-lse/src/"  asdf:*central-registry*)
(pushnew #P "/home/pjb/src/public/rdp/"      asdf:*central-registry*)
(pushnew :developing *features*)
(setf *print-right-margin* 200
      *print-pretty* t
      *print-case* :downcase)
(asdf:run-shell-command "rm -rf /home/pjb/.cache/common-lisp/kuiper.lan.informatimago.com/ccl-1.7-f94-linux-amd64/home/pjb/src/git/pjb/nasium-lse/src/")
(in-package :cl-user)
(progn (ignore-errors (delete-package :com.informatimago.lse.server))
       (ignore-errors (delete-package :com.informatimago.lse))
       (ignore-errors (delete-package :com.informatimago.lse.byte-code))
       (ignore-errors (delete-package :com.informatimago.lse.identifiers)))
(asdf-delete-system :com.informatimago.lse)
(ql:quickload       :com.informatimago.lse)
(in-package         :com.informatimago.lse)
(com.informatimago.common-lisp.cesarum.package:add-nickname :COM.INFORMATIMAGO.LSE.IDENTIFIERS :id)
;;----------------------------------------------------------------------


(in-package :cl-user)
(cd      #P "/home/pjb/src/pjb/nasium-lse/src/")
(pushnew #P "/home/pjb/src/pjb/nasium-lse/src/" asdf:*central-registry*)
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
_______________________________________________________________________________
EMULSE LIMBO> "

(console-state (client-console (first (server-clients *server*))))

(let ((scanner (make-instance 'lse-scanner :source "1*COMMENT")))
       (advance-line scanner)
       (write-line (scanner-buffer scanner))
       (print (list 'column= (scanner-column scanner) 'state= (scanner-state scanner)))
       (print (setf (scanner-current-token scanner) (scan-lse-token scanner)))
       (print (list 'column= (scanner-column scanner) 'state= (scanner-state scanner)))
       (print (setf (scanner-current-token scanner) (scan-lse-token scanner)))
       (print (list 'column= (scanner-column scanner) 'state= (scanner-state scanner))))

(with-open-file (src  #P"/home/pjb/src/pjb/nasium-lse/TESTCOMP.LSE")
  (let ((scanner (make-instance 'lse-scanner :source src)))
   (loop
     :for line = (readline (slot-value scanner 'stream))
     :do (format t ";; ~A~%" line)
     :while line)))

(in-package :com.informatimago.lse)
(with-open-file (src  #P"/home/pjb/src/pjb/nasium-lse/TESTCOMP.LSE")
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
(test/lse-scanner  #P"/home/pjb/src/pjb/nasium-lse/SYNTERR.LSE")
(test/lse-scanner  #P"/home/pjb/src/pjb/nasium-lse/TESTCOMP.LSE")

(test-parse-file   #P "/home/pjb/src/pjb/nasium-lse/SYNTERR.LSE")
(test-parse-file   #P "/home/pjb/src/pjb/lse/BOURG/BOUR.LSE")
(test-parse-file   #P"/home/pjb/src/pjb/nasium-lse/TESTCOMP.LSE")

(test-compile-file #P "/home/pjb/src/pjb/lse/BOURG/BOUR.LSE")
(test-compile-file #P "/home/pjb/src/pjb/nasium-lse/TESTCOMP.LSE")



(in-package :COM.INFORMATIMAGO.LSE)
(untrace read-block
       read-header
       write-header
       build-record-table
       read-record-table
       write-record-table
       %allocate-record
       lse-data-file-open
       lse-data-file-close
       lse-data-file-open-p
       read-record
       write-record
       delete-record
       test/dump-lse-file
       test/dump-lse-file/low-level
       test/file)
(car )
(ccl::basic-character-output-stream)


(in-package "COM.INFORMATIMAGO.LSE")
(let ((terminal (task-terminal *task*))
      (count 1))
  (declare (ignorable count))
  (terminal-new-line terminal 2)
  (terminal-write-string terminal "__________________________________________________")
  (terminal-carriage-return (task-terminal *task*))
  (terminal-write-string terminal "enter: ")
  (terminal-read-string terminal))

(let ((terminal (task-terminal *task*))
      (count 1))
  (declare (ignorable count))
  (terminal-new-line terminal 2)
  (terminal-write-string terminal "Hello")
  (terminal-line-feed (task-terminal *task*))
  ;; (with-slots (COM.INFORMATIMAGO.LSE.CLI::current-column
  ;;              COM.INFORMATIMAGO.LSE.CLI::buffer) terminal
  ;;   (COM.INFORMATIMAGO.LSE.CLI::flush terminal)
  ;;   (let ((output (terminal-output-stream terminal)))
  ;;     (loop :repeat count :do (terpri output))
  ;;     (terminal-finish-output terminal)
  ;;     (fill COM.INFORMATIMAGO.LSE.CLI::buffer #\space)
  ;;     (COM.INFORMATIMAGO.LSE.CLI::show-buffer terminal)
  ;;     (list COM.INFORMATIMAGO.LSE.CLI::current-column
  ;;           COM.INFORMATIMAGO.LSE.CLI::buffer
  ;;           )))
  (list
   (terminal-write-string terminal "world")
   (terminal-carriage-return terminal)
   (terminal-write-string terminal "->")
   (terminal-new-line terminal 2)
   (terminal-write-string terminal "String:")
   (terminal-read-string terminal)
   (terminal-write-string terminal "[After]")
   (terminal-new-line terminal 1)
   (terminal-write-string terminal "String:")
   (terminal-read-string terminal)
   (terminal-carriage-return terminal)
   (terminal-write-string terminal "[After]")
   (terminal-new-line terminal 2)
   (terminal-write-string terminal "Number:")
   (terminal-read terminal)
   (terminal-write-string terminal "[After]")
   (terminal-new-line terminal 1)
   (terminal-write-string terminal "Number:")
   (terminal-read terminal)
   (terminal-carriage-return terminal)
   (terminal-write-string terminal "[After]")
   (terminal-new-line terminal 2)
   (clear-input (terminal-input-stream terminal))))


    (iolib.serial:stty
     :raw     t

     ;; :ignbrk  t   ; ignore break (only on serial lines).
     ;; :brkint  nil ; useless when  :ignbrk t
     ;; :ignpar  t   ; ignore parity errors
     ;; :parmrk  nil ; read parity errors as \0
     ;; :inpck   nil ; disable parity check

     :istrip  t   ; enable  strip off eigth bit (should be nil for utf-8 input)
     :inlrc   t   ; enable  translate NL to CR on input
     :igncr   nil ; disable ignore CR  on input
     :icrnl   nil ; disable translate CR to NL on input.
     :iuclc   nil ; disable map uppercase to lower (not POSIX).
     :ixon    nil ; disable XON/XOFF flow control on output. ???
     :ixoff   nil ; disable XON/XOFF flow control on input.
     :ixany   nil ; disable Typing any character to restart stopped output.
     :imaxbel nil ; disable Ring Bell when input queue is full.
     :iutf8   nil ; disable UTF-8 input (for character erase in cooked mode).

     :opost   nil ; disable implementation defined output processing.
     :olcuc   nil ; disable map lowercase touppercase (not POSIX).
     :onlcr   nil ; disable map NL to CR-NL on output.
     :ocrnl   nil ; disable map CR to NL on output.
     :onocr   nil ; disable output CR at column 0.
     :onlret  nil ; disable don't output CR (ie. output CR).
     :ofill   nil ; disable send  fill characcters for a delay (instead of timer).
     :ofdel   nil ; fill character is NUL; t =  fill character is DEL.
     :nldly   :nl0  ; Newline delay mask (member :nl0 :nl1)
     :crdly   :cr0  ; CR delay mask (member :cr0 :cr1 :cr2 :cr3)
     :tabdly  :tab0 ; TAB delay mask  (member :tab0 :tab1 :tab2 :tab3)
     :bsdly   :bs0  ; Backspace delay mask (member :bs0 :bs1)
     :vtdly   :vt0  ; Vertical tab delay mask (member :vt0 :vt1)
     :ffdly   :ff0  ; Form feed delay mask (member :ff0 :ff1)

     ;; For modem control:
     ;; ;; :cbaud    speed mask
     ;; ;; :cbaudex  speed mask
     ;; :csize    :cs8 ; (member :cs5 :cs6 :cs7 :cs8)
     ;; :cstopb   nil  ; disable set two stop bits
     ;; :cread    t    ; enable receiver
     ;; :parenb   nil  ; disable parity generation on  output and parity checking on input.
     ;; :parodd   nil  ;  if set then parity is odd otherwise parity is even.
     ;; :hupcl    nil  ; lowoer modem conotrollines after last  process closes the device (hang-up).
     ;; :clocal   t    ; ignoroe modem control lines.
     ;; :loblk    nil  ; (not POSIX, not linux)
     ;; ;; :cibaud   mask for input speed
     ;; ;; :cmspar   use stick (mark/space) parity.
     ;; :crtscts t    ;  enable  hardware flow control.

     :isig    nil ; disable when the character INTR, QUIT, SUSP, or DSUSP are received, generate the signal.
     :icanon  nil ; disable canonical mode.
     :xcase   nil ; (not POSIX) (and :icanon :xcase) => upper case terminal
     :echo    nil ; disable echo of input characters
     :echoe   nil ; (and :icanon :echoe) => ERASE and WERASE erase the previous character and word.
     :echok   nil ; (and :icanon :echok) => KILL  erase current line.
     :echonl  nil ; (and :icanon :echonl) =>  echo the NL  even when :echo is t.
     :echoprt nil ; (not POSIX) (and :icanon :iecho :echoprt) => characters are printed as they are erased. ( /a\  ??? )
     :echoke  nil ; (and Icanon :echoke) => KILL is echoed by erasing each character on the line (as specified by :echoe and :echoprt).
     :echoctl nil ; (not POSIX) (:echo :echoctl) control codes (not TAB,  NL, START, STOP) are  echoed as ^X
     :defecho nil ; (not POSIX, not linux) Echo only when a process is reading.
     :flusho  nil ; (not POSIX, not linux) output is flushed.  Toggled by the DISCARD character.
     :noflsh  t   ; enable Disable flushing input and output when signaling INT, QUIT and SUSP.
     :tostop  t   ; sends SIGTTOU to processes who writes to this terminal.
     :pendin  nil ; (not POSIX, not linux) input queue is reprinted when next char is read.
     :iexten  nil ; disable Enable implementation-defined input-processing. To enable EOL2, LNEXT, REPRINT, WERASE, and IUCLC.
     
     :vintr   #x01     ; C-a ; SIGINT                                    needs :isig t
     :vquit   #x1b     ; ESC ; SIGQUIT                                   needs :isig t
     :verase  0              ; erase character                           needs :icanon t
     :vkill   0              ; erase line                                needs :icanon t
     :veof    0              ; send input buffer (= eof when empty)      needs :icanon t
     :vmin    0              ; minimum number of characters for noncanonical read       
     :veol    #x13     ; C-s ; additionnal end of line character         needs :icanon t
     :vtime   0              ; timeout in decisecond for noncanonical read
     :veol2   0              ; yet additionnal end of line character     needs :icanon t (not POSIX)
     :vswtch  0              ; (not POSIX, not linux) switch character.
     :vstart  0              ; The X-ON character                        needs :ixon
     :vstop   0              ; The X-OFF character                       needs :ixon
     :vsusp   0              ; SIGSUSP                                   needs :isig t
     :vdsusp  0              ; (not POSIX, not LINUX) SIGSUSP when read  needs :isig t :iexten t
     :vlnext  0              ; (not POSIX, not LINUX) literal next       needs :iexten t
     :werase  0              ; (not POSIX) word erase                    needs :icanon t :iexten t
     :vreprnt 0              ; (not POSIX) reprint unread characters     needs :icanon t :iexten t
     :vdiscard 0             ; (not POSIX, not LINUX) toggle start/stop discarding output needs :iexten t
     :vstatus 0             ; (not POSIX, not LINUX) status request
     )

