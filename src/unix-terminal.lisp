;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               unix-terminal.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Implements a unix terminal subclass,  specified by a terminfo
;;;;    entry, and using termios and unix fd I/O.
;;;;
;;;;    We require this low-level approach, so that we may deal with
;;;;    the terminals character by character, and we may synthesize
;;;;    user interruption upon reception of certain characters, and
;;;;    doing so possibly on several terminals from the same
;;;;    application (server).
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-24 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
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

(in-package "COM.INFORMATIMAGO.LSE.UNIX-TERMINAL")




(defun terminal-control-attributes (fd &optional action)
  "Allocates a new termios foreign structure, fills it with current
values for the specified file descriptor FD, and return it.
ACTION is ignored."
  (declare (ignore action))
  (let ((termios (cffi:foreign-alloc 'iolib.serial:termios)))
    (iolib.serial:%tcgetattr fd termios)
    termios))

(defun terminal-control-character (termios cc)
  "Return the  control character value."
  (cffi:mem-aref (cffi:foreign-slot-pointer termios
                                            'iolib.serial:termios
                                            'iolib.serial::control-chars)
                 'iolib.serial::cc
                 ;; constant name is offset
                 (cffi:foreign-enum-value 'iolib.serial:control-character cc)))


(defun (setf terminal-control-attributes) (new-termios fd action)
  "

Sets the given NEW-TERMIOS (which has been obtained with the
TERMINAL-CONTROL-ATTRIBUTES function to the specified file descriptor FD.

ACTION specifies when the setting occurs:
        :now    immediately
        :drain  after all output has been transmitted.
        :flush  after all output has been transmitted, and all input is discaded.
"
  (check-type action (member :now :drain :flush))
  (iolib.serial:%tcsetattr fd (ecase action
                                (:now :tcsanow)
                                (:drain :tcsadrain)
                                (:flush :tcsaflush))
                           new-termios)
  new-termios)

(defun termios-free (termios)
  "Deallocate the TERMIOS structure returned by TERMINAL-CONTROL-ATTRIBUTES."
  (cffi:foreign-free termios))


(defmacro with-terminal-control-attributes ((termios-variable fd) &body body)
  "Binds the variable passed to TERMIOS-VARIABLE to a termios
structure returned by (terminal-control-attributes fd), and execute
\(progn BODY) in an unwind-protect.  The cleanup dealloocates the
termios with TERMIOS-FREE."
  (let ((vtermios (gensym)))
    `(let* ((,vtermios (terminal-control-attributes ,fd))
            (,termios-variable ,vtermios))
       (unwind-protect  (progn ,@body)
         (termios-free ,vtermios)))))


(defun terminal-control-characters (fd)
  (let ((termios (terminal-control-attributes fd)))
    (mapcar (lambda (cc) (cons cc (terminal-control-character termios cc)))
            '(:vintr :vquit :vsusp :vkill :veof :veol :veol2
              :verase :vwerase :vreprint :vstart :vstop))))



(defclass unix-terminal (standard-terminal)
  ((input-file-descriptor  :initarg :input-fd
                           :initform 0
                           :reader  terminal-input-file-descriptor)
   (output-file-descriptor :initarg :output-fd
                           :initform 1
                           :reader  terminal-output-file-descriptor)
   (modern-mode            :initarg :modern-mode
                           :initform nil
                           :reader terminal-modern-mode
                           :documentation "
When false (default), the terminal works like on the MITRA-15 LSE System:
C-s (X-OFF) to send input to the computer.
C-a (SOH)   to send a signal to the program.
\           to \"erase\" the previous character.
CR          to send input to the computer, but CR is included in input strings.
ESC         to interrupt the program.
There's no cursor, but a line of _ followed by a CR is printed on the
last line, (it's erased before line feed) so that users may see up to
where they've written.

When true, the terminal works more like a modern unix terminal;
")
   (terminfo               :initarg :terminfo
                           :initform (terminfo:set-terminal (getenv "TERM"))
                           :reader terminal-terminfo)
   (saved-termios          :initform nil)
   (echo                   :initform t
                           :reader terminal-echo)
   (buffer                 :initform (make-array 80
                                                 :element-type 'character
                                                 :adjustable t
                                                 :fill-pointer 0))
   (input-read             :initform 0)
   (input-finished         :initform nil)
   (vintr                  :initform 0)
   (vquit                  :initform 0)
   (vsusp                  :initform 0)
   (vkill                  :initform 0)
   (veof                   :initform 0)
   (veol                   :initform 0)
   (veol2                  :initform 0)
   (verase                 :initform 0)
   (vwerase                :initform 0)
   (vreprint               :initform 0)
   (vstart                 :initform 0)
   (vstop                  :initform 0)))


(defparameter *external-format/iso-8859-1*
  #+clisp charset:iso-8859-1
  #+ccl  (ccl:make-external-format :domain :file
                                   :character-encoding :iso-8859-1
                                   :line-termination :unix)
  #+cmu  :iso-8859-1
  #+ecl  :iso-8859-1
  #+sbcl :iso-8859-1)


(defmethod initialize-instance :after ((terminal unix-terminal) &rest args
                                       &key input-stream output-stream input-fd output-fd)
  (declare (ignorable args))
  (assert (not (and input-stream input-fd))
          () ":INPUT-STREAM is mutually exclusive with :INPUT-FD")
  (assert (not (and output-stream output-fd))
          () ":OUTPUT-STREAM is mutually exclusive with :OUTPUT-FD")
  (with-slots (input-file-descriptor output-file-descriptor) terminal
    (setf input-fd  (or input-fd  input-file-descriptor))
    (setf output-fd (or output-fd output-file-descriptor)))
  (flet ((fd (direction stream)
           (let ((fds (fd-stream-fd stream)))
             (if (listp fds)
                 (ecase direction
                   (:input  (first  fds))
                   (:output (second fds)))
                 fds)))
         (make-stream (direction fd)
           (make-fd-stream fd
                           :input  (find direction '(:input  :io))
                           :output (find direction '(:output :io))
                           :element-type 'character
                           :external-format *external-format/iso-8859-1*
                           :output-buffering :none
                           :name "unix terminal")))
    (if input-stream
        (if output-stream
            (setf input-fd      (fd :input  input-stream)
                  output-fd     (fd :output output-stream))
            (setf input-fd      (fd :input  input-stream) 
                  output-stream (make-stream :output output-fd)))
        (if output-stream
            (setf input-stream  (make-stream :inptu  input-fd)
                  output-fd     (fd :output output-stream)) 
            (if (= input-fd output-fd)
                (setf output-stream (setf input-stream (make-stream :io input-fd)))
                (setf input-stream  (make-stream :input  input-fd)
                      output-stream (make-stream :output output-fd))))))
  (setf (slot-value terminal 'input-file-descriptor)   input-fd
        (slot-value terminal 'output-file-descriptor)  output-fd
        (slot-value terminal 'input-stream)            input-stream
        (slot-value terminal 'output-stream)           output-stream)
  terminal)

;; (ccl::stream-device *terminal-io*  :input)
;; (ccl::stream-device *terminal-io*  :output)


(defmethod terminal-initialize ((terminal unix-terminal))
  (with-slots (input-file-descriptor
               saved-termios modern-mode
               vintr vquit vsusp vkill veof veol veol2
               verase vwerase vreprint vstart vstop) terminal
    (when saved-termios
      (error "Calling ~S on a ~S already initialized."
             'terminal-initialize (class-name (class-of terminal))))
    (setf saved-termios (terminal-control-attributes input-file-descriptor))
    (if modern-mode
        (progn
          (let ((ccs (terminal-control-characters input-file-descriptor)))
            (setf vintr    (cdr (assoc :vintr    ccs))
                  vquit    (cdr (assoc :vquit    ccs))
                  vsusp    (cdr (assoc :vsusp    ccs))
                  vkill    (cdr (assoc :vkill    ccs))
                  veof     (cdr (assoc :veof     ccs))
                  veol     (let ((eol (cdr (assoc :veol     ccs))))
                             (if (zerop eol)
                                 #x0d
                                 eol))
                  veol2    (cdr (assoc :veol2    ccs))
                  verase   (cdr (assoc :verase   ccs))
                  vwerase  (cdr (assoc :vwerase  ccs))
                  vreprint (cdr (assoc :vreprint ccs))
                  vstart   (cdr (assoc :vstart   ccs))
                  vstop    (cdr (assoc :vstop    ccs))))
          (iolib.serial:stty
           input-file-descriptor
           :cooked  t

           ;; Input control:
           :istrip  nil ; strip off eigth bit (should be nil for utf-8 input)
           :inlrc   nil ; translate NL to CR on input
           :igncr   nil ; ignore CR  on input
           :icrnl   t   ; translate CR to NL on input.
           ;; :iuclc   nil ; map uppercase to lower (not POSIX).
           :ixon    nil ; XON/XOFF flow control on output.
           :ixoff   nil ; XON/XOFF flow control on input.
           :ixany   nil ; Typing any character to restart stopped output.
           :imaxbel nil ; Ring Bell when input queue is full.
           :iutf8   nil ; UTF-8 input (for character erase in cooked mode).
           ;; We don't process utf-8 on unix-terminal (we would have to decode utf-8 to implement erase ourselves in raw).

           ;; Output control:
           :opost   nil     ; implementation defined output processing.
           ;; :olcuc   nil     ; map lowercase touppercase (not POSIX).
           :onlcr   nil     ; map NL to CR-NL on output.
           :ocrnl   nil     ; map CR to NL on output.
           :onocr   t       ; output CR at column 0.
           :onlret  nil     ; don't output CR (ie. output CR).
           :ofill   nil ; send  fill characters for a delay (instead of timer).
           ;; :ofdel   nil ; (not POSIX, not linux) fill character is NUL; t =  fill character is DEL.
           :nldly   :nl0   ; Newline delay mask (member :nl0 :nl1)
           :crdly   :cr0   ; CR delay mask (member :cr0 :cr1 :cr2 :cr3)
           :tabdly  :tab0 ; TAB delay mask  (member :tab0 :tab1 :tab2 :tab3)
           :bsdly   :bs0  ; Backspace delay mask (member :bs0 :bs1)
           :vtdly   :vt0  ; Vertical tab delay mask (member :vt0 :vt1)
           :ffdly   :ff0  ; Form feed delay mask (member :ff0 :ff1)

           ;; For modem control:
           ;; ;; :cbaud    speed mask
           ;; ;; :cbaudex  speed mask
           ;; :csize    :cs8 ; (member :cs5 :cs6 :cs7 :cs8)
           ;; :cstopb   nil  ; set two stop bits
           ;; :cread    t    ; enable receiver
           ;; :parenb   nil  ; parity generation on  output and parity checking on input.
           ;; :parodd   nil  ;  if set then parity is odd otherwise parity is even.
           ;; :hupcl    nil  ; lowoer modem conotrollines after last  process closes the device (hang-up).
           ;; :clocal   t    ; ignoroe modem control lines.
           ;; :loblk    nil  ; (not POSIX, not linux)
           ;; ;; :cibaud   mask for input speed
           ;; ;; :cmspar   use stick (mark/space) parity.
           ;; :crtscts t    ; hardware flow control.


           ;; Line control:

           ;; We can deal with signals on linux, so go ahead (but not on
           ;; MS-Windows, but there, we'd have to write a windows-terminal
           ;; class…
           
           :isig    nil ; when the character INTR, QUIT, SUSP, or DSUSP are received, generate the signal.
           :icanon  t   ; canonical mode.
           ;; :xcase  nil ; (not POSIX) (and :icanon :xcase) => upper case terminal
           :echoe   t   ;             (and :icanon :echoe) => ERASE and WERASE erase the previous character and word.
           :echok   t   ;             (and :icanon :echok) => KILL  erase current line.
           :echonl  nil ;             (and :icanon :echonl) => echo the NL  even when :echo is t.
           :echoke  nil ;             (and :icanon :echoke) => KILL is echoed by erasing each character on the line (as specified by :echoe and :echoprt).
           :echoprt nil ; (not POSIX) (and :icanon :iecho :echoprt) => characters are printed as they are erased. ( /a\  ??? )
           :echo    t   ; echo of input characters
           :echoctl nil ; (not POSIX) (:echo :echoctl) control codes (not TAB,  NL, START, STOP) are  echoed as ^X
           ;; :defecho nil ; (not POSIX, not linux) Echo only when a process is reading.
           ;; :flusho  nil ; (not POSIX, not linux) output is flushed.  Toggled by the DISCARD character.
           :noflsh  t   ; Disable flushing input and output when signaling INT, QUIT and SUSP.
           :tostop  nil ; sends SIGTTOU to processes who writes to this terminal.
           ;; :pendin  nil ; (not POSIX, not linux) input queue is reprinted when next char is read.
           :iexten  nil ; implementation-defined input-processing. To enable EOL2, LNEXT, REPRINT, WERASE, and IUCLC.

           ;; Character control:
           :vintr    0 ; SIGINT                                    needs :isig t
           :vquit    0 ; SIGQUIT                                   needs :isig t
           :vsusp    0 ; SIGSUSP                                   needs :isig t
           ;; :vdsusp  0 ; (not POSIX, not LINUX) SIGSUSP when read  needs :isig t :iexten t

           :vmin     1 ; minimum number of characters for noncanonical read       
           :vtime    0 ; timeout in decisecond for noncanonical read

           :verase   0 ; erase character                           needs :icanon t
           :vkill    0 ; erase line                                needs :icanon t
           :veof     0 ; send input buffer (= eof when empty)      needs :icanon t
           :veol     0 ; additionnal end of line character         needs :icanon t
           :veol2    0 ; yet additionnal end of line character     needs :icanon t (not POSIX)
           :vwerase  0 ; (not POSIX) word erase                    needs :icanon t :iexten t
           :vreprint 0 ; (not POSIX) reprint unread characters     needs :icanon t :iexten t

           :vstart  0 ; The X-ON character                        needs :ixon
           :vstop   0 ; The X-OFF character                       needs :ixon

           ;; :vswtch   0             ; (not POSIX, not linux) switch character.
           ;; :vlnext   0             ; (not POSIX, not LINUX) literal next       needs :iexten t
           ;; :vdiscard 0             ; (not POSIX, not LINUX) toggle start/stop discarding output needs :iexten t
           ;; :vstatus  0             ; (not POSIX, not LINUX) status request
           ))
        (progn
         ;;  Mitra-15 mode:
          (setf vintr    #x1b ; ESC
                vquit    #x01 ; Ctrl-A
                vsusp    0
                vkill    0
                veof     0
                veol     #x13 ; X-OFF
                veol2    #x0d ; CR
                verase   #x5C ; \
                vwerase  0
                vreprint 0
                vstart   0
                vstop    0)
         (iolib.serial:stty
          input-file-descriptor
          :raw     t

          ;; Input control:
          :istrip  nil ; strip off eigth bit (should be nil for utf-8 input)
          :inlrc   nil ; translate NL to CR on input
          :igncr   nil ; ignore CR  on input
          :icrnl   nil ; translate CR to NL on input.
          ;; :iuclc   nil ; map uppercase to lower (not POSIX).
          :ixon    nil ; XON/XOFF flow control on output. ???
          :ixoff   nil ; XON/XOFF flow control on input.
          :ixany   nil ; Typing any character to restart stopped output.
          :imaxbel nil ; Ring Bell when input queue is full.
          :iutf8   nil ; UTF-8 input (for character erase in cooked mode).
          ;; We don't process utf-8 on unix-terminal (we would have to decode utf-8 to implement erase ourselves in raw).

          ;; Output control:
          :opost   nil     ; implementation defined output processing.
          ;; :olcuc   nil     ; map lowercase touppercase (not POSIX).
          :onlcr   nil     ; map NL to CR-NL on output.
          :ocrnl   nil     ; map CR to NL on output.
          :onocr   t       ; output CR at column 0.
          :onlret  nil     ; don't output CR (ie. output CR).
          :ofill   nil ; send  fill characters for a delay (instead of timer).
          ;; :ofdel   nil ; (not POSIX, not linux) fill character is NUL; t =  fill character is DEL.
          :nldly   :nl0   ; Newline delay mask (member :nl0 :nl1)
          :crdly   :cr0   ; CR delay mask (member :cr0 :cr1 :cr2 :cr3)
          :tabdly  :tab0 ; TAB delay mask  (member :tab0 :tab1 :tab2 :tab3)
          :bsdly   :bs0  ; Backspace delay mask (member :bs0 :bs1)
          :vtdly   :vt0  ; Vertical tab delay mask (member :vt0 :vt1)
          :ffdly   :ff0  ; Form feed delay mask (member :ff0 :ff1)

          ;; For modem control:
          ;; ;; :cbaud    speed mask
          ;; ;; :cbaudex  speed mask
          ;; :csize    :cs8 ; (member :cs5 :cs6 :cs7 :cs8)
          ;; :cstopb   nil  ; set two stop bits
          ;; :cread    t    ; receiver
          ;; :parenb   nil  ; parity generation on  output and parity checking on input.
          ;; :parodd   nil  ; if set then parity is odd otherwise parity is even.
          ;; :hupcl    nil  ; lower modem control lines after last  process closes the device (hang-up).
          ;; :clocal   t    ; ignore modem control lines.
          ;; :loblk    nil  ; (not POSIX, not linux)
          ;; ;; :cibaud   mask for input speed
          ;; ;; :cmspar   use stick (mark/space) parity.
          ;; :crtscts t    ;  hardware flow control.


          ;; Line control:

          ;; Some implementations cannot really deal with signals, or
          ;; at all eg. on MS-Windows, so we'll do without them.
         
          :isig     nil ;  when the character INTR, QUIT, SUSP, or DSUSP are received, generate the signal.
          :icanon   nil ; canonical mode.
          ;; :xcase    nil ; (not POSIX) (and :icanon :xcase) => upper case terminal
          :echoe    nil ;             (and :icanon :echoe) => ERASE and WERASE erase the previous character and word.
          :echok    nil ;             (and :icanon :echok) => KILL  erase current line.
          :echonl   nil ;             (and :icanon :echonl) =>  echo the NL  even when :echo is t.
          :echoke   nil ;             (and :icanon :echoke) => KILL is echoed by erasing each character on the line (as specified by :echoe and :echoprt).
          :echoprt  nil ; (not POSIX) (and :icanon :iecho :echoprt) => characters are printed as they are erased. ( /a\  ??? )
          :echo     t   ; echo of input characters
          :echoctl  nil ; (not POSIX) (:echo :echoctl) control codes (not TAB,  NL, START, STOP) are  echoed as ^X
          ;; :defecho  nil ; (not POSIX, not linux) Echo only when a process is reading.
          ;; :flusho   nil ; (not POSIX, not linux) output is flushed.  Toggled by the DISCARD character.
          :noflsh   t ; Disable flushing input and output when signaling INT, QUIT and SUSP.
          :tostop   nil ; sends SIGTTOU to processes who writes to this terminal.
          ;; :pendin   nil ; (not POSIX, not linux) input queue is reprinted when next char is read.
          :iexten   nil ; implementation-defined input-processing. To enable EOL2, LNEXT, REPRINT, WERASE, and IUCLC.

          ;; Character control:
          :vintr    0 ; SIGINT                                    needs :isig t
          :vquit    0 ; SIGQUIT                                   needs :isig t
          :vsusp    0 ; SIGSUSP                                   needs :isig t
          ;; :vdsusp  0 ; (not POSIX, not LINUX) SIGSUSP when read  needs :isig t :iexten t

          :vmin     1 ; minimum number of characters for noncanonical read       
          :vtime    0 ; timeout in decisecond for noncanonical read

          :verase   0 ; erase character                           needs :icanon t
          :vkill    0 ; erase line                                needs :icanon t
          :veof     0 ; send input buffer (= eof when empty)      needs :icanon t
          :veol     0 ; additionnal end of line character         needs :icanon t
          :veol2    0 ; yet additionnal end of line character     needs :icanon t (not POSIX)
          :vwerase  0 ; (not POSIX) word erase                    needs :icanon t :iexten t
          :vreprint 0 ; (not POSIX) reprint unread characters     needs :icanon t :iexten t

          :vstart  0 ; The X-ON character                        needs :ixon
          :vstop   0 ; The X-OFF character                       needs :ixon

          ;; :vswtch   0             ; (not POSIX, not linux) switch character.
          ;; :vlnext   0             ; (not POSIX, not LINUX) literal next       needs :iexten t
          ;; :vdiscard 0             ; (not POSIX, not LINUX) toggle start/stop discarding output needs :iexten t
          ;; :vstatus  0             ; (not POSIX, not LINUX) status request
          ))))
  terminal)


(defmethod terminal-finalize ((terminal unix-terminal))
  (terminal-finish-output terminal)
  (with-slots (input-file-descriptor saved-termios) terminal
    (unless saved-termios
      (error "Calling ~S on a ~S not initialized."
             'terminal-finalize (class-name (class-of terminal))))
    (setf (terminal-control-attributes input-file-descriptor :flush) saved-termios
          saved-termios nil))
  terminal)



(defmethod terminal-columns ((terminal unix-terminal))
  80)

(defmethod terminal-rows ((terminal unix-terminal))
  25)


(defmethod terminal-ring-bell ((terminal unix-terminal))
  (with-slots (output-stream terminfo) terminal
    (let* ((terminfo:*terminfo* terminfo)
           (bell terminfo:bell))
      (when bell
        (terminfo:tputs bell output-stream))))
  (terminal-finish-output terminal))


(defmethod terminal-carriage-return ((terminal unix-terminal))
  (with-slots (output-stream terminfo) terminal
    (let* ((terminfo:*terminfo* terminfo)
           (carriage-return terminfo:carriage-return))
      (when carriage-return
        (terminfo:tputs carriage-return output-stream))))
  (terminal-finish-output terminal))


(defmethod terminal-line-feed ((terminal unix-terminal) &optional (count 1))
  (with-slots (output-stream terminfo) terminal
    (let* ((terminfo:*terminfo* terminfo)
           ;; (lf-is-nl            terminfo:linefeed-is-newline)
           (line-feed           (or terminfo:linefeed-if-not-lf 
                                    #.(format nil "~C" (code-char LF)))))
      (if line-feed
          (loop
            :repeat count
            :do (terminfo:tputs line-feed output-stream))
          (loop
            :repeat count
            :do (terpri output-stream)))))
  (terminal-finish-output terminal))


(defmethod terminal-new-line ((terminal unix-terminal) &optional (count 1))
  (with-slots (output-stream terminfo) terminal
    (let* ((terminfo:*terminfo* terminfo)
           (carriage-return     terminfo:carriage-return)
           (clr-eol             terminfo:clr-eol))
      (terminfo:tputs clr-eol         output-stream)
      (terminfo:tputs carriage-return output-stream)))
  (terminal-line-feed terminal count))


(defmethod (setf terminal-echo) (new-echo (terminal unix-terminal))
  (with-slots (input-file-descriptor echo modern-mode) terminal
    (unless (eql (not echo) (not new-echo))
      (setf echo (not (not new-echo)))
      (when modern-mode
        (iolib.serial:stty input-file-descriptor :echo echo)))
    echo))




(defun unix-signal (pid signum)
  (iolib.syscalls:kill pid signum))


(defun read-one-char (terminal)
  ;; MITRA-15    UNIX          x
  ;; \           erase         to \"erase\" the previous character.
  ;; C-s (X-OFF) newline       to send input to the computer.
  ;; CR          -             to send input to the computer, but CR is included in input strings.
  ;; ESC         SIGINT char   to interrupt the program.
  ;; C-a (SOH)   SIGQUIT char  to send a signal to the program.
  ;; There's no cursor, but a line of _ followed by a CR is printed on the
  ;; last line, (it's erased before line feed) so that users may see up to
  ;; where they've written.
  (with-slots ((stream input-stream)
               buffer input-finished
               vintr vquit vsusp vkill veof veol veol2
               verase vwerase vreprint) terminal
    (let ((ch (read-char stream)))
      (when ch
        (let ((code (char-code ch)))
          ;; (print `(char read ,ch ,(char-code ch)))
          (cond
            ((zerop code)   #|ignore|#)
            ((= code vintr) (signal 'user-interrupt))
            ((= code vquit) (setf (task-signal *task*) t))
            ((= code vsusp) (unix-signal 0 +SIGSTOP+))
            ((= code vkill) (unix-signal 0 +SIGKILL+))
            (t
             (unless input-finished
               (cond
                 ((= code veof)   #|close the stream|#)
                 ((= code veol)   (setf input-finished t) (terminal-write-string terminal " "))
                 ((= code veol2)  (vector-push-extend ch buffer 1) (setf input-finished t))
                 ((= code verase)
                  (when (plusp (fill-pointer buffer))
                    ;; when modern-mode, erase the character on display
                    (decf (fill-pointer buffer))))
                 ((= code vwerase)
                  ;; when modern-mode, erase the word on display
                  (setf (fill-pointer buffer)
                        (or (position-if-not (function alphanumericp) buffer
                                             :from-end t
                                             :end (or (position-if (function alphanumericp) buffer
                                                                   :from-end t)
                                                      0))
                            0)))
                 ((= code vreprint)
                  ;; We need to keep the line on display (output) to reprint it.
                  )
                 (t
                  (vector-push-extend ch buffer (length buffer))))))))))))




(defmethod terminal-yield ((terminal unix-terminal))
  (loop
    :while (listen (terminal-input-stream terminal))
    :do (read-one-char terminal)))


(defmethod terminal-read-line ((terminal unix-terminal) &key (echo t) (beep nil))
  (with-temporary-echo (terminal echo)
    (when beep
      (terminal-ring-bell terminal))
    (terminal-finish-output terminal)
    (with-slots (buffer input-finished input-read) terminal
      (flet ((finish ()
               (prog1 (subseq buffer input-read)
                 (setf input-finished nil
                       input-read 0
                       (fill-pointer buffer) 0))))
        (if input-finished
            (finish)
            (loop
              (read-one-char terminal)
              (when input-finished
                (return (finish)))))))))


(defgrammar donnee-lse
    :terminals ((nombre " *[-+]?[0-9]+(.[0-9]+([Ee][-+]?[0-9]+?)?)?($| *)"))
    :skip-spaces nil
    :start donnee-lse
    :rules ((--> donnee-lse
                 nombre :action (multiple-value-list
                                 (read-from-string (second $1))))))


(defmethod terminal-read ((terminal unix-terminal) &key (echo t) (beep nil))
  (with-temporary-echo (terminal echo)
    (when beep
      (terminal-ring-bell terminal))
    (terminal-finish-output terminal)
    (with-slots (buffer input-finished input-read) terminal
      (flet ((finish ()
               (handler-case
                   (destructuring-bind (donnee position) (parse-donnee-lse buffer :start input-read)
                     (if (< position (length buffer))
                         (setf input-read position)
                         (setf input-read 0
                               (fill-pointer buffer) 0
                               input-finished nil))
                     donnee)
                 (error ()
                   (let ((donnee (subseq buffer input-read)))
                     (setf input-read 0
                           (fill-pointer buffer) 0
                           input-finished nil)
                     (lse-error "DONNEE INVALIDE ~S, ATTENDU UN NOMBRE" donnee))))))
        (if input-finished
            (finish)
            (loop
              (read-one-char terminal)
              (when input-finished
                (return (finish)))))))))


(defun test/unix-terminal ()
  (let ((*term* (make-instance 'unix-terminal))
        (*task* (make-instance 'task)))
    (terminal-initialize *term*)
    (handler-case 
        (unwind-protect
             (flet ((line ()
                      (terminal-new-line *term* 2)
                      (terminal-write-string *term* "----------------------------------------")
                      (terminal-new-line *term*)))
               (line)
               (progn
                 (terminal-write-string *term* "Hello World!")
                 (terminal-carriage-return *term*)
                 (terminal-write-string *term* "HELLO"))
               (line)
               (progn
                 (terminal-write-string *term* "Hello")
                 (terminal-line-feed *term*)
                 (terminal-write-string *term* "world"))
               (line)
               (progn
                 (terminal-write-string *term* "Hello")
                 (terminal-line-feed *term* 2)
                 (terminal-write-string *term* " world")
                 (terminal-carriage-return *term*)
                 (terminal-write-string *term* "HELLO"))
               (line)
               (progn
                 (terminal-carriage-return *term*)
                 (terminal-write-string *term* "____________________________________________________________")
                 (terminal-carriage-return *term*)
                 (terminal-write-string *term* "Enter a line: ")
                 (terminal-finish-output *term*)
                 (print (list (terminal-read-line *term*)
                              (task-signal *task*)
                              (task-interruption *task*)))
                 (terminal-new-line *term*)
                 ;; (terminal-carriage-return *term*)
                 ;; (terminal-write-string *term* "DONE----------")
                 )
               (line)
               (progn
                 (terminal-carriage-return *term*)
                 (terminal-write-string *term* "____________________________________________________________")
                 (terminal-carriage-return *term*)
                 (terminal-write-string *term* "Enter two numbers: ")
                 (terminal-finish-output *term*)
                 (print (list (list (terminal-read *term*)
                                    (task-signal *task*)
                                    (task-interruption *task*))
                              (list (terminal-read *term*)
                                    (task-signal *task*)
                                    (task-interruption *task*))))
                 (terminal-new-line *term*)
                 ;; (terminal-carriage-return *term*)
                 ;; (terminal-write-string *term* "DONE----------")
                 )
               (line))
          (terminal-finalize *term*))
      (user-interrupt (condition)
        (format t "~%Interrupted: ~A~%"  (user-interrupt-signal condition))
        (finish-output)
        (return-from test/unix-terminal (list :interrupted condition)))))
  :success)




(defun test/termios ()
  (let ((termios (terminal-control-attributes 0)))
    (mapcar (lambda (cc)
              (let ((code  (terminal-control-character termios cc)))
               (list cc code (format nil "^~C" (code-char (logand #x7f (+ 64 code)))))))
            '(:vmin :vtime :vintr :vquit :vsusp #|:vdsusp|# :verase :vkill
              :veof :veol :veol2 :vwerase :vreprint :vstart :vstop))))

(defparameter *term* (make-instance 'unix-terminal))


;;;; THE END ;;;;
