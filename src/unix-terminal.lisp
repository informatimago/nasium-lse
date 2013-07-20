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
;;;;    Copyright Pascal J. Bourguignon 2012 - 2013
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



(defun termios-attributes (fd &optional action)
  "Allocates a new termios foreign structure, fills it with current
values for the specified file descriptor FD, and return it.
ACTION is ignored."
  (declare (ignore action))
  (let ((termios (cffi:foreign-alloc 'iolib.serial:termios)))
    (iolib.serial::%tcgetattr fd termios)
    termios))


(defun (setf termios-attributes) (new-termios fd action)
  "Sets the given NEW-TERMIOS (which has been obtained with the
TERMIOS-ATTRIBUTES function to the specified file descriptor FD.

ACTION specifies when the setting occurs:
        :now    immediately
        :drain  after all output has been transmitted.
        :flush  after all output has been transmitted, and all input is discaded.
"
  (check-type action (member :now :drain :flush))
  (iolib.serial::%tcsetattr fd (ecase action
                                (:now :tcsanow)
                                (:drain :tcsadrain)
                                (:flush :tcsaflush))
                           new-termios)
  new-termios)




(defun termios-flag (termios flag)
  "Returns the flag from the corresponding TERMIOS field."
  (let ((type (iolib.serial::which-termios-keyword flag)))
    (unless type
      (error "Unknown termios option ~a" flag))
    (not (plusp (logand (cffi:foreign-slot-value termios 'iolib.serial:termios type)
                        (cffi:foreign-enum-value type flag))))))


(defun termios-control-character (termios cc)
  "Returns the control character value."
  (cffi:mem-aref (cffi:foreign-slot-pointer termios
                                            'iolib.serial:termios
                                            'iolib.serial::control-chars)
                 'iolib.serial::cc
                 ;; constant name is offset
                 (cffi:foreign-enum-value 'iolib.serial:control-character cc)))


(defun termios-free (termios)
  "Deallocate the TERMIOS structure returned by TERMIOS-ATTRIBUTES."
  (cffi:foreign-free termios))


(defmacro with-termios-attributes ((termios-variable fd) &body body)
  "Binds the variable passed to TERMIOS-VARIABLE to a termios
structure returned by (termios-attributes fd), and execute
\(progn BODY) in an unwind-protect.  The cleanup dealloocates the
termios with TERMIOS-FREE."
  (let ((vtermios (gensym)))
    `(let* ((,vtermios (termios-attributes ,fd))
            (,termios-variable ,vtermios))
       (unwind-protect  (progn ,@body)
         (termios-free ,vtermios)))))


(defun lispify-flags (termios)
  "Return an a-list mapping flags keywords to code."
  (mapcan (lambda (flag) (list flag (termios-flag termios flag)))
          '(
            ;; cflags
            #+(or linux bsd) :cbaud
            #+(or linux bsd) :cbaudex
            :csize
            :cstopb
            :cread
            :parenb
            :parodd
            :hupcl
            :clocal
            #-linux :loblk
            #+(or linux bsd) :cibaud
            #+(or linux bsd) :cmspar
            #+(or linux bsd) :crtscts
            ;; lflags
            :isig
            :icanon
            #-linux :xcase
            :echo
            :echoe
            :echok
            :echonl
            #+(or linux bsd) :echoctl
            #+(or linux bsd) :echoprt
            #+(or linux bsd) :echoke
            #-linux :defecho
            #+bsd :flusho
            :noflsh
            :tostop
            #+bsd :pendin
            :iexten
            ;; iflags
            :ignbrk
            :brkint
            :ignpar
            :parmrk
            :inpck
            :istrip
            :inlcr
            :igncr
            :icrnl
            #+linux :iuclc
            :ixon
            ;; XSI features are #+xfi marked in sb-posix grovel file,
            ;; but (find xsi *features*) return NIL
            ;; so i'm leaving xsi features unmarked 
            :ixany
            :ixoff
            #-linux :imaxbel
            #+linux :iutf8
            ;; oflags
            :opost
            #+linux :olcuc
            :onlcr
            :ocrnl
            :onocr
            :onlret
            :ofill
            #-linux :ofdel
            #+(or linux bsd) :nldly
            #+(or linux bsd) :crdly
            #+(or linux bsd) :tabdly
            #+(or linux bsd) :bsdly
            #+(or linux bsd) :vtdly
            #+(or linux bsd) :ffdly)))


(defun lispify-control-characters (termios)
  "Return an a-list mapping control-character keywords to code."
  (mapcan (lambda (cc) (list cc (termios-control-character termios cc)))
          '(:vintr
            :vquit
            :verase
            :vkill
            :veof
            :vmin
            :veol
            :vtime
            #+linux :veol2
            #-linux :vswtch
            :vstart
            :vstop
            :vsusp
            #-linux :vdsusp
            #+linux :vlnext
            #+linux :vwerase
            #+linux :vreprint
            #-linux :vdiscard
            #-linux :vstatus)))


(defun lispify-attributes (termios)
  (append (lispify-flags termios) (lispify-control-characters termios)))


(defun compare-terminal-attributes (old new)
    "
RETURN: A sublist of options that didn't change successfully;
        A sublist of options successfully changed.
"

    (loop
      :with different = '()
      :with same      = '()
      :for (okey oval) :on old :by (function cddr)
      :for (nkey nval) :on new :by (function cddr)
      :do (progn
            (unless (eq okey nkey)
              (error "~S internal error ~S /= ~S"
                     'compare-terminal-attributes okey nkey))
            (if (equal oval nval)
                (progn
                  (push okey same)
                  (push oval same))
                (progn
                  (push nkey different)
                  (push nval different))))
      :finally (return (values (nreverse different)
                               (nreverse same)))))


(defun compare-termios (old new)
  "
RETURN: A sublist of options that didn't change successfully;
        A sublist of options successfully changed.
"
  (compare-terminal-attributes (lispify-attributes old)
                               (lispify-attributes new)))





(defun stty (serial &rest options &key
             RAW COOKED EVENP ODDP speed input-speed output-speed
             ;; cflags
             #+(or linux bsd) cbaud
             #+(or linux bsd) cbaudex
             csize cs5 cs6 cs7 cs8
             cstopb
             cread
             parenb
             parodd
             hupcl
             clocal
             #-linux loblk
             #+(or linux bsd) cibaud
             #+(or linux bsd) cmspar
             #+(or linux bsd) crtscts
             ;; lflags
             isig
             icanon
             #-linux xcase
             echo
             echoe
             echok
             echonl
             #+(or linux bsd) echoctl
             #+(or linux bsd) echoprt
             #+(or linux bsd) echoke
             #-linux defecho
             #+bsd flusho
             noflsh
             tostop
             #+bsd pendin
             iexten
             ;; iflags
             ignbrk
             brkint
             ignpar
             parmrk
             inpck
             istrip
             inlcr
             igncr
             icrnl
             #+linux iuclc
             ixon
             ;; XSI features are #+xfi marked in sb-posix grovel file,
             ;; but (find xsi *features*) return NIL
             ;; so i'm leaving xsi features unmarked 
             ixany
             ixoff
             #-linux imaxbel
             #+linux iutf8
             ;; oflags
             opost
             #+linux olcuc
             onlcr
             ocrnl
             onocr
             onlret
             ofill
             #-linux ofdel
             #+(or linux bsd) nldly  #+(or linux bsd) nl0 #+(or linux bsd) nl1
             #+(or linux bsd) crdly  #+(or linux bsd) cr0 #+(or linux bsd) cr1 #+(or linux bsd) cr2 #+(or linux bsd) cr3
             #+(or linux bsd) tabdly #+(or linux bsd) tab0 #+(or linux bsd) tab1 #+(or linux bsd) tab2 #+(or linux bsd) tab3
             #+(or linux bsd) bsdly  #+(or linux bsd) bs0 #+(or linux bsd) bs1
             #+(or linux bsd) vtdly  #+(or linux bsd) vt0 #+(or linux bsd) vt1
             #+(or linux bsd) ffdly  #+(or linux bsd) ff0 #+(or linux bsd) ff1
             
             ;; control characters             
             vintr
             vquit
             verase
             vkill
             veof
             vmin
             veol
             vtime
             #+linux veol2
             #-linux vswtch
             vstart
             vstop
             vsusp
             #-linux vdsusp
             #+linux vlnext
             #+linux vwerase
             #+linux vreprint
             #-linux vdiscard
             #-linux vstatus
             )
  "
DO:       Implement stty(1) in a lispy way.
SERIAL:   can be a stream or a file descriptoro.
OPTIONS:  should be p-list of termios keywords and values:
          NIL or T for flags or one of :RAW, :COOKED, :EVENP or :ODDP,
          or integers for control characters or :SPEED.
EXAMPLES:  

   :inlcr t         set corresponding flag,
   :inlcr nil       reset it,
   :speed 115200    set corresponding speed,
   :vtime 0         setup corresponding control character value.
   
   Setup for 8n1 mode:          (stty fd :evenp nil)
   Setup speed:                 (stty fd :speed 115200) or (stty my-stream :speed 115200)
   Setup raw mode and speed:    (stty fd :speed 11520 :raw t) 
   Setup cooked mode:           (stty fd :raw nil)

RETURN: A sublist of options that didn't change successfully;
        A sublist of options successfully changed.
"

  ;; (progn (print (cons 'stty options)) (terpri) (finish-output))
  (let ((fd (etypecase serial
              (integer serial)
              (stream  (iolib.serial::fd-of serial)))))
    (flet ((speed-to-baud (speed)
             (intern (format nil "B~A" speed) "KEYWORD")))
      (cffi:with-foreign-objects ((termios 'iolib.serial:termios)
                                  (newterm 'iolib.serial:termios))
        (iolib.serial::%tcgetattr fd termios)
        (loop
          :for (key value) :on options :by (function cddr)
          :do (case key
                (:speed        (let ((baud (speed-to-baud speed)))
                                 (iolib.serial::%cfsetispeed termios baud)
                                 (iolib.serial::%cfsetospeed termios baud)))
                (:input-speed  (let ((baud (speed-to-baud speed)))
                                 (iolib.serial::%cfsetispeed termios baud)))
                (:output-speed (let ((baud (speed-to-baud speed)))
                                 (iolib.serial::%cfsetospeed termios baud)))
                (:RAW          (if raw
                                   (iolib.serial::make-raw-termios    termios)
                                   (iolib.serial::make-cooked-termios termios)))
                (:COOKED       (if cooked
                                   (iolib.serial::make-cooked-termios termios)
                                   (iolib.serial::make-raw-termios    termios)))
                (:EVENP        (if evenp
                                   (iolib.serial::make-evenp-termios termios)
                                   (iolib.serial::make-oddp-termios  termios)))
                (:ODDP         (if evenp
                                   (iolib.serial::make-oddp-termios  termios)
                                   (iolib.serial::make-evenp-termios termios)))
                (otherwise
                 (cond
                   ((iolib.serial::termios-flag-p key)
                    (iolib.serial::setup-termios-flag termios key value))
                   ((iolib.serial::termios-control-character-p key)
                    (iolib.serial::setup-termios-control-character termios key value))
                   (t
                    (error "Invalid option: ~S ~S" key value))))))
        (iolib.serial::%tcsetattr fd :tcsanow termios)
        (iolib.serial::%tcgetattr fd newterm)
        (compare-termios termios newterm)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;

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
This must be set before TERMINAL-INITIALIZE is called.
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
   (cr-as-xoff             :initarg :cr-as-xoff
                           :accessor terminal-cr-as-xoff
                           :documentation "
When true, CR works like XOFF, without being read into strings.
Valid only whe MODERN-MODE is false.
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
   (vintr                  :initform 0   :reader terminal-vintr)
   (vquit                  :initform 0   :reader terminal-vquit)
   (vsusp                  :initform 0   :reader terminal-vsusp)
   (vkill                  :initform 0   :reader terminal-vkill)
   (veof                   :initform 0   :reader terminal-veof)
   (veol                   :initform 0   :reader terminal-veol)
   (veol2                  :initform 0   :reader terminal-veol2)
   (verase                 :initform 0   :reader terminal-verase)
   (vwerase                :initform 0   :reader terminal-vwerase)
   (vreprint               :initform 0   :reader terminal-vreprint)
   (vstart                 :initform 0   :reader terminal-vstart)
   (vstop                  :initform 0   :reader terminal-vstop)))


(defgeneric (setf terminal-modern-mode) (new-mode terminal))

(defmethod (setf terminal-modern-mode) (new-mode (terminal unix-terminal))
  (with-slots (modern-mode
               input-file-descriptor
               vintr vquit vsusp vkill veof veol veol2
               verase vwerase vreprint vstart vstop) terminal
    (if new-mode
        ;; Modern mode: get the characters from the termios.
        (let ((ccs (lispify-control-characters
                    (termios-attributes input-file-descriptor))))
          (setf vintr    (or (plist-get ccs :vintr) 0)
                vquit    (or (plist-get ccs :vquit) 0)
                vsusp    (or (plist-get ccs :vsusp) 0)
                vkill    (or (plist-get ccs :vkill) 0)
                veof     (or (plist-get ccs :veof)  0)
                veol     (let ((eol (plist-get ccs :veol)))
                           (if (and eol (< 0 eol 32))
                               eol
                               #x0d))
                veol2    (or (plist-get ccs :veol2)    0)
                verase   (or (plist-get ccs :verase)   0)
                vwerase  (or (plist-get ccs :vwerase)  0)
                vreprint (or (plist-get ccs :vreprint) 0)
                vstart   (or (plist-get ccs :vstart)   0)
                vstop    (or (plist-get ccs :vstop)    0)))
        ;;  Mitra-15 mode:
        (setf vintr    #x1b            ; ESC
              vquit    #x01            ; Ctrl-A
              vsusp    0
              vkill    0
              veof     0
              veol     #x13            ; X-OFF
              veol2    #x0d            ; CR
              verase   #x5C            ; \
              vwerase  0
              vreprint 0
              vstart   0
              vstop    0))
    ;; #+developing
    ;; (progn
    ;;   (format *trace-output* "~%TERMINAL MODE = ~:[OLD~;MODERN~]~%" new-mode)
    ;;   (format *trace-output* "~@{~12A ~A~%~}"
    ;;           :vintr    vintr    
    ;;           :vquit    vquit    
    ;;           :vsusp    vsusp    
    ;;           :vkill    vkill    
    ;;           :veof     veof     
    ;;           :veol     veol     
    ;;           :veol2    veol2    
    ;;           :verase   verase   
    ;;           :vwerase  vwerase  
    ;;           :vreprint vreprint 
    ;;           :vstart   vstart   
    ;;           :vstop    vstop))
    ;; (progn (print (list :vintr vintr :vquit vquit :vsusp vsusp  :vkill vkill :veof  veof :veol veol :veol2 veol2 :verase verase :vwerase vwerase :vreprint vreprint)) (terpri) (finish-output))
    (setf modern-mode new-mode)))




 (defmethod initialize-instance :after ((terminal unix-terminal) &rest args
                                       &key modern-mode
                                       input-stream output-stream input-fd output-fd)
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
        (slot-value terminal 'output-stream)           output-stream
        (terminal-modern-mode terminal) modern-mode)
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
    (setf saved-termios (termios-attributes input-file-descriptor))
    (let ((common '(
                    :raw     t
                    :echo    t
                    :echoctl nil
                    ;; :echonl  nil
                    
                    ;; ;; Input control:
                    ;; :istrip  nil ; strip off eigth bit (should be nil for utf-8 input)
                    ;; :igncr   nil ; ignore CR  on input
                    ;; :ixon    nil ; XON/XOFF flow control on output.
                    ;; :ixoff   nil ; XON/XOFF flow control on input.
                    ;; :ixany   nil ; Typing any character to restart stopped output.
                    ;; :iutf8   nil ; UTF-8 input (for character erase in cooked mode).
                    ;; ;; We don't process utf-8 on unix-terminal (we would have to
                    ;; ;; decode utf-8 to implement erase ourselves in raw).
                    ;; 
                    ;; ;; Output control:
                    ;; :opost   nil ; implementation defined output processing.
                    ;; :onlcr   nil ; map NL to CR-NL on output.
                    ;; :ocrnl   nil ; map CR to NL on output.
                    ;; :onocr   t   ; output CR at column 0.
                    ;; :onlret  nil ; don't output CR.
                    ;; :ofill   nil ; send  fill characters for a delay (instead of timer).
                    ;; 
                    ;; ;; Line control:
                    ;; :isig    nil ; when the character INTR, QUIT, SUSP, or DSUSP are received, generate the signal.
                    ;; :echonl  nil ;             (and :icanon :echonl) => echo the NL  even when :echo is nil.
                    ;; :echoke  nil ;             (and :icanon :echoke) => KILL is echoed by erasing each character on the line (as specified by :echoe and :echoprt).
                    ;; :echoprt nil ; (not POSIX) (and :icanon :iecho :echoprt) => characters are printed as they are erased. ( /a\  ??? )
                    ;; :echo    t   ; echo of input characters
                    ;; :echoctl nil ; (not POSIX) (:echo :echoctl) control codes (not TAB,  NL, START, STOP) are  echoed as ^X
                    ;; :noflsh  t   ; Disable flushing input and output when signaling INT, QUIT and SUSP.
                    ;; :tostop  nil ; sends SIGTTOU to processes who writes to this terminal.
                    ;; :iexten  nil ; implementation-defined input-processing. To enable EOL2, LNEXT, REPRINT, WERASE, and IUCLC.

                    ))
          (modern '(
                    ;; Input control:
                    :icrnl   t   ; translate CR to NL on input.
                    ;; Line control:
                    :icanon  t   ; canonical mode.
                    :echoe   t   ;             (and :icanon :echoe) => ERASE and WERASE erase the previous character and word.
                    :echok   t   ;             (and :icanon :echok) => KILL  erase current line.
                    ))
          (old    '(
                    ;; Input control:
                    :icrnl   nil ; translate CR to NL on input.
                    ;; Line control:
                    :icanon  nil ; canonical mode.
                    :echoe   nil ;             (and :icanon :echoe) => ERASE and WERASE erase the previous character and word.
                    :echok   nil ;             (and :icanon :echok) => KILL  erase current line.
                    )))
      (multiple-value-bind (diff same) 
          (apply (function stty) input-file-descriptor
                 ;; Character control:
                 :vintr    0; vintr     ; it looks like codes configured as interrupts
                 :vquit    0; vquit     ; are not transmitted even with :isig nil.
                 :vsusp    0; vsusp    
                 :vkill    0; vkill    
                 :veof     veof     
                 :veol     veol     
                 :verase   verase   
                 :vstart   vstart   
                 :vstop    vstop
                 #+linux :veol2    #+linux veol2 ; yet additionnal end of line character     needs :icanon t (not POSIX)
                 #+linux :vwerase  #+linux vwerase ; (not POSIX) word erase                    needs :icanon t :iexten t
                 #+linux :vreprint #+linux vreprint ; (not POSIX) reprint unread characters     needs :icanon t :iexten t
                 :vmin  1
                 :vtime 0
                 (append ;; (if modern-mode modern old)
                  common))
        (declare (ignorable same))
        #+swank (print same *terminal-io*)
        (when diff (warn "stty couldn't set those attributes: ~S" diff)))))
  terminal)


(defmethod terminal-finalize ((terminal unix-terminal))
  (terminal-finish-output terminal)
  (with-slots (input-file-descriptor saved-termios) terminal
    (unless saved-termios
      (error "Calling ~S on a ~S not initialized."
             'terminal-finalize (class-name (class-of terminal))))
    (setf (termios-attributes input-file-descriptor :flush) saved-termios
          saved-termios nil))
  terminal)



(defmethod terminal-columns ((terminal unix-terminal))
  (with-slots (terminfo) terminal
   (let ((terminfo:*terminfo* terminfo))
     (or terminfo:columns 80))))


(defmethod terminal-rows ((terminal unix-terminal))
  (with-slots (terminfo) terminal
   (let ((terminfo:*terminfo* terminfo))
     (or terminfo:lines 25))))


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



;; For terminal-new-line we cannot clr-eol, since after a CR, we may
;; not be at the end of the line. Notably, when input is valided with
;; an echoed CR, we're at the beginning of the line. Furthermore, when
;; input is valided with CR, we must output a LF, since the CR has
;; been echoed by the terminal, and that would mess things if we had
;; other input as in LIRE A,B,C. (X-OFF would not print a space, but
;; the user may type SPC instead of X-OFF or CR to validate a
;; numerical input to LIRE).

(defmethod terminal-new-line ((terminal unix-terminal) &optional (count 1))
  (with-slots (output-stream terminfo) terminal
    (let* ((terminfo:*terminfo* terminfo)
           (carriage-return     terminfo:carriage-return)
           ;;(clr-eol             terminfo:clr-eol)
           )
      ;; (terminfo:tputs clr-eol         output-stream)
      (terminfo:tputs carriage-return output-stream)))
  (terminal-line-feed terminal count))


(defmethod (setf terminal-echo) (new-echo (terminal unix-terminal))
  (with-slots (input-file-descriptor echo modern-mode) terminal
    (unless (eql (not echo) (not new-echo))
      (setf echo (not (not new-echo)))
      (when modern-mode
        (iolib.serial:stty input-file-descriptor :echo echo)))
    echo))


(defmethod terminal-erase-character ((terminal unix-terminal) &optional (count 1))
  (with-slots (output-stream terminfo) terminal
    (let* ((terminfo:*terminfo* terminfo)
           (cub                 terminfo:parm-left-cursor)
           (ech                 terminfo:erase-chars))
      (if cub
          (terminfo:tputs cub output-stream count)
          (let ((cub1 terminfo:cursor-left))
            (when cub1
              (loop :repeat count :do (terminfo:tputs cub1 output-stream)))))
      (if ech
          (terminfo:tputs ech output-stream count)
          (terminal-write-string terminal (make-string count :initial-element #\space))))
    (terminal-finish-output terminal)))


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
               verase vwerase vreprint
               modern-mode) terminal
    (let ((ch (read-char stream)))
      (when ch
        (let ((code (char-code ch)))
          ;; (print `(char read ,ch ,(char-code ch))) (finish-output)
          (cond
            ((zerop code)   #|ignore|#)
            ((= code vintr) (signal 'user-interrupt))
            ((= code vquit) (setf (task-signal *task*) t))
            ((= code vsusp) (unix-signal 0 +SIGSTOP+))
            ((= code vkill) (unix-signal 0 +SIGKILL+))
            (t
             (unless input-finished
               (cond
                 ((and veof  (= code veof))   (setf input-finished t) #|close the stream|#)
                 ((and veol  (= code veol))   (setf input-finished t))
                 ((and veol2 (= code veol2))  (setf input-finished t)
                  (unless (terminal-cr-as-xoff terminal)
                    (vector-push-extend ch buffer 1)))
                 ((and verase (= code verase))
                  (when (plusp (fill-pointer buffer))
                    (when modern-mode
                      ;; when modern-mode, erase the character on display
                      (terminal-erase-character terminal))
                    (decf (fill-pointer buffer))))
                 ((and vwerase (= code vwerase))
                  (let ((wsize (fill-pointer buffer)))
                    (setf (fill-pointer buffer)
                          (or (position-if-not (function alphanumericp) buffer
                                               :from-end t
                                               :end (or (position-if (function alphanumericp) buffer
                                                                     :from-end t)
                                                        0))
                              0))
                    (when modern-mode
                      ;; when modern-mode, erase the word on display
                      (terminal-erase-character terminal (- wsize (fill-pointer buffer))))))
                 ((and vreprint (= code vreprint))
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
              #+swank (print (list buffer input-finished input-read) *terminal-io*)
              (read-one-char terminal)
              (when input-finished
                (return (finish)))))))))



(defmethod terminal-read ((terminal unix-terminal) &key (echo t) (beep nil))
  (format *trace-output* "~&~A~%" 'unix-terminal)
  (with-temporary-echo (terminal echo)
    (when beep
      (terminal-ring-bell terminal))
    (terminal-finish-output terminal)
    (with-slots (buffer input-finished input-read) terminal
      (flet ((finish ()
               (handler-case
                   (destructuring-bind (donnee position) (parse-donnee-lse (subseq buffer input-read))
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
              #+swank (print (list buffer input-finished input-read) *terminal-io*)
              (read-one-char terminal)
              (when input-finished
                (return (finish)))))))))


(defmethod terminal-key ((terminal unix-terminal) keysym)
  (declare (ignorable terminal))
  (let ((code (funcall (ecase keysym
                         (:escape    (function terminal-vintr))
                         (:attention (function terminal-vquit))
                         (:xoff      (function terminal-veol))
                         (:delete    (function terminal-verase))
                         (:return    (function terminal-veol2)))
                       terminal)))
    (cond
      ((or (null code) (zerop code)) "(PAS DISPONIBLE)")
      ((= code  13) "[ENTRÉE]")
      ((= code  27) "[ÉCHAPEMENT]")
      ((< code  32) (format nil "[CONTRÔLE-~C]" (code-char (logand #x7f (+ 64 code)))))
      ((= code  32) "[ESPACE]")
      ((= code 127) "[EFFACEMENT]")
      (t (format nil "[~A]" (code-char code))))))






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
  (let ((termios (termios-attributes 0)))
    (mapcar (lambda (cc)
              (let ((code  (termios-control-character termios cc)))
                (list cc code (format nil "^~C" (code-char (logand #x7f (+ 64 code)))))))
            '(:vmin :vtime :vintr :vquit :vsusp #|:vdsusp|# :verase :vkill
              :veof :veol
              #-darwin :veol2
              #-darwin :vwerase
              #-darwin :vreprint
              :vstart :vstop))))

;; (defparameter *term* (make-instance 'unix-terminal))



;;;; THE END ;;;;
