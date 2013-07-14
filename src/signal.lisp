;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               signal.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Exports a couple of macros to deal portably with unix signals.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-02-20 <PJB> Added user-interrupt and yield-signals.
;;;;    2012-02-01 <PJB> Created.
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
;;;;    along with this program.  If not, see http://www.gnu.org/licenses/
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.SIGNAL"
  (:use "COMMON-LISP")
  (:export
   "USER-INTERRUPT" "USER-INTERRUPT-SIGNAL"
   "YIELD-SIGNALS" "SIGNAL-HANDLER-BIND" "CATCHING-SIGNALS"
   
   "+SIGHUP+" "+SIGINT+" "+SIGQUIT+" "+SIGILL+" "+SIGTRAP+"
   "+SIGABRT+" "+SIGBUS+" "+SIGFPE+" "+SIGKILL+" "+SIGUSR1+"
   "+SIGSEGV+" "+SIGUSR2+" "+SIGPIPE+" "+SIGALRM+" "+SIGTERM+"
   "+SIGCHLD+" "+SIGCONT+" "+SIGSTOP+" "+SIGTSTP+" "+SIGTTIN+"
   "+SIGTTOU+" "+SIGURG+" "+SIGXCPU+" "+SIGXFSZ+" "+SIGVTALRM+"
   "+SIGPROF+" "+SIGWINCH+" "+SIGIO+" "+SIGSYS+")
  (:documentation "
This package exports macros to help unix signal handling.
"))
(in-package "COM.INFORMATIMAGO.SIGNAL")


;;;---------------------------------------------------------------------
;;; clisp has glibc bindings in the linux module, but it cannot always
;;; be loaded.  On unix we may still access unix functions with FFI.
;;;---------------------------------------------------------------------

#+clisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (progn (require "linux")
             (pushnew :linux *features*))
    (error () (values))))


#+(and clisp linux)
(progn
  (defconstant   +SIGHUP+        linux:|SIGHU|        "Hangup (POSIX).")
  (defconstant   +SIGINT+        linux:|SIGIN|        "Interrupt (ANSI).")
  (defconstant   +SIGQUIT+       linux:|SIGQUI|       "Quit (POSIX).")
  (defconstant   +SIGILL+        linux:|SIGIL|        "Illegal instruction (ANSI).")
  (defconstant   +SIGTRAP+       linux:|SIGTRA|       "Trace trap (POSIX).")
  (defconstant   +SIGABRT+       linux:|SIGABR|       "Abort (ANSI).")
  (defconstant   +SIGIOT+        linux:|SIGIO|        "IOT trap (4.2 BSD).")
  (defconstant   +SIGBUS+        linux:|SIGBU|        "BUS error (4.2 BSD).")
  (defconstant   +SIGFPE+        linux:|SIGFP|        "Floating-point exception (ANSI).")
  (defconstant   +SIGKILL+       linux:|SIGKIL|       "Kill, unblockable (POSIX).")
  (defconstant   +SIGUSR1+       linux:|SIGUSR|       "User-defined signal 1 (POSIX).")
  (defconstant   +SIGSEGV+       linux:|SIGSEG|       "Segmentation violation (ANSI).")
  (defconstant   +SIGUSR2+       linux:|SIGUSR|       "User-defined signal 2 (POSIX).")
  (defconstant   +SIGPIPE+       linux:|SIGPIP|       "Broken pipe (POSIX).")
  (defconstant   +SIGALRM+       linux:|SIGALR|       "Alarm clock (POSIX).")
  (defconstant   +SIGTERM+       linux:|SIGTER|       "Termination (ANSI).")
  (defconstant   +SIGSTKFLT+     linux:|SIGSTKFL|     "Stack fault.")
  (defconstant   +SIGCHLD+       linux:|SIGCHL|       "Child status has changed (POSIX).")
  (defconstant   +SIGCLD+        linux:|SIGCL|        "Same as SIGCHLD (System V).")
  (defconstant   +SIGCONT+       linux:|SIGCON|       "Continue (POSIX).")
  (defconstant   +SIGSTOP+       linux:|SIGSTO|       "Stop, unblockable (POSIX).")
  (defconstant   +SIGTSTP+       linux:|SIGTST|       "Keyboard stop (POSIX).")
  (defconstant   +SIGTTIN+       linux:|SIGTTI|       "Background read from tty (POSIX).")
  (defconstant   +SIGTTOU+       linux:|SIGTTO|       "Background write to tty (POSIX).")
  (defconstant   +SIGURG+        linux:|SIGUR|        "Urgent condition on socket (4.2 BSD).")
  (defconstant   +SIGXCPU+       linux:|SIGXCP|       "CPU limit exceeded (4.2 BSD).")
  (defconstant   +SIGXFSZ+       linux:|SIGXFS|       "File size limit exceeded (4.2 BSD).")
  (defconstant   +SIGVTALRM+     linux:|SIGVTALR|     "Virtual alarm clock (4.2 BSD).")
  (defconstant   +SIGPROF+       linux:|SIGPRO|       "Profiling alarm clock (4.2 BSD).")
  (defconstant   +SIGWINCH+      linux:|SIGWINC|      "Window size change (4.3 BSD, Sun).")
  (defconstant   +SIGIO+         linux:|SIGI|         "I/O now possible (4.2 BSD).")
  (defconstant   +SIGPOLL+       linux:|SIGPOL|       "Pollable event occurred (System V).")
  (defconstant   +SIGPWR+        linux:|SIGPW|        "Power failure restart (System V).")
  (defconstant   +SIGSYS+        linux:|SIGSY|        "Bad system call.")
  (defconstant   +SIGUNUSED+     linux:|SIGUNUSE|))


#+(and unix ecl)
(progn
  (defconstant +SIGHUP+     SI::+SIGHUP+)
  (defconstant +SIGINT+     SI::+SIGINT+)
  (defconstant +SIGQUIT+    SI::+SIGQUIT+)
  (defconstant +SIGILL+     SI::+SIGILL+)
  (defconstant +SIGTRAP+    SI::+SIGTRAP+)
  (defconstant +SIGABRT+    SI::+SIGABRT+)
  (defconstant +SIGBUS+     SI::+SIGBUS+)
  (defconstant +SIGFPE+     SI::+SIGFPE+)
  (defconstant +SIGKILL+    SI::+SIGKILL+)
  (defconstant +SIGUSR1+    SI::+SIGUSR1+)
  (defconstant +SIGSEGV+    SI::+SIGSEGV+)
  (defconstant +SIGUSR2+    SI::+SIGUSR2+)
  (defconstant +SIGPIPE+    SI::+SIGPIPE+)
  (defconstant +SIGALRM+    SI::+SIGALRM+)
  (defconstant +SIGTERM+    SI::+SIGTERM+)
  (defconstant +SIGCHLD+    SI::+SIGCHLD+)
  (defconstant +SIGCONT+    SI::+SIGCONT+)
  (defconstant +SIGSTOP+    SI::+SIGSTOP+)
  (defconstant +SIGTSTP+    SI::+SIGTSTP+)
  (defconstant +SIGTTIN+    SI::+SIGTTIN+)
  (defconstant +SIGTTOU+    SI::+SIGTTOU+)
  (defconstant +SIGURG+     SI::+SIGURG+)
  (defconstant +SIGXCPU+    SI::+SIGXCPU+)
  (defconstant +SIGXFSZ+    SI::+SIGXFSZ+)
  (defconstant +SIGVTALRM+  SI::+SIGVTALRM+)
  (defconstant +SIGPROF+    SI::+SIGPROF+)
  (defconstant +SIGWINCH+   SI::+SIGWINCH+)
  (defconstant +SIGIO+      SI::+SIGIO+)
  (defconstant +SIGSYS+     SI::+SIGSYS+))


#-(or (and clisp linux) (and unix ecl))
(progn
 (defconstant +SIGHUP+                  1  "Hangup (POSIX).")
 (defconstant +SIGINT+                  2  "Interrupt (ANSI).")
 (defconstant +SIGQUIT+                 3  "Quit (POSIX).")
 (defconstant +SIGILL+                  4  "Illegal instruction (ANSI).")
 (defconstant +SIGTRAP+                 5  "Trace trap (POSIX).")
 (defconstant +SIGABRT+                 6  "Abort (ANSI).")
 (defconstant +SIGIOT+                  6  "IOT trap (4.2 BSD).")
 (defconstant +SIGBUS+                  7  "BUS error (4.2 BSD).")
 (defconstant +SIGFPE+                  8  "Floating-point exception (ANSI).")
 (defconstant +SIGKILL+                 9  "Kill, unblockable (POSIX).")
 (defconstant +SIGUSR1+                10  "User-defined signal 1 (POSIX).")
 (defconstant +SIGSEGV+                11  "Segmentation violation (ANSI).")
 (defconstant +SIGUSR2+                12  "User-defined signal 2 (POSIX).")
 (defconstant +SIGPIPE+                13  "Broken pipe (POSIX).")
 (defconstant +SIGALRM+                14  "Alarm clock (POSIX).")
 (defconstant +SIGTERM+                15  "Termination (ANSI).")
 (defconstant +SIGSTKFLT+              16  "Stack fault.")
 (defconstant +SIGCHLD+                17  "Child status has changed (POSIX).")
 (defconstant +SIGCLD+          +SIGCHLD+  "Same as SIGCHLD (System V).")
 (defconstant +SIGCONT+                18  "Continue (POSIX).")
 (defconstant +SIGSTOP+                19  "Stop, unblockable (POSIX).")
 (defconstant +SIGTSTP+                20  "Keyboard stop (POSIX).")
 (defconstant +SIGTTIN+                21  "Background read from tty (POSIX).")
 (defconstant +SIGTTOU+                22  "Background write to tty (POSIX).")
 (defconstant +SIGURG+                 23  "Urgent condition on socket (4.2 BSD).")
 (defconstant +SIGXCPU+                24  "CPU limit exceeded (4.2 BSD).")
 (defconstant +SIGXFSZ+                25  "File size limit exceeded (4.2 BSD).")
 (defconstant +SIGVTALRM+              26  "Virtual alarm clock (4.2 BSD).")
 (defconstant +SIGPROF+                27  "Profiling alarm clock (4.2 BSD).")
 (defconstant +SIGWINCH+               28  "Window size change (4.3 BSD, Sun).")
 (defconstant +SIGIO+                  29  "I/O now possible (4.2 BSD).")
 (defconstant +SIGPOLL+           +SIGIO+  "Pollable event occurred (System V).")
 (defconstant +SIGPWR+                 30  "Power failure restart (System V).")
 (defconstant +SIGSYS+                 31  "Bad system call.")
 (defconstant +SIGUNUSED+              31))



(define-condition user-interrupt (condition)
  ((signal :initarg :signal
           :initform 0
           :reader user-interrupt-signal))
  (:report (lambda (condition stream)
             (format stream "~S signal ~D"
                     'user-interrupt
                     (user-interrupt-signal condition)))))


(defun yield-signals (signals)
  #+(and ccl (not windows-target))
  (dolist (signum signals)
    (when (ccl:wait-for-signal signum 0)
      (signal 'user-interrupt :signal signum))))




;;;---------------------------------------------------------------------
;;; Find the libc library.

#+(and clisp unix (not linux))
(defun split-string (string &optional (separators " "))
  "
NOTE:   current implementation only accepts as separators
        a string containing literal characters.
"
  (let ((string     (if (simple-string-p string)
                        string
                        (copy-seq string)))
        (separators (if (simple-string-p separators)
                        separators
                        (copy-seq separators)))
        (chunks  '())
        (position 0)
        (nextpos  0)
        (strlen   (length string)) )
    (declare (type simple-string string separators))
    (loop :while (< position strlen)
      :do (loop :while (and (< nextpos strlen)
                            (not (position (char string nextpos) separators)))
            :do (setq nextpos (1+ nextpos)))
      (push (subseq string position nextpos) chunks)
      (setq position (1+ nextpos))
      (setq nextpos  position))
    (nreverse chunks)))


#+(and clisp unix (not linux))
(defun library-path ()
  (append '("/lib" "/usr/lib" "/lib64" "/usr/lib64")
          (split-string (ext:getenv "LD_LIBRARY_PATH") ":")))


#+(and clisp unix (not linux))
(defun find-library (name)
  (flet ((make-library-path (path) (concatenate 'string path "/" name ".so*")))
    (dolist (dir (library-path) nil)
      (let ((path (make-library-path dir)))
        (dolist (lib (directory path))
          (when (handler-case (FFI:OPEN-FOREIGN-LIBRARY (namestring lib))
                  (error (err) (princ err) (terpri)))
            (return-from find-library (namestring lib))))))))


#+(and clisp unix (not linux))
(defvar *libc* (load-time-value (find-library "libc")))


;;;---------------------------------------------------------------------
;;; FFI to the signal primitives we need

#+(and clisp unix (not linux))
(ffi:def-c-type sighandler (FFI:C-FUNCTION (:ARGUMENTS (signum ffi:int :in))
                                           (:return-type ffi:int)
                                           (:LANGUAGE :stdc)))

#+(and clisp unix (not linux))
(ffi:def-call-out unix-signal (:name "signal")
  (:arguments (signum ffi:int :in)
              (handler sighandler :in))
  (:return-type sighandler)
  (:language :stdc)
  (:library *libc*))


;; (unix-signal sigchld (lambda (signum) (declare (ignore signum)) 0))





;;;---------------------------------------------------------------------
;;; SIGCHLD signal handling
;;;---------------------------------------------------------------------

(defun install-signal-handler (signum handler)
  "RETURN: signum; the old handler; sigset"
  #-(and clisp unix)
  (declare (ignore signum handler))
  #+(and clisp unix (not linux))
  (values signum (unix-signal signum handler) 0)
  #+(and clisp unix linux)
  (let ((oldhan (linux:|set-signal-handler| signum handler))
        (sigset (second (multiple-value-list
                         (linux:|sigaddset| (second (multiple-value-list
                                                     (linux:|sigemptyset|)))
                                signum)))))
    (linux:|sigprocmask-set-n-save| linux:|SIG_UNBLOCK| sigset)
    (values signum oldhan sigset)))


(defun restore-signal-handler (signum oldhan sigset)
  #-(and clisp unix)
  (declare (ignore signum oldhan sigset))
  #+(and clisp unix (not linux))
  (declare (ignore sigset))
  #+(and clisp unix (not linux))
  (unix-signal signum oldhan)
  #+(and clisp unix linux)
  (progn
    (linux:|set-signal-handler| signum oldhan)
    (linux:|sigprocmask-set-n-save| linux:|SIG_UNBLOCK| sigset)))



(defmacro with-signal-handler (signum handler &body body)
  #-(and clisp #.(cl:if (cl:find-package "LINUX") '(:and) '(:or)))
  (declare (ignore signum handler))
  #+(and clisp #.(cl:if (cl:find-package "LINUX") '(:and) '(:or)))
  (let ((voldhan (gensym))
        (vsignum (gensym))
        (vsigset (gensym)))
    `(let* ((,vsignum ,signum)
            (,voldhan (linux:|set-signal-handler| ,vsignum ,handler))
            (,vsigset (second (multiple-value-list
                               (linux:|sigaddset| 
                                      (second (multiple-value-list
                                               (linux:|sigemptyset|)))
                                      ,vsignum)))))
       (linux:|sigprocmask-set-n-save| linux:|SIG_UNBLOCK| ,vsigset)
       (unwind-protect (progn ,@body)
         (linux:|set-signal-handler| ,vsignum ,voldhan)
         (linux:|sigprocmask-set-n-save| linux:|SIG_UNBLOCK| ,vsigset))))
  #-(and clisp #.(cl:if (cl:find-package "LINUX") '(:and) '(:or)))
  `(progn
     ,@body))


(defmacro with-sigchld-handler (&body body)
  #+(and clisp #.(cl:if (cl:find-package "LINUX") '(:and) '(:or)))
  `(with-signal-handler linux:|SIGCHLD| 
                        (lambda (signum) 
                          (declare (ignore signum))
                          (values))
                        ,@body)
  #-(and clisp #.(cl:if (cl:find-package "LINUX") '(:and) '(:or)))
  `(progn
     ,@body))


(defun install-sigchld-signal-handler ()
  #+(and clisp #.(cl:if (cl:find-package "LINUX") '(:and) '(:or)))
  (install-signal-handler linux:|SIGCHLD|
                          (lambda (signum)
                            (declare (ignore signum))
                            (values))))


(install-sigchld-signal-handler)



(defmacro signal-handler-bind (bindings &body body)
  "
BINDINGS: is a list of lists of a signal number (evaluated, so you can
          use a constant), and a function taking a signal number
          argument.

Uses HANDLER-BIND to catch the condition signaling that a unix signal
was received, and dispatch to the right handler.  If no handler is selected,
or if the selected handler returns, the CONTINUE restart is invoked.

NOTE:     Use CATCHING-SIGNALS to enable the reception of the wanted
          unix signals.
"
  `(handler-bind
       (#+ecl
        (ext::unix-signal-received
         (lambda (condi)
           (let ((received-signal (SI:UNIX-SIGNAL-RECEIVED-CODE condi)))
             (cond
               ,@(mapcar (lambda (binding)
                           (destructuring-bind (signum handler) binding
                             `((= received-signal ,signum)
                               (funcall ,handler ,signum))))
                  bindings))
             (invoke-restart 'continue)))))
     #-ecl  (cerror "Continue ignoring signals"
                "~S is not implemented for ~A" 'signal-handler-bind (lisp-implementation-type))
     ,@body))



(defun call-catching-signals (signums thunk)
  "
Installs signals handlers to catch unix and signal a lisp condition
for them, calls THUNK, and then revert to the default signal handlers,
for the given list of SIGNUMS.

SEE ALSO:   CATCHING-SIGNALS
"
  #-ecl (cerror "Continue ignoring signals"
                "~S is not implemented for ~A" 'call-catching-signals (lisp-implementation-type))
  (unwind-protect
       (progn
         #+ecl (dolist (sig signums)
                 #-(and) (ext:catch-signal sig t)
                 #+(and) (assert (ext:catch-signal sig t) (sig) "It appears the signal ~D is not known." sig))
         (funcall thunk))
    (progn
      #+ecl (dolist (sig signums) (ext:catch-signal sig nil)))))



(defmacro catching-signals (signums &body body)
  "
Executes BODY catching the signals listed in SIGNUMS.

SIGNUMS: a list of signal numbers (evaluated).

EXAMPLE:  (block there
             (signal-handler-bind ((+sigpipe+ (lambda (signum) (incf broken-pipes)))
                                   (+sigint+  (lambda (signum) (return-from there))))
                (catching-signals (list +sigpipe+ +sigint+)
                   (do-something))))

"
  `(call-catching-signals ,signums (lambda () ,@body)))


;;;; THE END ;;;;
