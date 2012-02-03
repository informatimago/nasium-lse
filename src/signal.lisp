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
;;;;    2012-02-01 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2012
;;;;    
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU General Public License
;;;;    as published by the Free Software Foundation; either version
;;;;    2 of the License, or (at your option) any later version.
;;;;    
;;;;    This program is distributed in the hope that it will be
;;;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;;;    PURPOSE.  See the GNU General Public License for more details.
;;;;    
;;;;    You should have received a copy of the GNU General Public
;;;;    License along with this program; if not, write to the Free
;;;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;;    Boston, MA 02111-1307 USA
;;;;**************************************************************************

(defpackage "COM.INFORMATIMAGO.SIGNAL"
  (:use "COMMON-LISP")
  (:export
   "SIGNAL-HANDLER-BIND" "CATCHING-SIGNALS"
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


#+ecl
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

#-ecl
(progn
  (defconstant +SIGHUP+     1)
  (defconstant +SIGINT+     2)
  (defconstant +SIGQUIT+    3)
  (defconstant +SIGILL+     4)
  (defconstant +SIGTRAP+    5)
  (defconstant +SIGABRT+    6)
  (defconstant +SIGBUS+     7)
  (defconstant +SIGFPE+     8)
  (defconstant +SIGKILL+    9)
  (defconstant +SIGUSR1+   10)
  (defconstant +SIGSEGV+   11)
  (defconstant +SIGUSR2+   12)
  (defconstant +SIGPIPE+   13)
  (defconstant +SIGALRM+   14)
  (defconstant +SIGTERM+   15)
  (defconstant +SIGCHLD+   17)
  (defconstant +SIGCONT+   18)
  (defconstant +SIGSTOP+   19)
  (defconstant +SIGTSTP+   20)
  (defconstant +SIGTTIN+   21)
  (defconstant +SIGTTOU+   22)
  (defconstant +SIGURG+    23)
  (defconstant +SIGXCPU+   24)
  (defconstant +SIGXFSZ+   25)
  (defconstant +SIGVTALRM+ 26)
  (defconstant +SIGPROF+   27)
  (defconstant +SIGWINCH+  28)
  (defconstant +SIGIO+     29)
  (defconstant +SIGSYS+    31))


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
