(use-package :it.bese.arnesi)

(defstruct coroutine
  routine
  active)

(defparameter *coroutine* (make-coroutine))

(defun/cc resume ()
  (setf (coroutine-active *Coroutine*) t)
  (call/cc (coroutine-routine *coroutine*)))

(defun/cc suspend ()
  (setf (coroutine-routine *Coroutine*)
        (progn (setf (coroutine-active *Coroutine*) nil)
               (call/cc (function identity))))
  ;; here, we pass first with active=nil, and then with active=t
  (print `(active = ,(coroutine-active *coroutine*)))
  (unless (coroutine-active *coroutine*)
    (it.bese.arnesi::toplevel-k nil)))

(defun routine ()
  (let ((i 0))
    (loop
       (print (incf i))
       (suspend))))

(defun f (dummy)
  (declare(ignore dummy))
  (routine))

(setf *coroutine* (make-coroutine :routine (function f)
                                  :active t))

