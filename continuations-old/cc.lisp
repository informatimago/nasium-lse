;; ;;You display the coroutine with the pair.  The car to continue, the
;; ;;cdr condition
;; 
;; (Define coroutine ' (#t. #t))
;; 
;; ;;Reopening  the continuation
;; 
;; (define (resume)
;;     (begin
;;      (set-cdr!   Coroutine #t)
;;      ((car coroutine) 'dummy)
;;      (newline)))
;; 
;; ;; With  condition as a #f, global escaping      
;; 
;; (define (suspend  escape)
;;     (set-car!  Coroutine (begin
;;                           (set-cdr!  Coroutine #f)
;;                           (call/cc  (lambda (X) X))))
;;   (if (cdr coroutine)
;;       #f
;;       (escape #f)))
;; 
;; 
;; (define (F  dummy)
;;     (call/cc
;;      (lambda (escape)
;;        (letrec ((iter (lambda (I)
;;                         (begin
;;                          (display I)
;;                          (suspend escape)
;;                          (iter (+ I 1))))))
;;                (iter 0)))))
;; 
;; (set-car! Coroutine F)
;; 
;; (resume) (resume) (resume)
;; 
;; ;; ... the resume please try executing many degrees


;;You display the coroutine with the pair.  The car to continue, the
;;cdr condition

;;----------------------------------------------------------------------


#||


||#

;; (defun f ()
;;   (let ((x 1))
;;     (loop
;;        (print `(f ,x))
;;        (if (= 10 x)
;;            (return-from f)
;;            (suspend))
;;        (incf x))))
;; 
;; (defun g (n)
;;   (let ((y 0))
;;     (loop
;;        (print `(g ,y))
;;        (setf y (mod (1+ y) n))
;;        (suspend)
;;        (suspend))))
;; 
;; (coroutines (f) (g 5) (g 7))

(asdf:operate 'asdf:load-op :com.informatimago.common-lisp)
(load "coroutine.lisp")
(use-package "COM.INFORMATIMAGO.COROUTINE")

(defun f ()
  (let ((x 1))
    (loop
       (print `(f ,x))
       (if (= 10 x)
           (return-from f)
           (suspend))
       (incf x))))

(defun g (n)
  (let ((y 0))
    (loop
       (print `(g ,y))
       (setf y (mod (1+ y) n))
       (suspend)
       (suspend))))

(process-run-function "f" (function f))
(process-run-function "g" (function g) 5)
(start-scheduler)


