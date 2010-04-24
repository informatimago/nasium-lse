
(Define coroutine ' (#t. #t))

;;Reopening  the continuation

(define (resume)
    (begin
     (set-cdr!   Coroutine #t)
     ((car coroutine) 'dummy)
     (newline)))

;; With  condition as a #f, global escaping      

(define (suspend  escape)
    (set-car!  Coroutine (begin
                          (set-cdr!  Coroutine #f)
                          (call/cc  (lambda (X) X))))
  (if (cdr coroutine)
      #f
      (escape #f)))


(define (F  dummy)
    (call/cc
     (lambda (escape)
       (letrec ((iter (lambda (I)
                        (begin
                         (display I)
                         (suspend escape)
                         (iter (+ I 1))))))
               (iter 0)))))

(set-car! Coroutine F)

(resume) (resume) (resume)

;; ... the resume please try executing many degrees
