;; Patch lse-domain.lisp, compile it and load it.
#+(or)
(let ((*package* *package*))
  (rename-file "lse-domain.lisp"  "lse-domain.src")
  (with-open-file (out "lse-domain.lisp"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (with-open-file (inp "lse-domain.src")
      (loop
         for sexp = (read inp nil inp)
         until (eq sexp inp)
         do (progn (when (and (listp sexp) (eq 'in-package (car sexp)))
                     (eval sexp))
                   ;; skip tok-litchaine from the LSE-DOMAIN
                   (unless (and (listp sexp)
                                (eq 'defun (first sexp))
                                (eq 'tok-litchaine (second sexp)))
                     (print sexp out)))))))
;; provide correct tok-litchaine:
#+(or)
(DEFUN TOK-LITCHAINE (STRING &OPTIONAL (START 0) (END (LENGTH STRING)))
  "\\('[^']*'\\)+"
  (SETF zebu::*REGEX-GROUPINGS* 1)
  (when (eql (char string start) #\')
    (incf start)
    (loop
       (cond ((<= end start)            ; String reaches eoln
              ;;(print `(:eoln ,end ,start --> nil))
              (return-from tok-litchaine nil)) 
             ((eql (char string start) #\')
              ;;(print `(:quote ,(char string start) ,start --> increment))
              (incf start)
              (cond ((<= end start)
                     ;;(print `(:ens  ,start --> return ,start))
                     (SETF (CADR (SVREF zebu::*REGEX-GROUPS* 0)) start)
                     (return-from tok-litchaine start))
                    ((eql (char string start) #\') ; go on
                     ;;(print `(:quote , (char string start) ,start --> increm))
                     (incf start))
                    (t                  ; eos
                     ;;(print `(:eos ,start --> return ,start))
                     (SETF (CADR (SVREF zebu::*REGEX-GROUPS* 0)) start)
                     (return-from tok-litchaine start))))
             (t
              ;;(print `(:increment ,start --> ,(1+ start)))
              (incf start))))))
