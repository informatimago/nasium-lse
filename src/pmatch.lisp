;;****************************************************************************
;;FILE:               pmatch.lisp
;;LANGUAGE:           Common-Lisp
;;SYSTEM:             Common-Lisp
;;USER-INTERFACE:     NONE
;;DESCRIPTION
;;    
;;    Sexp Pattern Matcher
;;    
;;AUTHORS
;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;MODIFICATIONS
;;    2003-12-17 <PJB> Created.
;;BUGS
;;    pattern matcher and instantiation won't work with arrays/matrices,
;;    structures...
;;BUGS
;;LEGAL
;;    GPL
;;    
;;    Copyright Pascal Bourguignon 2003 - 2004
;;    
;;    This program is free software; you can redistribute it and/or
;;    modify it under the terms of the GNU General Public License
;;    as published by the Free Software Foundation; either version
;;    2 of the License, or (at your option) any later version.
;;    
;;    This program is distributed in the hope that it will be
;;    useful, but WITHOUT ANY WARRANTY; without even the implied
;;    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;;    PURPOSE.  See the GNU General Public License for more details.
;;    
;;    You should have received a copy of the GNU General Public
;;    License along with this program; if not, write to the Free
;;    Software Foundation, Inc., 59 Temple Place, Suite 330,
;;    Boston, MA 02111-1307 USA
;;****************************************************************************

(DEFINE-PACKAGE "COM.INFORMATIMAGO.COMMON-LISP.PMATCH"
  (:DOCUMENTATION  "Sexp Pattern Matcher

    Copyright Pascal Bourguignon 2003 - 2004
    This package is provided under the GNU General Public License.
    See the source file for details.")
  (:FROM COMMON-LISP                                  :IMPORT :ALL)
;;   (:FROM COM.INFORMATIMAGO.COMMON-LISP.STRING         
;;    :IMPORT STRING-REPLACE SPLIT-STRING UNSPLIT-STRING)
;;   (:FROM COM.INFORMATIMAGO.COMMON-LISP.LIST           :IMPORT :ALL)
;;   (:FROM COM.INFORMATIMAGO.COMMON-LISP.UTILITY        :IMPORT :ALL)
  (:EXPORT
   "MATCH" "MATCH-STATE-FAILED-P" "MATCH-STATE-DICT" "MATCH-DICT-MAP"
   "??" "?/" "?V" "?C" "?X" "?+" "?*" ":"
   "COLLECT-VARIABLES" "MATCH-CASE"
   ));;COM.INFORMATIMAGO.COMMON-LISP.PMATCH


(defstruct match-state
  "PRIVATE" 
  (dict  '())
  (stack '())
  );;match-state


(defun match-dict-map (function ms)
  "
DO:     Calls FUNCTION (lambda (symbol value) ...) with all successive bindings,
        (unless matching state is failed).
RETURN: The list of results of the FUNCTION.
"
  (unless (match-state-failed-p ms)
      (mapcar (lambda (binding) (funcall function (first binding) (second binding)))
              (match-state-dict ms)))
  );;match-dict-map


(defun match-state-fail     (ms)
  "PRIVATE" 
  (push :failed (match-state-dict ms))
  ms);;match-state-fail


(defun match-state-retry    (ms)
  "PRIVATE" 
  (when (eq :failed (car (match-state-dict ms)))
    (pop (match-state-dict ms)))
  ms);;match-state-retry


(defun match-state-failed-p (ms)
  "
RETURN: Whether the match failed.
"
  (eq :failed (car (match-state-dict ms)))
  );;match-state-failed-p


(defun match-dict-remove-binding (ms pat)
  "PRIVATE" 
  (setf (match-state-dict ms)
        (delete (second (car pat)) (match-state-dict ms) :key (function car)))
;;   (do* ((var (second (car pat)))
;;         (previous nil current)
;;         (current (match-state-dict ms) (cdr current)))
;;       ((or (null current) (equal (caar current) var))
;;        (if previous
;;          (setf (cdr previous) (cdr current))
;;          (setf (match-state-dict ms) (cdr current)))))
  ms);;match-dict-remove-binding


(defun extend-dict (pat exp ms)
  "PRIVATE" 
  (let* ((var (second (car pat)))
         (val (car exp))
         (ass  (assoc var (match-state-dict ms))))
    (if ass
      ;; already there:
      (unless (equalp (second ass) val)
        (push (list var val) (match-state-dict ms))
        (match-state-fail ms))
      ;; a new binding:
      (push (list var val) (match-state-dict ms))))
  ms);;extend-dict


(defmacro defpattern (name symbol)
  `(defun ,name (pat)
     "PRIVATE" 
     (and (listp pat) (listp (car pat)) (string= ',symbol (caar pat))))
  );;defpattern


(defpattern arbitrary-variable-p          ?v)
(defpattern arbitrary-constant-p          ?c)
(defpattern arbitrary-expression-p        ?x)
(defpattern arbitrary-sequence-p          ?+)
(defpattern arbitrary-sequence-or-empty-p ?*)
(defpattern squeleton-eval-p              |:|)

(defpattern optional-p                    ??)
(defpattern alternative-p                 ?/)


(defun variable?   (exp)  "PRIVATE" (and (consp exp)  (symbolp   (car exp))))
(defun constant?   (exp)  "PRIVATE" (and (consp exp)  (constantp (car exp))))


(defun match (pat exp &optional (ms (make-match-state)))
  "
DO:        A pattern matcher accepting the following syntax:
             (?v v)  expects a symbol (variable).
             (?c c)  expects a constant (constantp).
             (?x x)  expects anything (one item).
             (?? l)  expects anything (zero or one item).
             (?+ l)  expects anything (one or more items).
             (?* l)  expects anything (zero or more items).
             other   expects exactly other (can be a sublist).
RETURN:    A match-state structure.
SEE ALSO:  match-state-failed-p to check if the matching failed.
           match-state-dict     to get the binding dictionary.
"
  ;; The pattern and the expression may be atoms or lists,
  ;; but usually we process (car pat) and (car exp), to be able
  ;; to match several items (?+ and ?*).
  (cond
   ((match-state-failed-p ms) ms)
   ((atom pat)
    (if (equal pat exp) ms (match-state-fail ms)))
   ((arbitrary-constant-p pat)
    (if (constant? exp)
      ;; TODO: Perhaps we should run extend-dict AFTER match!
      (match (cdr pat) (cdr exp) (extend-dict pat exp ms))
      (match-state-fail ms)))
   ((arbitrary-variable-p pat)
    (if (variable? exp)
      ;; TODO: Perhaps we should run extend-dict AFTER match!
      (match (cdr pat) (cdr exp) (extend-dict pat exp ms))
      (match-state-fail ms)))
   ((arbitrary-expression-p pat)
    (if (null exp)
      (match-state-fail ms)
      ;; TODO: Perhaps we should run extend-dict AFTER match!
      (match (cdr pat) (cdr exp) (extend-dict pat exp ms)) ))
   ((and (optional-p pat) (null exp))
    (match (cdr pat)  exp ms))
   ((and (arbitrary-sequence-p pat) (null exp))
    (match-state-fail ms))
   ((or (optional-p pat) 
        (arbitrary-sequence-p pat) (arbitrary-sequence-or-empty-p pat))
    ;; First get all we can or need.
    (push (if (optional-p pat)
            (let ((rest exp)
                  (list '())
                  (frame '()))
              (push (list list rest) frame)
              (push (car rest) list)
              (setf rest (cdr rest))
              (push (list list rest) frame)
              frame)
            (do ((rest  exp (cdr rest)) ;; what should match after
                 (list  '()) ;; what we match (reversed),
                 ;;             we reverse only in the final result [F].
                 (frame '()))
                ((null rest)
                 (progn
                   (push (list list rest) frame)
                   frame))
              (unless (and (null list) (arbitrary-sequence-p pat))
                ;; We skip the first push (of a nil expected) 
                ;; when we want one or more.
                (push (list list rest) frame))
              (push (car rest) list)))
          (match-state-stack ms))
    ;; then backtrack:
    (do* ((frame (pop (match-state-stack ms)) (cdr frame))
          (list  (first  (car frame))
                 (first  (car frame)))
          (rest  (second (car frame))
                 (second (car frame)))
          ;; We don't put the tentative list binding in the dictionary
          ;; to avoid being erased by match-dict-remove-binding in the
          ;; loop body.
          (soe   (match (cdr pat) rest ms)
                 (match (cdr pat) rest ms)))
        ((or (null frame)
             (when (not (match-state-failed-p ms))
               ;; Instead, we check for existance and push the
               ;; definitive list binding here in extend-dict. [F]
               (extend-dict pat (list (reverse list)) ms)
               (if (match-state-failed-p ms)
                 (prog1 nil (match-dict-remove-binding ms pat))
                 t)))
         ms)
      (match-state-retry ms)
      (match-dict-remove-binding ms pat) ))
   ((atom exp)
    (match-state-fail ms))
   (t ;; both cars are sublists.
    (match (cdr pat) (cdr exp)
           (match (car pat) (car exp) ms))))
  );;match


(defun evaluate (instance)
  "PRIVATE"
  (cond
   ((atom instance)               instance)
   ((and (atom (car instance)) (string= :|| (car instance)))
    (eval (evaluate (second instance))))
   (t (mapcar (function evaluate) instance)))
  );;evaluate


(defun dict-value (dict name)
  "PRIVATE"
  (second (assoc name dict :test (function string=)))
  );;dict-value


(defun dict-boundp (dict name)
  "PRIVATE"
  (and (or (symbolp name) (stringp name))
       (assoc name dict :test (function string=)))
  );;dict-boundp
  

(defun subst-bindings (expr dict)
  "PRIVATE"
  (cond
   ((atom expr) (list expr))
   ((and (atom (first expr)) (string= :||  (first expr)))
    (if (and (atom (second expr))
             (dict-boundp dict (second expr)))
      (list (dict-value dict (second expr)))
      (list (mapcan (lambda (subexpr) (subst-bindings subexpr dict)) expr))))
   ((and (atom (first expr)) (string= :|@| (first expr)))
    (copy-seq (dict-value dict (second expr))))
   (t (list (mapcan (lambda (subexpr) (subst-bindings subexpr dict)) expr))))
  );;subst-bindings


(defun instanciate (ms skeleton)
  "PRIVATE
PRE:   (not (match-state-failed-p ms))
DO:    Instanciate the skeleton, substituting all occurence of (: var)
       with the value bound to var in the binding dictionary of MS,
       Occurences of (:@ var) are split in line like ,@ in backquotes.
       Then all remaining (: form) are evaluated (with eval) from the
       deepest first.
"
  (assert (not (match-state-failed-p ms)))
  (evaluate (first (subst-bindings skeleton (match-state-dict ms))))
  );;instanciate


(defun collect-variables (pat)
  "
PAT:       A symbolic expression with the following syntax:
             (?v v)  expects a symbol (variable).
             (?c c)  expects a constant (constantp).
             (?x x)  expects anything (one item).
             (?+ l)  expects anything (one or more items).
             (?* l)  expects anything (zero or more items).
             other   expects exactly other (can be a sublist).
RETURN:    A list of the symbol used in the various (?. sym) items, 
           in no particular order, but with duplicates deleted.
"
  (delete-duplicates
   (cond
    ((atom pat)                   nil)
    ((member (car pat) '(?v ?c ?x ?+ ?*) :test (function eq)) (list (cadr pat)))
    (t (nconc (collect-variables (car pat)) (collect-variables (cdr pat))))))
  );;collect-variables


(defmacro match-case (sexp &rest clauses)
  "
SEXP:    A symbolic expression, evaluated.
CLAUSES: A list of (pattern &body body)
         The pattern must be litteral. 
         Lexical variable names are extracted from it, and body is executed
         in a lexical environment where these names are bound to the matched
         subexpression of SEXP.
DO:      Execute the body of the clause whose pattern matches the SEXP,
         or whose pattern is a symbol string-equal to OTHERWISE.
EXAMPLE: (match-case expr
            ((add       (?x a) to   (?x b)) `(+ ,a ,b))
            ((multiply  (?x a) with (?x b)) `(* ,a ,b))
            ((substract (?x a) from (?x a)) 0)
            (otherwise                      :error))
"
  (let ((ex (gensym)) (ms (gensym)) (dc (gensym)))
    `(let ((,ex ,sexp) (,ms) (,dc))
       (cond
        ,@(mapcar
           (lambda (clause)
             (let ((pat (car clause)) (body (cdr clause)))
               (if (and (symbolp pat) (string-equal "otherwise" pat))
                 `(t ,@body)
                 `((progn (setf ,ms (match ',pat ,ex))
                          (not (match-state-failed-p ,ms)))
                   (setf ,dc (match-state-dict ,ms))
                   (let ( ,@(mapcar
                             (lambda (name) `(,name (cadr (assoc ',name ,dc)))) 
                             (collect-variables pat)) )
                     ,@body))))) clauses)))));;match-case


;;;; pmatch.lisp                      --                     --          ;;;;
