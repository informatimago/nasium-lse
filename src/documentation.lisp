;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               documentation.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    This file defines documentation tools.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-08 <PJB> Extracted from commands.lisp
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

(in-package "COM.INFORMATIMAGO.LSE")


(defstruct chapter
  title
  category
  oneliner
  text)

(defmethod print-object ((self chapter) stream)
  (print-unreadable-object (self stream :identity t :type t)
    (format stream ":title ~S :category ~S"
            (chapter-title self) (chapter-category self)))
  self)

(defparameter *chapters* (make-hash-table :test (function equalp)))


(defun add-chapter (title chapter)
  (let* ((old-list (gethash title *chapters*))
         (old-chap (find (chapter-category chapter) old-list
                         :test (function string-equal)
                         :key (function chapter-category))))
    (if old-chap
        (setf (gethash title *chapters*)
              (nsubstitute chapter old-chap old-list))
        (push chapter (gethash title *chapters*))))
  chapter)


(defun delete-chapter (title category)
  (setf (gethash title *chapters*)
        (delete category (gethash title *chapters*)
              :test (function string-equal)
              :key (function chapter-category))))


(defun find-chapter (title &optional (category nil categoryp))
  (let ((chap-list (gethash title *chapters*)))
    (cond
      (categoryp
       (find category chap-list
             :test (function string-equal)
             :key (function chapter-category)))
      ((rest chap-list)
       chap-list)
      (t
       (first chap-list)))))


(defun find-category (category)
  (let ((result '()))
   (maphash (lambda (title chap-list)
              (declare (ignore title))
              (dolist (chapter chap-list)
                (when (string-equal category (chapter-category chapter))
                  (pushnew chapter result))))
            *chapters*)
   (sort result (function string-lessp) :key (function chapter-title))))



(defmacro defchapter (title-category-oneliner &body text)
  (let ((title    (if (listp title-category-oneliner)
                      (first title-category-oneliner)
                      title-category-oneliner))
        (category (if (listp title-category-oneliner)
                      (second title-category-oneliner)
                      nil))
        (oneliner (if (listp title-category-oneliner)
                      (third title-category-oneliner)
                      nil)))
    `(add-chapter ',title
                  (make-chapter :title ',title
                                :category ',category
                                :oneliner ',oneliner
                                :text ,(if (every 'stringp text)
                                           (unsplit-string text #\Newline)
                                           `(lambda (chapter)
                                              (declare (ignorable chapter))
                                              (block chapter ,@text)))))))


(defmacro definstruction (title-category-oneliner &body text)
  (let ((oneliner (if (listp title-category-oneliner)
                      (third title-category-oneliner)
                      nil)))
    `(defchapter ,title-category-oneliner
         , (with-output-to-string (out)
             (when oneliner (format out "~A~%" oneliner))
             (dolist (item text)
               (write-documentation out item))))))





;;;---------------------------------------------------------------------

(defparameter *character-foldings*
  '(("e" "èéêë") ("a" "àáâãäå") ("i" "ìíîï")
    ("u" "ùúûü") ("o" "òóôõöø") ("y" "ýÿ")
    ("E" "ÈÉÊË") ("A" "ÀÁÂÃÄÅ") ("I" "ÌÍÎÏ")
    ("U" "ÙÚÛÜ") ("O" "ÒÓÔÕÖØ") ("Y" "Ý")
    ("ae" "æ") ("c" "ç") ("eth" "ð") ("n" "ñ") ("th" "þ")
    ("AE" "Æ") ("C" "Ç") ("ETH" "Ð") ("N" "Ñ") ("TH" "Þ")
    ("ss" "ß")))

(defparameter *accented-letters*
  (with-output-to-string (out)
    (dolist (cf *character-foldings*)
      (princ (second cf) out))))

(defun accented-letter-p (ch)
  (find ch *accented-letters*))

(defun character-folding (character)
  (car (member (character character) *character-foldings* 
               :test (function position) :key (function second))))

(defun character-fold (character)
  "
RETURN: A string containing the character without accent 
        (for accented characters), or a pure ASCII form of the character.
"
  (car (character-folding character)))

(defun remove-accents (string)
  (if (find-if (function accented-letter-p) string)
      (with-output-to-string (out)
        (loop
          :for ch :across string
          :do (let ((conv (character-folding ch)))
                (princ (or (car conv) ch) out))))
      string))



(defparameter *key-labels*
 '(("[ESC]"     . :escape)
   ("[CTRL-A]"  . :attention)
   ("[XOFF]"    . :xoff)
   ("[DEL]"     . :delete)
   ("[RETURN]"  . :return)))


(defun process-doc (docstring)
  (flet ((keys (docstring translate)
           (if (find #\[ docstring)
               (with-output-to-string (out)
                 (loop
                   :with end = (length docstring)
                   :with start = 0
                   :with sstart = 0
                   :while (< sstart end)
                   :do (let* ((lcro (position #\[ docstring :start sstart))
                              (rcro (and lcro
                                         (position #\] docstring :start (1+ lcro))))
                              (key  (and rcro
                                         (assoc (subseq docstring lcro (1+ rcro))
                                                *key-labels* :test (function string=)))))
                         (if key
                             (progn
                               (princ (subseq docstring start lcro)     out)
                               (princ (funcall translate (cdr key)) out)
                               (setf sstart (setf start (1+ rcro))))
                             (setf sstart (if lcro (1+ lcro) end))))
                   :finally (princ (subseq docstring start end) out)))
               docstring)))
    (if (and (boundp '*task*) *task*)
        (let ((terminal (task-terminal *task*)))
          (funcall (if (task-upcase-output *task*)
                       (function string-upcase)
                       (function identity))
                   (funcall (if (task-accented-output *task*)
                                (function identity)
                                (function remove-accents))
                            (keys docstring (lambda (key) (terminal-key terminal key))))))
        (keys docstring (function identity)))))


(defun split-text (text)
  (with-input-from-string (inp text)
    (loop
      :with text = '()
      :with para = '()
      :for line = (read-line inp nil nil)
      :while line
      :do (cond
            ((zerop (length (string-trim " " line)))
             (when para (push (nreverse para) text))
             (setf para '()))
            ((digit-char-p (aref line 0))
             (when para (push (nreverse para) text))
             (push (list line) text)
             (setf para '()))
            ((char= #\| (aref line 0))
             (when para (push (nreverse para) text))
             (push (list line) text)
             (setf para '()))
            (t
             (setf para (nreconc (split-sequence #\space line :REMOVE-EMPTY-SUBSEQS t)
                                 para))))
      :finally (when para (push (nreverse para) text)) (return (nreverse text)))))


(defun justify-text (text first-left-margin left-margin right-margin)
  (with-output-to-string (out)
    (loop
      :with margin1 = (make-string left-margin       :initial-element #\space)
      :with margin2 = (make-string (+ 2 left-margin) :initial-element #\space)
      :with para-margin = "" 
      :for column = first-left-margin :then left-margin
      :for para :in text
      :do (progn
            (princ para-margin out)
            (setf para-margin (if (string= "-" (first para))
                                  margin2
                                  margin1))
            (dolist (word para)
              (let* ((spaces (if (find (aref word (1- (length word))) ".!?")
                                 "  " " ")))
                (when (< right-margin (+ column (length word)))
                  (terpri out)
                  (princ para-margin out)
                  (setf column (length para-margin)))
                (princ word out)
                (when (<= (+ column (length word) (length spaces)) right-margin)
                  (princ spaces out))
                (incf column (+ (length word) (length spaces)))))
            (terpri out)
            (terpri out)))))


(defun write-documentation (target text)
  (funcall (typecase target
             (stream    (function format))
             (otherwise (function io-format)))
           target "~A~%"
           (justify-text (split-text (process-doc text))
                         0 0
                         (typecase target
                           (stream  80)
                           (otherwise (terminal-columns (task-terminal target)))))))



(defun first-line (text)
  (subseq text 0 (position #\Newline text)))


;;;; THE END ;;;;
