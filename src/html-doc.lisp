;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               html-doc.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Generate HTML L.S.E. documentation from the in-line documentation.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2012-03-14 <PJB> Created
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2012 - 2014
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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf *readtable* (copy-readtable nil)))
(in-package "COM.INFORMATIMAGO.LSE")

(defvar *html-doc-base-filename* "lse-"
  "Prefix to html file names (but the index.html).")


(defun html-doc-category-file-name (category)
  (format niL "~(~A~A.html~)"
          *html-doc-base-filename*
          (remove-accents category)))


(defvar *documentation-directory* #P"/tmp/doc/")




;;; Generate the grammar rules.

(defun noregexp (str)
  (flet ((noreg (str)
           (if (and (< 1 (length str))
                     (find-if (lambda (ch) (find ch "[]?+*$."))
                              str))
               nil
               str)))
   (if (prefixp "\\" str)
       (noreg (subseq str 1))
       (noreg str))))

(defun optpar (rhs formated-rhs)
  (if (and (listp rhs)
           (eql 'alt (first rhs))
           (rest (rest rhs)))
      (format nil "(~A)" formated-rhs)
      formated-rhs))


(defun format-rule (rule terminals &optional (ntwidth 20))
  (labels ((format-rhs (rhs)
             (if (atom rhs)
                 (cond
                   ((stringp rhs)
                    (format nil "'~A'"
                                          (if (find #\' rhs)
                                              (with-output-to-string (out)
                                                (loop :for ch :across rhs
                                                  :do (if (char= #\' ch)
                                                          (princ "''" out)
                                                          (princ ch out))))
                                              rhs)))
                   ((let ((entry (assoc rhs terminals)))
                      (when entry
                        (let ((str (noregexp (second entry))))
                          (if str
                              (format nil "'~A'" str)
                              (subseq (symbol-name rhs) 4))))))
                   (t
                    (format nil "~A" rhs)))
                 (ecase (first rhs)
                   ((seq)
                    (format nil "~{~A~^ ~}"
                            (mapcar (lambda (item) (optpar item (format-rhs item)))
                                    (rest rhs))))
                   ((rep)
                    (format nil "{~{~A~^ ~}}"
                            (mapcar (lambda (item) (optpar item (format-rhs item)))
                                    (rest rhs))))
                   ((opt)
                    (format nil "[~{~A~^ ~}]"
                                (mapcar (lambda (item) (optpar item (format-rhs item)))
                                        (rest rhs))))
                   ((alt)
                    (format nil "~{~A~^ | ~}"
                            (mapcar (function format-rhs)
                                    (rest rhs))))))))
    (format nil "~VA ::= ~A." ntwidth (first rule) (format-rhs (second rule)))))


(defun generate-lse-grammar ()
  (with-output-to-string (*standard-output*)
    (let* ((lse (grammar-named 'lse))
           (terminals (grammar-terminals lse)))
      (format t "~%Symboles terminaux : ~2%")
      (dolist (terminal (remove-if (function noregexp) terminals :key (function second)))
        (format t "    ~:@(~VA~) = /~A/.~2%"
                20 (subseq (string (first terminal)) 4) (second terminal)))
      (format t "~%Règles de grammaire : ~2%")
      (dolist (rule (grammar-rules (grammar-named 'lse)))
        (princ "    ")
        (write-line (format-rule (clean-up-rule rule) terminals))
        (terpri)))))



;;; Categories:

(defun categories ()
  "Returns a list of documentation categories.
NOTE: Unclassified chapters are in the category NIL."
  (let ((categories '()))
    (maphash (lambda (title chap-list)
               (dolist (chapter chap-list)
                 (pushnew (chapter-category chapter) categories
                          :test (function equalp))))
             *chapters*)
    categories))


(defun chapters-per-category ()
 "Returns an a-list mapping categories to a sorted list of chapter titles."
 (mapcar (lambda (category)
           (cons category
                 (sort (map 'list 'chapter-title (find-category category))
                       (function string-lessp))))
         (categories)))


;; (let ((results '()))
;;  (maphash (lambda (title chapters)
;;             (dolist (chapter chapters)
;;               (push
;;                (if (functionp (chapter-text chapter))
;;                    `(:functional ,(chapter-title chapter) ,(chapter-category chapter))
;;                    `(:textual ,(chapter-title chapter) ,(chapter-category chapter)))
;;                results)))
;;           *chapters*)
;;  (sort results (function string<) :key (function first)))




(defparameter *html-key-labels*
  '(("ESC"         . "[INTERRUPTION]")
    ("ÉCHAPEMENT"  . "[INTERRUPTION]")
    ("CTRL-A"      . "[ATTENTION]")
    ("XOFF"        . "[X-OFF]")
    ("DEL"         . "[EFFACEMENT]")
    ("RETURN"      . "[ENTRÉE]")))

(defun substitutions (word)
  (let ((under (position #\_ word))
        (caret (position #\^ word)))
    (if (or under caret)
        (with-output-to-string (out)
          (loop
            :with len = (length word)
            :with i = 0
            :while (< i len)
            ;; We escape only ^ and _ with \.  Other occurences of \ are literal.
            :do (let ((ch (aref word i)))
                  (case ch
                    ((#\\) (if (< (1+ i) len)
                               (let ((nextch (aref word (1+ i))))
                                 (case nextch
                                   ((#\^ #\_) (princ nextch out) (incf i))
                                   (otherwise (princ ch out))))
                               (princ ch out)))
                    ((#\^)     (princ *unicode-upwards-arrow*   out))
                    ((#\_)     (princ *unicode-leftwards-arrow* out))
                    (otherwise (princ ch out)))
                  (incf i))))
        word)))


(defun generate-html-word (word)
  (cond
    ((or (prefixp "http://" word)
         (prefixp "https://" word)
         (prefixp "mailto:" word)
         (prefixp "news:" word)
         (prefixp "irc://" word))
     (html:a (:href word) (html:pcdata "~A" word)))
    ((prefixp "[" word)
     (let ((rcro (position #\] word)))
       (if rcro
           (let* ((key (subseq word 1 rcro))
                  (aft (subseq word (1+ rcro)))
                  (entry (assoc key *html-key-labels*
                                :test (function string=))))
             (if entry
                 (html:kbd - (html:pcdata "~A" (cdr entry)))
                 (html:kbd - (html:pcdata "[~A]" key)))
             (html:pcdata "~A" aft))
           (html:pcdata "~A" (substitutions word)))))
    (t
     (html:pcdata "~A" (substitutions word))))
  (html:pcdata " "))



(defun generate-html-text (text)
  (loop
    :with newline = (string #\Newline)
    :with paras = (split-text text)
    :while paras
    :do (let ((para (pop paras)))
          (cond
            ((null para))
            ((null (first para)))
            ((zerop (length (first para))))

            ;; Lists.
            ((char= #\- (aref (first para) 0))
             ;; (push (cons (subseq (first para) 1) (rest para)) paras)
             (push para paras)
             (html:ul -
               (loop
                 :for para = (first paras)
                 :while (and (first para)
                             (plusp (length (first para)))
                             (char= #\- (aref (first para) 0)))
                 :do (pop paras)
                 :do (html:li - (dolist (word (rest para))
                                  (generate-html-word word))))))

            ;; Example programs.
            ((or (digit-char-p (aref (first para) 0))
                 (char= #\?    (aref (first para) 0))
                 (char= #\|    (aref (first para) 0)))
             (push para paras)
             (html:pre -
               (loop
                 :for para = (first paras)
                 :while (and (first para) (or (digit-char-p (aref (first para) 0))
                                              (char= #\?    (aref (first para) 0))
                                              (char= #\|    (aref (first para) 0))))
                 :do (pop paras)
                 :do (dolist (word para (html:pcdata newline))
                       (if (char= #\| (aref word 0))
                           (generate-html-word (subseq word 1))
                           (generate-html-word word))))))
            
            ;; References.
            ((prefixp "Voir" (first para))
             (let ((sep nil))
              (dolist (item (split-sequence #\, (format nil "~{~A~^ ~}" para)))
                (let* ((dot (char= #\. (aref item (1- (length item)))))
                       (item (if dot
                                 (subseq item 0 (1- (length item)))
                                 item))
                       (pos (position-if 'lower-case-p item :from-end t))
                       (pos (if pos
                                (position-if 'upper-case-p item :start pos)
                                (position-if 'upper-case-p item)))
                       (pre (if pos (subseq item 0 pos) item))
                       (ref (if pos (subseq item pos) nil)))
                  (when sep (html:pcdata sep))
                  (setf sep ", ")
                  (if (and ref (every (lambda (ch) (or (find ch " '") (upper-case-p ch))) ref))
                      (progn
                        (when (plusp (length pre)) (html:pcdata "~A" pre))
                        (if (= 2 (length ref))
                            (let ((command (find-chapter ref "COMMANDES")))
                              (if command
                                  (html:code -
                                   (html:a (:href (format nil "~A#~A"
                                                          (html-doc-category-file-name "COMMANDES")
                                                          (chapter-title command)))
                                     (html:pcdata "~A" (chapter-title command))))
                               (html:pcdata "~A" ref)))
                            (html:code -
                             (html:a (:href (format nil "#~A" ref))
                               (html:pcdata "~A" ref)))))
                      (html:pcdata "~A" item))
                  (when dot (html:pcdata "."))))))

            ;; Normal paragraphs.
            (t
             (html:p -
               (dolist (word para)
                 (generate-html-word word))))))))


(defmethod generate-chapter ((chapter chapter) (category t))
  (html:a (:name (chapter-title chapter)))
  (html:h3 - (html:pcdata "~A" (chapter-title chapter)))
  (when (and (chapter-oneliner chapter)
             (string/= (chapter-oneliner chapter)
                       (chapter-text chapter)))
   (html:p - (html:pcdata "~A" (chapter-oneliner chapter))))
  (generate-html-text (chapter-text chapter)))


(defmethod generate-chapter ((chapter chapter) (category (eql :fonctions)))
  (html:a (:name (chapter-title chapter)))
  (html:h3 - (html:pcdata "~A~@[ - ~A~]"
                          (chapter-title chapter)
                          (substitutions (chapter-oneliner chapter))))
  (let* ((syntax (first-line (chapter-text chapter)))
         (text   (subseq  (chapter-text chapter) (length syntax))))
    (html:code - (html:pcdata (substitutions syntax)))
    (generate-html-text text)))


(defmethod generate-chapter ((chapter chapter) (category (eql :commandes)))
  (let* ((text (string-trim #(#\Space #\Newline) (chapter-text chapter)))
         (synopsis (first-line text))
         (text (string-left-trim #(#\Space #\Newline) (subseq text (length synopsis)))))
    (html:a (:name (chapter-title chapter)))
    ;; (html:h3 - (html:pcdata "~A" (chapter-title chapter)))
    (html:code -
      (html:b - (html:pcdata "~A" (subseq synopsis 0 2)))
      (html:pcdata "~A" (subseq synopsis 2)))
    (html:blockquote -
      (generate-html-text text))))


(defmethod generate-chapter ((chapter chapter) (category (eql :instructions)))
  (html:a (:name (chapter-title chapter)))
  (html:h3 - (html:pcdata "~A" (chapter-title chapter)))
  (let* ((syntax (first-line (chapter-text chapter)))
         (text   (subseq (chapter-text chapter) (length syntax))))
    (html:code - (generate-html-text syntax))
    (generate-html-text text)))


(defun generate-category (category prev up next)
  "Generate a category documentation html file."
  (let ((cat (intern category "KEYWORD")))
    (with-open-file (html:*html-output-stream*
                     (merge-pathnames (html-doc-category-file-name category) *documentation-directory* nil)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :external-format *external-format/utf-8*)
      (format *trace-output* "~&Generating ~A~%" (pathname html:*html-output-stream*))
      (force-output *trace-output*)
      (html:with-html-output (html:*html-output-stream*
                              :kind :html
                              :encoding :utf-8)
        (html:doctype :strict
          (html:html -
            (html:head -
              (html:title - (html:pcdata "NASIUM L.S.E. - ~:(~A~)" category))
              (html:link (:rel "shortcut icon" :href "http://www.ogamita.com/favicon.ico"))
              (html:link (:rel "icon"          :href "http://www.ogamita.com/favicon.ico" :type "image/vnd.microsoft.icon"))
              (html:link (:rel "icon"          :href "http://www.ogamita.com/favicon.png" :type "image/png"))
              (html:link (:rel "stylesheet"    :href "http://www.ogamita.com/default.css" :type "text/css"))
              (html:meta (:http-equiv "Content-Type"   :content "text/html; charset=utf-8"))
              (html:meta (:name "author"               :content "Pascal J. Bourguignon"))
              (html:meta (:name "Reply-To"             :content "pjb@informatimago.com"))
              (html:meta (:name "Keywords"             :content
                                (format nil "NASIUM, LSE, L.S.E, Langage Symbolique d'Enseignement, Langage de programmation, Français, ~(~A~)" category))))
            (html:body -
              (html:img (:src "../nasium-lse-2.png" :alt "NASIUM L.S.E."))
              ;; (html:h1 - (html:pcdata "NASIUM L.S.E."))
              (html:hr)
              (html:div (:class "navigation")
                (html:a (:href "../")   (html:pcdata "Maison"))
                (html:pcdata " | ")
                (when prev
                  (html:a (:href prev) (html:pcdata "Page précédente"))
                  (html:pcdata " | "))
                (html:a (:href up)   (html:pcdata "Index"))
                (when next
                  (html:pcdata " | ")
                  (html:a (:href next) (html:pcdata "Page suivante"))))
              (html:hr)
              (html:h2 - (html:pcdata "~:(~A~)" category))
              ;; introduction:
              (let ((intro (find-chapter "INTRODUCTION" category)))
                (when intro
                  (generate-html-text (chapter-text intro))))
              (let ((chapters (cdr (assoc category (chapters-per-category)
                                          :test (function string-equal)))))
                (when chapters
                  (when (rest chapters)
                    ;; table of contents:
                    (html:div (:class "toc")
                      (html:p - (html:pcdata "Table des ~(~A~) :" category))
                      (html:ul -
                        (dolist (chapter chapters)
                          (unless (string-equal "INTRODUCTION" chapter)
                            (html:li -
                              (html:a (:href (format nil "#~A" chapter))
                                (html:pcdata chapter))))))))
                  ;; sections:
                  (dolist (chapter chapters)
                    (unless (string-equal "INTRODUCTION" chapter)
                      (generate-chapter (find-chapter chapter category) cat))))
                (when (string= "GRAMMAIRE" category)
                  (html:pre (:class "grammar")
                    (html:pcdata (generate-lse-grammar))))))))))))



;;; Generate the documentation and index:

(defparameter *indexed-categories*
  '("GÉNÉRALITÉS" #+lse-unix "OPTIONS"
    "COMMANDES" "INSTRUCTIONS" "FONCTIONS" "GRAMMAIRE" "LÉGAL"))

(defun generate-index ()
  "Generate an index file for the catgory html files."
  (with-open-file (html:*html-output-stream*
                   (merge-pathnames "index.html" *documentation-directory* nil)
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create
                   :external-format *external-format/utf-8*)
    (format *trace-output* "~&Generating ~A~%" (pathname html:*html-output-stream*))
    (force-output *trace-output*)
    (html:with-html-output (html:*html-output-stream*
                            :kind :html
                            :encoding :utf-8)
      (html:doctype :strict
        (html:html -
          (html:head -
            (html:title - (html:pcdata "NASIUM L.S.E. - Langage Symbolique d'Enseignement"))
            (html:link (:rel "shortcut icon" :href "http://www.ogamita.com/favicon.ico"))
            (html:link (:rel "icon"          :href "http://www.ogamita.com/favicon.ico" :type "image/vnd.microsoft.icon"))
            (html:link (:rel "icon"          :href "http://www.ogamita.com/favicon.png" :type "image/png"))
            (html:link (:rel "stylesheet"    :href "http://www.ogamita.com/default.css" :type "text/css"))
            
            (html:meta (:http-equiv "Content-Type"   :content "text/html; charset=utf-8"))
            (html:meta (:name "author"               :content "Pascal J. Bourguignon"))
            (html:meta (:name "Reply-To"             :content "pjb@informatimago.com"))
            (html:meta (:name "Keywords"             :content "NASIUM, LSE, L.S.E, Langage Symbolique d'Enseignement, Langage de programmation, Français")))
          (html:body -
            (html:img (:src "../nasium-lse-2.png" :alt "NASIUM L.S.E."))
            ;; (html:h1 - (html:pcdata "NASIUM L.S.E."))
            (html:hr)
            (html:div (:class "navigation")
              (html:a (:href "../")   (html:pcdata "Maison")))
            (html:hr)
            (html:pre -
              (html:pcdata *title-banner* (version) *copyright*))
            (html:ol -
              (let ((chapters (chapters-per-category)))
                (dolist (category *indexed-categories*)
                  (html:li ()
                    (html:a (:href (html-doc-category-file-name category))
                      (html:pcdata "~A" category))))))))))))


(defun generate-html-documentation (*documentation-directory*)
  (let ((*documentation-directory* (etypecase *documentation-directory*
                                       (pathname *documentation-directory*)
                                       (stream   (pathname *documentation-directory*))
                                       (string   (pathname *documentation-directory*)))))
    (ensure-directories-exist (merge-pathnames "file.test" *documentation-directory* nil))
    (generate-index)
    (loop
      :for (prev category next) :on (append '(nil) *indexed-categories* '(nil))
      :while category
      :do (generate-category category
                             (when prev (html-doc-category-file-name prev))
                             "index.html"
                             (when next (html-doc-category-file-name next))))))



;;;; THE END ;;;;
