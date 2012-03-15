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

(defvar *html-doc-base-filename* "lse-"
  "Prefix to html file names (but the index.html).")


(defun html-doc-category-file-name (category)
  (format niL "~(~A~A.html~)"
          *html-doc-base-filename*
          (remove-accents category)))


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
            :do (let ((ch (aref word i)))
                  (if (and (char= #\\ ch)
                           (< (1+ i) len)
                           (or (char= #\_ (aref word (1+ i)))
                               (char= #\^ (aref word (1+ i)))))
                      (progn
                        (if (char= #\_ (aref word (1+ i)))
                            (princ *unicode-leftwards-arrow* out)
                            (princ *unicode-upwards-arrow*   out))
                        (incf i 2))
                      (progn
                        (princ ch out)
                        (incf i))))))
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
                               (html:a (:href (format nil "~A#~A"
                                                      (html-doc-category-file-name "COMMANDES")
                                                      (chapter-title command)))
                                 (html:pcdata "~A" (chapter-title command)))
                               (html:pcdata "~A" ref)))
                            (html:a (:href (format nil "#~A" ref))
                              (html:pcdata "~A" ref))))
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
  (html:a (:name (chapter-title chapter)))
  (html:h3 - (html:pcdata "~A" (chapter-title chapter)))
  (let* ((text (string-trim #(#\Space #\Newline) (chapter-text chapter)))
         (synopsis (first-line text))
         (text (string-left-trim #(#\Space #\Newline) (subseq text (length synopsis)))))
    (html:code -
      (html:b - (html:pcdata "~A" (subseq synopsis 0 2)))
      (html:pcdata "~A" (subseq synopsis 2)))
    (generate-html-text text)))


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
                     (html-doc-category-file-name category)
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create
                     :external-format *external-format/utf-8*)
      (html:with-html-output (html:*html-output-stream*
                              :kind :html
                              :encoding :utf-8)
        (html:doctype :strict
          (html:html -
            (html:head -
              (html:title - (html:pcdata "~:(~A~) - Système L.S.E. sur unix" category))
              (html:link (:rel "shortcut icon" :href "../../favicon.ico" :type "image/x-icon"))
              (html:link (:rel "icon"          :href "../../favicon.ico" :type "image/x-icon"))
              (html:link (:rel "stylesheet"    :href "../../default.css" :type "text/css"))
              (html:meta (:name "author"       :content "Pascal J. Bourguignon"))
              (html:meta (:name "Reply-To"     :content "pjb@informatimago.com"))
              (html:meta (:name "Keywords"     :content
                                (format nil "LSE, L.S.E, Langage Symbolique d'Enseignement, Langage de programmation, Français, ~(~A~)" category))))
            (html:body -
              (html:div (:class "navigation")
                (when prev
                  (html:a (:href prev) (html:pcdata "Page précédente"))
                  (html:pcdata "|"))
                (html:a (:href up)   (html:pcdata "Index"))
                (when next
                  (html:pcdata "|")
                  (html:a (:href next) (html:pcdata "Page suivante"))))
              (html:hr)
              (html:h1 - (html:pcdata "Système L.S.E. sur unix"))
              (html:h2 - (html:pcdata "~:(~A~)" category))
              ;; introduction:
              (let ((intro (find-chapter "INTRODUCTION" category)))
                (when intro
                  (generate-html-text (chapter-text intro))))
              ;; table of contents:
              (html:div (:class "toc")
                (html:p - (html:pcdata "Table des ~(~A~) :" category))
                (html:ul -
                  (dolist (chapter (cdr (assoc category (chapters-per-category)
                                               :test (function string-equal))))
                    (unless (string-equal "INTRODUCTION" chapter)
                      (html:li -
                        (html:a (:href (format nil "#~A" chapter))
                          (html:pcdata chapter)))))))
              ;; sections:
              (dolist (chapter (cdr (assoc category (chapters-per-category)
                                           :test (function string-equal))))
                (unless (string-equal "INTRODUCTION" chapter)
                  (generate-chapter (find-chapter chapter category) cat))))))))))


(defparameter *indexed-categories*
  '("GÉNÉRALITÉS" #+lse-unix "OPTIONS"
    "COMMANDES" "INSTRUCTIONS" "FONCTIONS" "LÉGAL"))

(defun generate-index ()
  "Generate an index file for the catgory html files."
  (with-open-file (html:*html-output-stream*
                   "index.html"
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create
                   :external-format *external-format/utf-8*)
    (html:with-html-output (html:*html-output-stream*
                            :kind :html
                            :encoding :utf-8)
      (html:doctype :strict
        (html:html -
          (html:head -
            (html:title - (html:pcdata "Documentation du système L.S.E. sur unix"))
            (html:link (:rel "shortcut icon" :href "../../favicon.ico" :type "image/x-icon"))
            (html:link (:rel "icon"          :href "../../favicon.ico" :type "image/x-icon"))
            (html:link (:rel "stylesheet"    :href "../../default.css" :type "text/css"))
            (html:meta (:name "author"       :content "Pascal J. Bourguignon"))
            (html:meta (:name "Reply-To"     :content "pjb@informatimago.com"))
            (html:meta (:name "Keywords"     :content "LSE, L.S.E, Langage Symbolique d'Enseignement, Langage de programmation, Français")))
          (html:body -
            (html:h1 - (html:pcdata "Système L.S.E. sur unix"))
            (html:ol -
              (let ((chapters (chapters-per-category)))
                (dolist (category *indexed-categories*)
                  (html:li ()
                    (html:a (:href (html-doc-category-file-name category))
                      (html:pcdata "~A" category))))))))))))


(defun generate-html-documentation (subdir)
  (let ((*default-pathname-defaults* (make-pathname :directory (list :relative subdir))))
    (ensure-directories-exist "file.test")
    (generate-index)
    (loop
      :for (prev category next) :on (append '(nil) *indexed-categories* '(nil))
      :while category
      :do (generate-category category
                             (when prev (html-doc-category-file-name prev))
                             "index.html"
                             (when next (html-doc-category-file-name next))))))


;; (split-text (chapter-text (find-chapter "2. B.A.BA" "GÉNÉRALITÉS")))
;; (split-text (chapter-text (find-chapter "INTRODUCTION" "COMMANDES")))
(generate-html-documentation "doc-unix-cli")

;;;; THE END ;;;;;
