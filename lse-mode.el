;;;; -*- mode:emacs-lisp;coding:utf-8 -*-
;;;;****************************************************************************
;;;;FILE:               lse-mode.el
;;;;LANGUAGE:           emacs lisp
;;;;SYSTEM:             POSIX
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    L.S.E. editing support package
;;;;    
;;;;    A major mode for editing L.S.E. code.  It provides convenient
;;;;    abbrevs for L.S.E. keywords, knows about the standard layout
;;;;    rules, and supports a native compile command.
;;;;
;;;;    http://www.emacswiki.org/cgi-bin/wiki.pl?SampleMode
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal Bourguignon <pjb@informatimago.com>
;;;;          Michael Schmidt <michael@pbinfo.UUCP>
;;;;          Tom Perrine <Perrin@LOGICON.ARPA>
;;;;          Mick Jordan
;;;;          Peter Robinson
;;;;MODIFICATIONS
;;;;    2003-01-31 <PJB> Created, from the modula-2 mode.
;;;;    2005-08-21 <PJB> Added header & license.
;;;;BUGS
;;;;LEGAL
;;;;    GPL
;;;;    
;;;;    Copyright Pascal Bourguignon 2005 - 2005
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
;;;;****************************************************************************

(require 'pjb-strings)
(require 'cl)


(setq auto-mode-alist (append '(("\\.\\(lse\\|LSE\\)\\'" . lse-mode))
                              auto-mode-alist))



(defgroup lse nil
  "Major mode for editing L.S.E. code."
  :prefix "lse-"
  :group 'languages)


(defvar lse-mode-syntax-table nil
  "Syntax table in use in L.S.E. buffers.")

(if lse-mode-syntax-table
    ()
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\) ")(" table)
    (modify-syntax-entry ?* "." table)
    (modify-syntax-entry ?& "_" table)
    (modify-syntax-entry ?+ "." table)
    (modify-syntax-entry ?- "." table)
    (modify-syntax-entry ?= "." table)
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?\' "\"" table)
    (setq lse-mode-syntax-table table)) )


(defvar lse-mode-map nil
  "Keymap used in L.S.E. mode.")

(if lse-mode-map
    ()
    (let ((map (make-sparse-keymap)))
      (do ((lcc ?a (1+ lcc))
           (ucc ?A (1+ ucc)))
          ((< ?z lcc))
        (define-key map (make-string 1 lcc) (make-string 1 ucc)))
      ;; (define-key map "_"  'lse-assign)
      (define-key map "\n"  'lse-newline)
      (define-key map "\r"  'lse-newline)
      (define-key map "\C-c\C-z" 'suspend-emacs)
      (setq lse-mode-map map)) )

;;(define-key  lse-mode-map "_"  'self-insert-command)


(setq lse-default-font-lock
      ;;"Font-lock for L.S.E. mode.")
      (append 
       '(
         ;; pretty chars:
         ("\\(_\\)" (1 (progn (compose-region (match-beginning 1) (match-end 1)
                                        ?← 'decompose-region) nil)))
         ("\\(\\^\\)" (1 (progn (compose-region (match-beginning 1) (match-end 1)
                                        ?↑ 'decompose-region) nil)))
         ;; comments:
         ("\\(^[0-9]+\\|; *\\)\\(\\*.*\\)"
          (2 font-lock-comment-face))
         ;; strings
         ("\\('[^'\n]*'\\)"
          (1 font-lock-string-face)))
       
       (let ((lse-keywords
              '("AFFICHER" "ALLER" "ALORS" "CHAINE" "CHARGER" "DEBUT"
                "EN" "ET" "EXECUTER" "FAIRE" "FIN" "GARER"
                "JUSQUA" "LIBERER" "LIRE" "LOCAL" "NON" "OU" "PAS"
                "PAUSE" "POUR" "PROCEDURE" "QUE" "RESULTAT" "RETOUR"  
                "SI" "SINON" "SUPPRIMER" "TABLEAU" "TANT" "TERMINER" ))
             (lse-builtins
              '("ENT" "RAC" "ABS" "EXP" "LGN" "SIN" "COS" "ATG" "ALE"
                "TEM" "ATT" "DIS" "ETL" "OUL" "OUX" "CCA" "CNB" "CNB"
                "DAT" "EQC" "EQN" "EQN" "GRL" "GRL" "LGR" "POS" "PTR"
                "PTR" "SCH" "SCH" "SKP" "SKP" ))
             (lse-types
              '("CHAINE" "TABLEAU" ))
             )
         (list
          ;;
          ;; Keywords except those fontified elsewhere.
          (concat "\\<\\(" (regexp-opt lse-keywords) "\\)\\>")
          ;;
          ;; Builtins.
          (cons (concat "\\<\\(" (regexp-opt lse-builtins) "\\)\\>")
                'font-lock-builtin-face)
          ;;
          ;; Type names.
          (cons (concat "\\<\\(" (regexp-opt lse-types) "\\)\\>")
                'font-lock-type-face)
          ))   
       '(
         ;; fonction
         ("\\(&[A-Z][A-Z0-9]\\{0,4\\}\\)\\>"
          (1 font-lock-function-name-face))
         ;; variables
         ("\\<\\([A-Z][A-Z0-9]\\{0,4\\}\\)\\>"
          (1 font-lock-variable-name-face))
         ;; constantes
         ("\\([0-9]+\\.[0-9]+E[-+]?[0-9]+\\)"    (1 font-lock-constant-face))
         ("\\([0-9]+\\.E[-+]?[0-9]+\\)"          (1 font-lock-constant-face))
         ("\\([0-9]+E[-+]?[0-9]+\\)"             (1 font-lock-constant-face))
         ("\\([0-9]+\\.[0-9]*\\)"                (1 font-lock-constant-face))
         ("\\([0-9]+\\)"                         (1 font-lock-constant-face))
         ;; line numbers:
         ("^\\([0-9]+\\)"
          (1 font-lock-type-face)))))


(setq lse-font-lock lse-default-font-lock)

;;  "Default expressions to highlight in L.S.E. modes.")



;;;###autoload
(defun lse-mode ()
  "This is a mode intended to support program development in L.S.E..
\\<lse-mode-map>
"
  (interactive)
  (let ((modp (buffer-modified-p)))
    (kill-all-local-variables)
    (use-local-map lse-mode-map)
    (setq major-mode 'lse-mode)
    (setq mode-name "L.S.E.")
    (set-syntax-table lse-mode-syntax-table)
    ;;(make-local-variable 'paragraph-start)
    ;;(setq paragraph-start (concat "$\\|" page-delimiter))
    ;;(make-local-variable 'paragraph-separate)
    ;;(setq paragraph-separate paragraph-start)
    ;;(make-local-variable 'paragraph-ignore-fill-prefix)
    ;;(setq paragraph-ignore-fill-prefix t)
    ;; (make-local-variable 'indent-line-function)
    ;; (setq indent-line-function 'c-indent-line)
    (make-local-variable 'require-final-newline)
    (setq require-final-newline t)
    (make-local-variable 'comment-start)
    (setq comment-start "*")
    (make-local-variable 'comment-end)
    (setq comment-end "")
    ;;(make-local-variable 'comment-column)
    ;;(setq comment-column 41)
    (make-local-variable 'comment-start-skip)
    (setq comment-start-skip "\\*")
    ;;(make-local-variable 'comment-indent-function)
    ;;(setq comment-indent-function 'lse-comment-indent)
    ;;(make-local-variable 'parse-sexp-ignore-comments)
    ;;(setq parse-sexp-ignore-comments t)
    (make-local-variable 'font-lock-defaults)
    (setq font-lock-defaults '(lse-font-lock  t t nil nil ))
    (setf (buffer-modified-p (current-buffer)) modp))
  (run-hooks 'lse-mode-hook))



(defun lse-newline ()
  "Insert newline and line number incremented with the same step as previously."
  (interactive)
  (newline)
  (let ((nlpt (point))
        (line (progn
                (forward-line -1)
                (beginning-of-line)
                (if (looking-at " *[0-9]+")
                    (let ((curr (string-to-number (match-string 0)))
                          (curpos (point)))
                      (forward-line -1)
                      (beginning-of-line)
                      (cond
                        ((= curpos (point))
                         (1+ curr))
                        ((looking-at " *[0-9]+")
                         (let ((prev (string-to-number (match-string 0))))
                           (+ curr (abs (- curr prev)))))
                        (t
                         (+ 10 curr))))
                  10))))
    (goto-char nlpt)
    (beginning-of-line)
    (insert (format "%d " line))
    (when (looking-at " +")
      (delete-region (match-beginning 0) (match-end 0)))))


(defun* lse-renumber-region (start end &optional (from 1) (step 1))
  (interactive "r\nnFrom: \nnStep: ")
  (let ((linum from)
        (end (let ((m (make-marker))) (set-marker m end) m)))
    (goto-char start)
    (while (< (point) end)
      (beginning-of-line)
      (when (looking-at " *[0-9]+ *")
        (delete-region (match-beginning 0) (match-end 0)))
      (if (looking-at "*")
          (insert (format "%d" linum))
          (insert (format "%d " linum)))
      (incf linum step)
      (forward-line 1))))





;; (defparameter lse-assign-image  
;;   (find-image 
;;    (list
;;     (list :type 'xbm 
;;           :width 8
;;           :height 12
;;           :ascent 91
;;           :background nil
;;           :foreground nil
;;           :data
;;           (loop
;;              with lines = (mapcar 
;;                            (lambda (line)
;;                              (loop
;;                                 with vec = (make-bool-vector (length line) nil)
;;                                 for bit in (mapcar
;;                                             (lambda (char) (STRING/= char ".")) 
;;                                             (split-string line ""))
;;                                 for i from 0 below (length vec)
;;                                 do (setf (aref vec i) bit)
;;                                 finally (return vec)))
;;                            (split-string "........\n........\n........\n..**....\n.**.....\n*******.\n.**.....\n..**....\n........\n........\n........\n........\n"))
;;              with pic = (make-vector (length lines) nil)
;;              for line in lines
;;              for i from 0
;;              do (setf (aref pic i) line)
;;              finally (return pic)) )))
;; "A left arrow glyph.")


;; (defun lse-underline-to-assign ()
;;   (interactive)
;;   (save-excursion
;;     (save-restriction
;;       (widen)
;;       (goto-char (point-min))
;;       (while (search-forward "_" nil t)
;;         (delete-region (match-beginning 0) (match-end 0))
;;         (insert-image lse-assign-image "_")))))


;; (defun lse-put-image (image pos &optional string area)
;;   "Put image IMAGE in front of POS in the current buffer.
;; IMAGE must be an image created with `create-image' or `defimage'.
;; IMAGE is displayed by putting an overlay into the current buffer with a
;; `before-string' STRING that has a `display' property whose value is the
;; image.  STRING is defaulted if you omit it.
;; POS may be an integer or marker.
;; AREA is where to display the image.  AREA nil or omitted means
;; display it in the text area, a value of `left-margin' means
;; display it in the left marginal area, a value of `right-margin'
;; means display it in the right marginal area."
;;   (unless string (setq string "x"))
;;   (let ((buffer (current-buffer)))
;;     (unless (eq (car-safe image) 'image)
;;       (error "Not an image: %s" image))
;;     (unless (or (null area) (memq area '(left-margin right-margin)))
;;       (error "Invalid area %s" area))
;;     (let (overlay
;;           (curpos (point))
;;           (prop (if (null area) image (list (list 'margin area) image))))
;;       (setq string (copy-seq string))
;;       (put-text-property 0 (length string) 'display prop string)
;;       (insert string)
;;       (setq overlay (make-overlay curpos (point) buffer))
;;       (overlay-put overlay 'put-image t)
;;       (overlay-put overlay 'evaporate t)
;;       (overlay-put overlay 'intangible nil)
;;       )))


;; (defun lse-assign ()
;;   "Insert an assign operator (the ASCII code of the underline) 
;; but adding an overlay on it with the `lse-assign-image' (a left arrow)."
;;   (interactive)
;;   (lse-put-image lse-assign-image (point) "_"))



(provide 'lse-mode)
;;(provide 'lse)

;;;; THE END ;;;;
