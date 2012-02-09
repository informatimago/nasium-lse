;;; This file was generated by Zebu (Version 3.5.5)

(IN-PACKAGE "COM.INFORMATIMAGO.LSE")
(REQUIRE "zebu-package")
(USE-PACKAGE "ZEBU")

(DEFUN IDENT766 (TOK-IDENTIFICATEUR)
  (PROGN (UNLESS (AND (CHAR= (CHARACTER "&")
                             (AREF
                              (TOKEN-TEXT TOK-IDENTIFICATEUR)
                              0))
                      (<= (LENGTH (TOKEN-TEXT TOK-IDENTIFICATEUR)
                                  5)))
           (ERROR "IDENTIFICATEUR INVALIDE: ~A"
                  (TOKEN-TEXT TOK-IDENTIFICATEUR)))
         TOK-IDENTIFICATEUR))


(EVAL-WHEN (:COMPILE-TOPLEVEL)
  (UNLESS (MEMBER "zebu-regex" *MODULES* :TEST #'EQUAL)
    (WARN "Load the Zebu Compiler!")))
(DECLAIM (SPECIAL ZEBU::*REGEX-GROUPS* ZEBU::*REGEX-GROUPINGS*))
(DEFUN TOK-IDENTIFICATEUR (STRING &OPTIONAL (ZEBU::START 0)
                           (ZEBU::END (LENGTH STRING)))
  (WHEN (PROGN (SETF ZEBU::*REGEX-GROUPINGS* 1)
               (BLOCK ZEBU::FINAL-RETURN
                 (BLOCK ZEBU::COMPARE
                   (LET ((ZEBU::INDEX ZEBU::START)
                         (LENGTH ZEBU::END))
                     (SETF (SVREF ZEBU::*REGEX-GROUPS* 0)
                           (LIST ZEBU::INDEX NIL))
                     (LET ((ZEBU::NEW-INDEX (+ ZEBU::INDEX 16)))
                       (IF (< LENGTH ZEBU::NEW-INDEX)
                           (RETURN-FROM ZEBU::COMPARE NIL))
                       (IF (STRING= STRING
                                    "{IDENTIFICATEUR}"
                                    :START1
                                    ZEBU::INDEX
                                    :END1
                                    ZEBU::NEW-INDEX)
                           (INCF ZEBU::INDEX 16)
                           (RETURN-FROM ZEBU::COMPARE NIL)))
                     (SETF (CADR (SVREF ZEBU::*REGEX-GROUPS* 0))
                           ZEBU::INDEX)
                     (RETURN-FROM ZEBU::FINAL-RETURN T)))))
    (SECOND (SVREF ZEBU::*REGEX-GROUPS* 0))))
