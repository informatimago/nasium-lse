;;; This file was generated by Zebu (Version 3.5.5)

(IN-PACKAGE "COM.INFORMATIMAGO.LSE")
(REQUIRE "zebu-package")
(USE-PACKAGE "ZEBU")

(DEFUN DEUX-NUMEROS-OPTIONELS762 (TOK-NUMERO.1
                                  TOK-VIRGULE
                                  TOK-NUMERO.2)
  (DECLARE (IGNORE TOK-VIRGULE))
  (PROGN (LIST TOK-NUMERO.1 TOK-NUMERO.2)))

(DEFUN DEUX-NUMEROS-OPTIONELS763 () (PROGN (LIST 1 NIL)))


(EVAL-WHEN (:COMPILE-TOPLEVEL)
  (UNLESS (MEMBER "zebu-regex" *MODULES* :TEST #'EQUAL)
    (WARN "Load the Zebu Compiler!")))
(DECLAIM (SPECIAL ZEBU::*REGEX-GROUPS* ZEBU::*REGEX-GROUPINGS*))
(DEFUN TOK-VIRGULE (STRING &OPTIONAL (ZEBU::START 0)
                    (ZEBU::END (LENGTH STRING)))
  (WHEN (PROGN (SETF ZEBU::*REGEX-GROUPINGS* 1)
               (BLOCK ZEBU::FINAL-RETURN
                 (BLOCK ZEBU::COMPARE
                   (LET ((ZEBU::INDEX ZEBU::START)
                         (LENGTH ZEBU::END))
                     (SETF (SVREF ZEBU::*REGEX-GROUPS* 0)
                           (LIST ZEBU::INDEX NIL))
                     (IF (AND (< ZEBU::INDEX LENGTH)
                              (EQL (CHAR STRING ZEBU::INDEX) #\,))
                         (INCF ZEBU::INDEX)
                         (RETURN-FROM ZEBU::COMPARE NIL))
                     (SETF (CADR (SVREF ZEBU::*REGEX-GROUPS* 0))
                           ZEBU::INDEX)
                     (RETURN-FROM ZEBU::FINAL-RETURN T)))))
    (SECOND (SVREF ZEBU::*REGEX-GROUPS* 0))))

(DEFUN TOK-NUMERO (STRING &OPTIONAL (ZEBU::START 0)
                   (ZEBU::END (LENGTH STRING)))
  (WHEN (PROGN (SETF ZEBU::*REGEX-GROUPINGS* 1)
               (BLOCK ZEBU::FINAL-RETURN
                 (BLOCK ZEBU::COMPARE
                   (LET ((ZEBU::INDEX ZEBU::START)
                         (LENGTH ZEBU::END))
                     (SETF (SVREF ZEBU::*REGEX-GROUPS* 0)
                           (LIST ZEBU::INDEX NIL))
                     (LET ((ZEBU::NEW-INDEX (+ ZEBU::INDEX 8)))
                       (IF (< LENGTH ZEBU::NEW-INDEX)
                           (RETURN-FROM ZEBU::COMPARE NIL))
                       (IF (STRING= STRING
                                    "{NUMERO}"
                                    :START1
                                    ZEBU::INDEX
                                    :END1
                                    ZEBU::NEW-INDEX)
                           (INCF ZEBU::INDEX 8)
                           (RETURN-FROM ZEBU::COMPARE NIL)))
                     (SETF (CADR (SVREF ZEBU::*REGEX-GROUPS* 0))
                           ZEBU::INDEX)
                     (RETURN-FROM ZEBU::FINAL-RETURN T)))))
    (SECOND (SVREF ZEBU::*REGEX-GROUPS* 0))))
