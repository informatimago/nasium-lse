
(defun convert-line (line number)
  (format t "~D AFFICHER[~{~A,~}/]~%"
          number
          (mapcan (lambda (seq)
                    (when seq
                      (list
                       (if (char= #\space (car seq))
                           (format nil "~DX" (length seq))
                           (format nil "~D'~C'" (length seq) (car seq))))))
                  (com.informatimago.common-lisp.cesarum.list:nsplit-list-on-indicator
                   (coerce line 'list)
                   (function char/=)))))

(defun convert-aa-to-af (path &key (start-line 10))
  (with-open-file (aa path)
    (loop
      :for line = (read-line aa nil nil)
      :while line
      :do (convert-line line start-line)
          (incf start-line))))

(convert-aa-to-af #P"~/src/pjb/nasium-lse/LSE.txt")





10 AFFICHER[,/]
11 AFFICHER[,/]
12 AFFICHER[,/]
13 AFFICHER[,/]
14 AFFICHER[,/]
15 AFFICHER[,/]
16 AFFICHER[6X,3'L',27X,4'S',17X,15'E',1X,/]
17 AFFICHER[6X,3'L',24X,10'S',14X,15'E',10X,/]
18 AFFICHER[6X,3'L',23X,3'S',6X,3'S',13X,3'E',12X,/]
19 AFFICHER[6X,3'L',22X,3'S',23X,3'E',12X,/]
20 AFFICHER[6X,3'L',22X,3'S',23X,3'E',12X,/]
21 AFFICHER[6X,3'L',23X,4'S',21X,3'E',12X,/]
22 AFFICHER[6X,3'L',26X,5'S',17X,8'E',7X,/]
23 AFFICHER[6X,3'L',30X,4'S',14X,3'E',12X,/]
24 AFFICHER[6X,3'L',33X,3'S',12X,3'E',12X,/]
25 AFFICHER[6X,3'L',33X,3'S',12X,3'E',12X,/]
26 AFFICHER[6X,3'L',23X,3'S',6X,3'S',13X,3'E',12X,/]
27 AFFICHER[6X,15'L',12X,10'S',14X,15'E',10X,/]
28 AFFICHER[6X,15'L',15X,4'S',17X,15'E',/]
29 AFFICHER[,/]
30 AFFICHER[,/]
31 AFFICHER[,/]
32 AFFICHER[,/]
33 AFFICHER[,/]
34 AFFICHER[,/]
35 AFFICHER[79'_',/]
