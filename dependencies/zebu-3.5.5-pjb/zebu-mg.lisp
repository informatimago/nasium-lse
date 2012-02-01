(let* ((file-path   "zebu-mg")
       (source-path (make-pathname
                     :type (first *load-source-pathname-types*)
                     :defaults *ZEBU-directory*))
       (binary-path (make-pathname
                     :type (first *load-binary-pathname-types*)
                     :defaults *ZEBU-binary-directory*))
       (ofile       (merge-pathnames
                     (make-pathname :type "tab" :defaults file-path)
                     binary-path nil))
       (odate       (and (probe-file ofile)
                         (file-write-date ofile)))
       (ifile       (merge-pathnames
                     (make-pathname :type "zb" :defaults file-path)
                     source-path nil))
       (idate       (if (probe-file ifile)
                        (file-write-date ifile)
                        (error "File not found ~a" ifile)))
       (zb:*generate-domain* nil))
  (print (list file-path source-path binary-path
               '/ ofile odate '/ ifile idate))
  (finish-output)
  (when (or (null odate) (> idate odate))
    (ZB:zebu-compile-file ifile :output-file ofile)))

