;; abc-export.lisp

(in-package scale-calculator)

(defun note-abc-format (note)
  "Convert a note to its abc representation"
  (if (> (accidental note) 0)
      (format nil "~a~a" (svref #(#\= #\_ #\^) (accidental note)) (pitch-char (pitch note)))
      (format nil "~a" (pitch-char (pitch note)))))

(defun export-scale-to-abc (filename scale root)
  "Create an ABC format file for the given scale starting with the given root"
  (let ((notes (scale-notes-for scale root)))
    (format t "~a" notes)
    (with-open-file (s filename :direction :output :if-exists :supersede)
      (format s "X:1~%")
      (format s "T:~a ~a~%" root scale)
      (format s "M:4/4~%")
      (format s "K:~a~%" root)
      (mapcar #'(lambda (note)
		  (format s "~a" (note-abc-format note))) notes)
      (format s "~(~a~)|]~%" (note-abc-format root)))))
