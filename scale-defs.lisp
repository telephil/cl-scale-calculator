;;;; scale-defs.lisp

(in-package scale-calculator)

(defconstant +natural-minor-scale+
  (make-scale "Natural Minor"
	      `((,+major-second+ 2)
		(,+minor-third+ 3)
		(,+perfect-fourth+ 4)
		(,+perfect-fifth+ 5)
		(,+minor-six+ 6)
		(,+minor-seventh+ 7))))

(defconstant +harmonic-minor-scale+
  (make-scale "Harmonic Minor"
	      `((,+major-second+ 2)
		(,+minor-third+ 3)
		(,+perfect-fourth+ 4)
		(,+perfect-fifth+ 5)
		(,+minor-six+ 6)
		(,+major-seventh+ 7))))

(defconstant +minor-pentatonic-scale+
  (make-scale "Minor Pentatonic"
	      `((,+minor-third+ 3)
		(,+perfect-fourth+ 4)
		(,+perfect-fifth+ 5)
		(,+minor-seventh+ 7))))

(defconstant +natural-major-scale+
  (make-scale "Natural Major"
	      `((,+major-second+ 2)
		(,+major-third+ 3)
		(,+perfect-fourth+ 4)
		(,+perfect-fifth+ 5)
		(,+major-six+ 6)
		(,+major-seventh+ 7))))
