;; scale-calculator: compute scale notes

(defpackage scale-calculator
  (:use "common-lisp")
  (:export "make-note"
	   "scale-notes-for"
	   "+natural-minor-scale+"
	   "+harmonic-minor-scale+"
	   "+minor-pentatonic-scale+"
	   "+natural-major-scale+"))

