;;;; scale.lisp

(in-package scale-calculator)

(defclass scale ()
  ((name :type (string) :accessor name :initarg :name)
   (intervals :type (cons) :accessor intervals :initarg :intervals)))

(defun make-scale (name intervals)
  "Create a new scale with the given name and intervals"
  (make-instance 'scale :name name :intervals intervals))

(defmethod print-object ((s scale) stream)
  "Print an ascii representation of the given scale"
  (format stream "~a Scale" (name s)))

(defun scale-notes-for (scale root)
  "Compute notes for the given scale starting with the given root"
  (loop for (interval distance) in (intervals scale)
	with notes = '()
	do (let ((note (note-add root interval)))
	     (when (and (> distance 0) (not (= (note-distance root note) distance)))
	       (flat-equivalent note))
	     (push note notes))
	finally (return (cons root (reverse notes)))))
