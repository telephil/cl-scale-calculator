;;;; note.lisp

(in-package scale-calculator)

(defclass note ()
  ((pitch :type (integer 0 6) :accessor pitch :initarg :pitch :initform +C+)
   (accidental :type (integer 0 2) :accessor accidental :initarg :accidental :initform +natural+)))

(defun make-note (&optional (pitch +C+) (accidental +natural+))
  "Create a new note using given pitch and accidental. Defaults to natural C."
  (make-instance 'note :pitch pitch :accidental accidental))

(defun pitch-char (pitch)
  "Return the ascii representation of the given pitch"
  (code-char (+ (mod (+ 2 pitch) 7) (char-code #\A))))

(defun accidental-char (accidental)
  "Return the ascii representation of the given accidental"
  (svref #(nil #\b #\#) accidental))

(defmethod print-object ((n note) stream)
  "Print the ascii representation of the given note"
  (let ((c (accidental-char (accidental n))))
    (format stream "~a" (pitch-char (pitch n)))
    (when c
      (format stream "~a" c))))

(defun natural-p (note)
  "Tells if given note's accidental is Natural"
  (= (accidental note) +natural+))

(defun to-natural (note)
  "Change given note's accidental to Natural"
  (setf (accidental note) +natural+))

(defun flat-p (note)
  "Tells if given note's accidental is Flat"
  (= (accidental note) +flat+))

(defun to-flat (note)
  "Change given note's accidental to Flat"
  (setf (accidental note) +flat+))

(defun sharp-p (note)
  "Tells if given note's accidental is Sharp"
  (= (accidental note) +sharp+))

(defun to-sharp (note)
  "Change given note's accidental to Sharp"
  (setf (accidental note) +sharp+))

(defun has-sharp-p (note)
  "Tells whether a note has a sharp form. False for E and B"
  (and (not (= (pitch note) +E+)) (not (= (pitch note) +B+))))

(defun pitch-incf (note)
  "Increment the pitch of a given note"
  (setf (pitch note) (mod (1+ (pitch note)) 7)))

(defun flat-equivalent (note)
  "Change the note to its flat equivalent (e.g. A# -> Bb)"
  (when (or (sharp-p note) (not (has-sharp-p note)))
    (pitch-incf note)
    (to-flat note)))

(defun note-add (note interval)
  "Calculate the note which is interval tones after note"
  (let ((result (make-note (pitch note) (accidental note)))
	(halves (truncate (/ interval 0.5))))
    (loop for i from 1 to halves
       do (cond
	    ((natural-p result) 
	     (if (or (= (pitch result) +E+) (= (pitch result) +B+))
		 (pitch-incf result)
		 (to-sharp result)))
	    ((flat-p result)
	     (when (has-sharp-p result)
	       (pitch-incf result))
	     (to-natural result))
	    ((sharp-p result)
	     (pitch-incf result)
	     (to-natural result)))
       finally (return result))))

(defun note-distance (note1 note2)
  "Calculate the number of notes between two"
  (let ((n1 (pitch note1))
	(n2 (pitch note2)))
    (if (< n2 n1)
	(+ 8 (- n2 n1))
      (1+ (- n2 n1)))))

(defun note-equal (note1 note2)
  "Test whether two notes are the same"
  (if (not (= (pitch note1) (pitch note2)))
      (and (= (note-distance note1 note2) 2)
	   (or (and (sharp-p note1) (flat-p note2))
	       (and (sharp-p note2) (flat-p note1))))
    (= (accidental note1) (accidental note2))))

(defun semitones-count (note1 note2)
  "Calculate the number of semitones between two notes"
  (let ((note (make-note (pitch note1) (accidental note1))))
    (do ((count 0 (+ count 0.5)))
	((note-equal note note2) count)
      (setf note (note-add note +chromatic-semitone+)))))
