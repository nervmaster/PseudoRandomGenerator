;; Well with the impossibility to choose an a-1 that would share all prime factors of (- (expt 2 32) 1) I choose the arguments at random
;; The range is not so great. But puting the result enclosed with a remainder of 100 it got fairly good randomness (Need still to be tested)
;; Need to make a function to get a number by the machine's time

(defun get-magic-number ()
	(rem (get-universal-time) (expt 2 16))) ;;No special reason at all

(defvar *state-of-rng* (get-magic-number))

(defun pseudo-random ( &optional (seed *state-of-rng*))
	"Linear congruentional generator"
	(let ((a 154641) (c 10071) (m 4294967295) (x seed))
		(setf *state-of-rng* (rem (+ (* x a) c) m))))
	