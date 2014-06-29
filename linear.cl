;; Well with the impossibility to choose an a-1 that would share all prime factors of (- (expt 2 32) 1) I choose the arguments at random
;; The range is not so great. But puting the result enclosed with a remainder of 100 it got fairly good randomness (Need still to be tested)
;; Need to make a function to get a number by the machine's time




(defun get-magic-number ()
	(rem (get-universal-time) (expt 2 16))) ;;No special reason at all

(defclass rng-state ()
	;;"Class that defines the actual state of the lcg"
	((a :accessor a-value
		:initarg :a)
	 (c :accessor c-value
	 	:initarg :c)
	 (m :accessor m-value
	 	:initarg :m)
	 (seed :accessor seed-value
	 	:initarg :seed)))

(defparameter *state-of-rng* (make-instance 'rng-state :a 154641 :c 10071 :m (- (expt 2 32) 1) :seed (get-magic-number)))

(defun pseudo-random ( &optional (state *state-of-rng*))
	"Linear congruentional generator"
		(setf (seed-value state) 
			(rem (+	(* (seed-value state) (a-value state)) (c-value state))	(m-value state)))) ;; x = (ax + c) mod m
	