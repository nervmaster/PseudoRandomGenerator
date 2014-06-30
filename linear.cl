;;Attempting to create a LCG with the constraints defined
;;M = (2^31) (~32 bits)
;;C = 66201 (Coprime of m)
;;A = 268435457 (A-1 shares all prime factors of m and is also multiple of 4)


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

(defparameter *state-of-rng* (make-instance 'rng-state :a 268435457 :c 66201 :m (expt 2 31) :seed (get-magic-number)))

(defun pseudo-random ( &optional (state *state-of-rng*))
	"Linear congruentional generator"
		(setf (seed-value state) 
			(rem (+	(* (seed-value state) (a-value state)) (c-value state))	(m-value state)))) ;; x = (ax + c) mod m
	