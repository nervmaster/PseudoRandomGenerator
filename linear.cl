;;Attempting to create a LCG with the constraints defined
;;M = (2^31) (~32 bits)
;;C = 66201 (Coprime of m)
;;A = 268435457 (A-1 shares all prime factors of m and is also multiple of 4)

(defpackage :com.randomnumbergenerator.lcg32
	(:use common-lisp)
	(:export :pseudo-random :rng-state))

(in-package :com.randomnumbergenerator.lcg32)

(defun get-magic-number ()
	(rem (get-universal-time) (expt 2 16))) ;;No special reason at all

(defclass rng-state ()
	;;"Class that defines the actual state of the lcg"
	((a :accessor a-value
		:initarg :a
		:initform 268435457)
	 (c :accessor c-value
	 	:initarg :c
	 	:initform 66201)
	 (m :accessor m-value
	 	:initarg :m
	 	:initform (expt 2 31))
	 (seed :accessor seed-value
	 	:initarg :seed
	 	:initform (get-magic-number))))

(defparameter *state-of-rng* (make-instance 'rng-state))

(defun pseudo-random ( &optional (state *state-of-rng*))
	"Linear congruentional generator"
		(setf (seed-value state) 
			(rem (+	(* (seed-value state) (a-value state)) (c-value state))	(m-value state)))) ;; x = (ax + c) mod m

