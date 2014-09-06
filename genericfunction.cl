(load "MersenneTwister.cl")
(load "mt64.cl") 
(load "isaac.cl")
(load "linear.cl")


(defpackage :com.randomnumbergenerator.suite
	
	(:use	:com.randomnumbergenerator.lcg32
			:com.randomnumbergenerator.isaac32
			:com.randomnumbergenerator.mt32
			:common-lisp)

	(:import-from :com.randomnumbergenerator.mt64 :extract-number64)
	
	(:export :random-chunk-32
		    :random-chunk-64
		    :mt-state
		    :rng-state
		    :isaac-state)

(in-package :com.randomnumbergenerator.suite)


(defgeneric random-chunk-32 (state)
	(:documentation "Extract the next random number generated from given specific algorithm state"))

(defmethod random-chunk-32 ((state mt-state))
	(extract-number state))

(defmethod random-chunk-32 ((state rng-state))
	(pseudo-random state))

(defmethod random-chunk-32 ((state isaac-state))
	(main-motor state))

(defgeneric random-chunk-64 (state)
	(:documentation "Extract the next 64bit random number from given spacific algorithm state"))

(defmethod random-chunk-64 ((state mt-state))
	(extract-number64 state))