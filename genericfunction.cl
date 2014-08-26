(load "linear.cl")
(load "MersenneTwister.cl")
(load "mt64.cl") 
(load "isaac.cl")


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