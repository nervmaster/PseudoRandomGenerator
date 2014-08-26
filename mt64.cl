;; Translating the pseudo-algorithm found on wikipedia: http://en.wikipedia.org/wiki/Mersenne_twister
;; Will try to compare with the actual lisp algorithm on common lisp
;; Implementing a 64bit version


(defclass mt-state ()
	((mt :accessor mt-vector
		 :initform (loop for i from 0 to 623 collect i))
	 (index :accessor index
	 	:initform '-1)))


(defparameter *MT* (make-instance 'mt-state))



(defun initialize-generator64 (&optional (seed 12345) (state *MT*))
	"initializes the generator from a seed"
	(setf (index state) 0)
	(setf (first (mt-vector state)) seed)
	(loop for i from 1 to 623
		do (setf (nth i (mt-vector state)) ;; MT[i] := lowest 32 bits of(0x6c0789656c078965 * (MT[i-1] xor (right shift by 61 bits(MT[i-1]))) + i)
				(logand 
					(*  
						#x6c0789656c078965
						(+ 
							(logxor 
								(nth (- i 1) (mt-vector state))
								(ash (nth (- i 1) (mt-vector state)) (- 61)))
							i))
					#xFFFFFFFFFFFFFFFF))))

(defun generate-numbers64 (&optional (state *MT*))
	(let (y)
		(loop for i from 0 to 623 
			do (setf y 						;; y = (MT[i] and 0x80000000...) + (MT[(i+1) mod 624] and 0x7FFFFFFF...)
					(+
						(logand (nth i (mt-vector state)) #x8000000000000000)
						(logand (nth (rem (+ i 1) 624) (mt-vector state)) #x7fffffffffffffff)))
			do (setf (nth i (mt-vector state))			;; MT[i] = MT[(i + 397) mod 624] xor (right shift by 1 bit(y))
					(logxor (nth (rem (+ i 397) 624) (mt-vector state)) (ash y (- 1))))
			when (not (evenp y))			;;When odd
				do	(setf (nth i (mt-vector state))		;; MT[i] = MT[i] xor 0x9908b0df
						(logxor (nth i (mt-vector state)) #x9908b0df9908b0df)))))


(defun extract-number64 (&optional (state *MT*))
	"Extract a tempered pseudorandom number based on the index-th value,
	 calling (generate-numbers64) every 624 numbers"
	 (if (= (index state) -1)
	 	(initialize-generator64 (logand (get-universal-time) #xFFFFFFFFFFFFFFFFFF ) state))
	 (if (= (index state) 0)
	 	(generate-numbers64 state))
	 (let ((y (nth (index state) (mt-vector state))))
	 	(setf y 
	 		(logxor y (ash y (- 22))))
	 	(setf y
	 		(logxor y (logand (ash y 14) #x9d2c56809d2c5680)))
	 	(setf y
	 		(logxor y (logand (ash y 30) #xefc60000efc60000)))
	 	(setf y
	 		(logxor y (ash y (- 36))))
	 	(setf (index state) (rem (+ (index state) 1) 624))
	 	(return-from extract-number64 y)))
