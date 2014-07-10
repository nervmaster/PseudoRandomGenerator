;; Translating the pseudo-algorithm found on wikipedia: http://en.wikipedia.org/wiki/Mersenne_twister
;; Will try to compare with the actual lisp algorithm on common lisp
;; after that implement a 64 bit version


(defclass mt-state ()
	((mt :accessor mt-vector
		 :initform (loop for i from 0 to 623 collect i))
	 (index :accessor index
	 	:initform '0)))


(defparameter *MT* (make-instance 'mt-state))



(defun initialize-generator (&optional (seed 12345))
	"initializes the generator from a seed"
	(setf (index *MT*) 0)
	(setf (first (mt-vector *MT*)) seed)
	(loop for i from 1 to 623
		do (setf (nth i (mt-vector *MT*)) ;; MT[i] := lowest 32 bits of(0x6c078965 * (MT[i-1] xor (right shift by 30 bits(MT[i-1]))) + i)
				(logand 
					(*  
						#x6c078965
						(+ 
							(logxor 
								(nth (- i 1) (mt-vector *MT*))
								(ash (nth (- i 1) (mt-vector *MT*)) (- 30)))
							i))
					#xFFFFFFFF))))

(defun generate-numbers ()
	(let (y)
		(loop for i from 0 to 623 
			do (setf y 						;; y = (MT[i] and 0x80000000) + (MT[(i+1) mod 624] and 0x7FFFFFFF)
					(+
						(logand (nth i (mt-vector *MT*)) #x8000000)
						(logand (nth (rem i 624) (mt-vector *MT*)) #x7fffffff)))
			do (setf (nth i (mt-vector *MT*))			;; MT[i] = MT[(i + 397) mod 624] xor (right shift by 1 bit(y))
					(logxor (nth (rem (+ i 397) 624) (mt-vector *MT*)) (ash y (- 1))))
			when (not (evenp y))			;;When odd
				do	(setf (nth i (mt-vector *MT*))		;; MT[i] = MT[i] xor 0x9908b0df
						(logxor (nth i (mt-vector *MT*)) #x9908b0df)))))


(defun extract-number ()
	"Extract a tempered pseudorandom number based on the index-th value,
	 calling (generate-numbers) every 624 numbers"
	 (if (= (index *MT*) 0)
	 	(generate-numbers))
	 (let ((y (nth (index *MT*) (mt-vector *MT*))))
	 	(setf y 
	 		(logxor y (ash y (- 11))))
	 	(setf y
	 		(logxor y (logand (ash y 7) #x9d2c5680)))
	 	(setf y
	 		(logxor y (logand (ash y 15) #xefc60000)))
	 	(setf y
	 		(logxor y (ash y (- 18))))
	 	(setf (index *MT*) (rem (+ (index *MT*) 1) 624))
	 	(return-from extract-number y)))














