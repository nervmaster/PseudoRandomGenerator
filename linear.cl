;; 2^32 = 4294967296
;; M = 4294967292 (2^32 - 4)
;; C = 19998721 (Coprime of M)
;; A = 1431655765 (A-1 Shares prime-factors of M and also 4)

(defun pseudo-random (seed)
	"Linear congruentional generator"
	(let ((a 1431655765) (c 19998721) (m 4294967292) (x seed))
		(loop for i from 0 to (rem (+ (* x a) c) m)
			do (setf x (rem (+ (* x a) c) m))
		(return-from pseudo-random x))))
