;; 2^32 = 4294967296
;; M = 4294967295 (2^32)
;; C = 2147488179 (Coprime of M)
;; A = 1431655765 (A-1 Shares prime-factors of M and also 4)


(defun get-magic-number ()
	(+ 1 1))

(defvar *state-of-rng* (get-magic-number))

(defun pseudo-random ( &optional (seed *state-of-rng*))
	"Linear congruentional generator"
	(let ((a 154641) (c 10071) (m 4294967295) (x seed))
		(setf *state-of-rng* (rem (+ (* x a) c) m))))
	