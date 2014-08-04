;; Fully translated from the file readable.c from http://www.burtleburtle.net/bob/rand/isaacafa.html


(defclass isaac-state ()
	((randrsl :accessor randrsl
		:initform (loop for i from 0 to 255 collect 0))
	 (randcnt :accessor randcnt
	 	:initform -1)
	 (mm :accessor mm
	 	:initform (loop for i from 0 to 255 collect 0))
	 (aa :accessor aa
	 	:initform 0)
	 (bb :accessor bb
	 	:initform 0)
	 (cc :accessor cc
	 	:initform 0)))

(defparameter *st-isaac* (make-instance 'isaac-state))
(defparameter *ub4* #xFFFFFFFF)

(defun isaac ()
	(let (x y)
		(setf (cc *st-isaac*) (+ (cc *st-isaac*) 1))
		(setf (bb *st-isaac*) (+ (cc *st-isaac*) (bb *st-isaac*)))
		(loop for i from 0 to 255
			do (setf x (nth i (mm *st-isaac*)))
			do (setf (aa *st-isaac*)
					(logand *ub4* (logxor 
						(aa *st-isaac*)
						(case (rem i 4)
							(0 (ash (aa *st-isaac*) 13))
							(1 (ash (aa *st-isaac*) -6))
							(2 (ash (aa *st-isaac*) 2))
							(3 (ash (aa *st-isaac*) 16))))))
			do (setf (aa *st-isaac*)
					(logand *ub4* (+ (nth (rem (+ i 128) 256) (mm *st-isaac*)) (aa *st-isaac*))))
			do (setf (nth i (mm *st-isaac*))
					(setf y
						(logand *ub4* (+ (nth (rem (ash x -2) 256) (mm *st-isaac*)) (aa *st-isaac*) (bb *st-isaac*)))))
			do (setf (nth i (randrsl *st-isaac*))
					(setf (bb *st-isaac*)
						(logand *ub4* (+ (nth (rem (ash y -10) 256) (mm *st-isaac*)) x)))))))

(defun mix (a b c d e f g h)
	(setf a (logxor a (ash b 11)))   (setf d (+ d a)) (setf b (+ b c))
	(setf b (logxor b (ash c -2)))   (setf e (+ e b)) (setf c (+ c d))
	(setf c (logxor c (ash d 8)))    (setf f (+ f c)) (setf d (+ d e))
	(setf d (logxor d (ash e -16)))  (setf g (+ g d)) (setf e (+ e f))
	(setf e (logxor e (ash f 10)))   (setf h (+ h e)) (setf f (+ f g))
	(setf f (logxor f (ash g -4)))   (setf a (+ a f)) (setf g (+ g h))
	(setf g (logxor g (ash h 8)))    (setf b (+ b g)) (setf h (+ h a))
	(setf h (logxor h (ash a -9)))   (setf c (+ c h)) (setf a (+ a b))
	(setf a (logand a *ub4*) b (logand b *ub4*) c (logand c *ub4*)
		  d (logand d *ub4*) e (logand e *ub4*) f (logand f *ub4*)
		  g (logand g *ub4*) h (logand h *ub4*)))



(defun rand-init (flag)
	(let (a b c d e f g h)
		(setf (aa *st-isaac*) (setf (bb *st-isaac*)	(setf (cc *st-isaac*) 0)))
		(setf a (setf b (setf c (setf d (setf e (setf f (setf g (setf h #x9e3779b9))))))))
		(loop for i from 1 to 3
			do (mix a b c d e f g h))
		(loop for i from 0 to 255 by 8
			do (if (> flag 0)
					(progn
						(setf a (+ a (nth (+ i 0) (randrsl *st-isaac*)))) (setf b (+ b (nth (+ i 1) (randrsl *st-isaac*))))
					 	(setf c (+ c (nth (+ i 2) (randrsl *st-isaac*)))) (setf d (+ d (nth (+ i 3) (randrsl *st-isaac*))))
					 	(setf e (+ e (nth (+ i 4) (randrsl *st-isaac*)))) (setf f (+ f (nth (+ i 5) (randrsl *st-isaac*))))
					 	(setf g (+ g (nth (+ i 6) (randrsl *st-isaac*)))) (setf h (+ h (nth (+ i 7) (randrsl *st-isaac*))))))
			do (mix a b c d e f g h)
			do (setf (nth (+ i 0) (mm *st-isaac*)) a) do (setf (nth (+ i 1) (mm *st-isaac*)) b)
			do (setf (nth (+ i 2) (mm *st-isaac*)) c) do (setf (nth (+ i 3) (mm *st-isaac*)) d)
			do (setf (nth (+ i 4) (mm *st-isaac*)) e) do (setf (nth (+ i 5) (mm *st-isaac*)) f)
			do (setf (nth (+ i 6) (mm *st-isaac*)) g) do (setf (nth (+ i 7) (mm *st-isaac*)) h))
		(if (> flag 0)
			(loop for i from 0 to 255 by 8
				do (setf a (+ a (nth (+ i 0) (randrsl *st-isaac*)))) do (setf b (+ b (nth (+ i 1) (randrsl *st-isaac*))))
				do (setf c (+ c (nth (+ i 2) (randrsl *st-isaac*)))) do (setf d (+ d (nth (+ i 3) (randrsl *st-isaac*))))
				do (setf e (+ e (nth (+ i 4) (randrsl *st-isaac*)))) do (setf f (+ f (nth (+ i 5) (randrsl *st-isaac*))))
				do (setf g (+ g (nth (+ i 6) (randrsl *st-isaac*)))) do (setf h (+ h (nth (+ i 7) (randrsl *st-isaac*))))
				do (mix a b c d e f g h)
				do (setf (nth (+ i 0) (mm *st-isaac*)) a) do (setf (nth (+ i 1) (mm *st-isaac*)) b)
				do (setf (nth (+ i 2) (mm *st-isaac*)) c) do (setf (nth (+ i 3) (mm *st-isaac*)) d)
				do (setf (nth (+ i 4) (mm *st-isaac*)) e) do (setf (nth (+ i 5) (mm *st-isaac*)) f)
				do (setf (nth (+ i 6) (mm *st-isaac*)) g) do (setf (nth (+ i 7) (mm *st-isaac*)) h)))
		(isaac)
		(setf (randcnt *st-isaac*) 256)))

(defun main-motor ()
	(if (= (randcnt *st-isaac*) -1)   			;;first execution ever
		(progn
			(rand-init 1)
			(setf (randcnt *st-isaac*) 0)))
	(if (= (randcnt *st-isaac*) 255)			;;check if reached final of algorithm
		(setf (randcnt *st-isaac*) 0))
	(if (= (randcnt *st-isaac*) 0)				;;New values generated
		(isaac))
	(setf (randcnt *st-isaac*) (+ (randcnt *st-isaac*) 1))
	(return-from main-motor (nth (randcnt *st-isaac*) (randrsl *st-isaac*))))






