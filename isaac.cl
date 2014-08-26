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

(defun isaac (&optional (state *st-isaac*))
	(let (x y)
		(setf (cc state) (+ (cc state) 1))
		(setf (bb state) (+ (cc state) (bb state)))
		(loop for i from 0 to 255
			do (setf x (nth i (mm state)))
			do (setf (aa state)
					(logand *ub4* (logxor 
						(aa state)
						(case (rem i 4)
							(0 (ash (aa state) 13))
							(1 (ash (aa state) -6))
							(2 (ash (aa state) 2))
							(3 (ash (aa state) 16))))))
			do (setf (aa state)
					(logand *ub4* (+ (nth (rem (+ i 128) 256) (mm state)) (aa state))))
			do (setf (nth i (mm state))
					(setf y
						(logand *ub4* (+ (nth (rem (ash x -2) 256) (mm state)) (aa state) (bb state)))))
			do (setf (nth i (randrsl state))
					(setf (bb state)
						(logand *ub4* (+ (nth (rem (ash y -10) 256) (mm state)) x)))))))

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



(defun rand-init (flag &optional (state *st-isaac*))
	(let (a b c d e f g h)
		(setf (aa state) (setf (bb state)	(setf (cc state) 0)))
		(setf a (setf b (setf c (setf d (setf e (setf f (setf g (setf h #x9e3779b9))))))))
		(loop for i from 1 to 3
			do (mix a b c d e f g h))
		(loop for i from 0 to 255 by 8
			do (if (> flag 0)
					(progn
						(setf a (+ a (nth (+ i 0) (randrsl state)))) (setf b (+ b (nth (+ i 1) (randrsl state))))
					 	(setf c (+ c (nth (+ i 2) (randrsl state)))) (setf d (+ d (nth (+ i 3) (randrsl state))))
					 	(setf e (+ e (nth (+ i 4) (randrsl state)))) (setf f (+ f (nth (+ i 5) (randrsl state))))
					 	(setf g (+ g (nth (+ i 6) (randrsl state)))) (setf h (+ h (nth (+ i 7) (randrsl state))))))
			do (mix a b c d e f g h)
			do (setf (nth (+ i 0) (mm state)) a) do (setf (nth (+ i 1) (mm state)) b)
			do (setf (nth (+ i 2) (mm state)) c) do (setf (nth (+ i 3) (mm state)) d)
			do (setf (nth (+ i 4) (mm state)) e) do (setf (nth (+ i 5) (mm state)) f)
			do (setf (nth (+ i 6) (mm state)) g) do (setf (nth (+ i 7) (mm state)) h))
		(if (> flag 0)
			(loop for i from 0 to 255 by 8
				do (setf a (+ a (nth (+ i 0) (randrsl state)))) do (setf b (+ b (nth (+ i 1) (randrsl state))))
				do (setf c (+ c (nth (+ i 2) (randrsl state)))) do (setf d (+ d (nth (+ i 3) (randrsl state))))
				do (setf e (+ e (nth (+ i 4) (randrsl state)))) do (setf f (+ f (nth (+ i 5) (randrsl state))))
				do (setf g (+ g (nth (+ i 6) (randrsl state)))) do (setf h (+ h (nth (+ i 7) (randrsl state))))
				do (mix a b c d e f g h)
				do (setf (nth (+ i 0) (mm state)) a) do (setf (nth (+ i 1) (mm state)) b)
				do (setf (nth (+ i 2) (mm state)) c) do (setf (nth (+ i 3) (mm state)) d)
				do (setf (nth (+ i 4) (mm state)) e) do (setf (nth (+ i 5) (mm state)) f)
				do (setf (nth (+ i 6) (mm state)) g) do (setf (nth (+ i 7) (mm state)) h)))
		(isaac state)
		(setf (randcnt state) 256)))

(defun main-motor (&optional (state *st-isaac*))
	(if (= (randcnt state) -1)   			;;first execution ever
		(progn
			(rand-init 1 state)
			(setf (randcnt state) 0)))
	(if (= (randcnt state) 255)			;;check if reached final of algorithm
		(setf (randcnt state) 0))
	(if (= (randcnt state) 0)				;;New values generated
		(isaac state))
	(setf (randcnt state) (+ (randcnt state) 1))
	(return-from main-motor (nth (randcnt state) (randrsl state))))






