;;function to write the random numbers on a text file for dieharder

(defun rng-to-file (rng name seed numbit)
	(with-open-file (out "rngtofile.txt" :direction :output :if-exists :supersede)
		(format out "#==============================================~%")
		(format out "# generator ~a seed = ~a~%" name seed)
		(format out "#==============================================~%")
		(format out "type: d~%")
		(format out "count: ~a~%" (expt 10 8))
		(format out "numbit: ~a~%" numbit)
		(loop for i from 1 to (expt 10 8)
			do (format out "~a~%" (funcall rng)))))
