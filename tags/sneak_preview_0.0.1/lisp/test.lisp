(defmacro test (&body body)
  (let ((print-forms nil))
    `(progn ,@(loop for b in body collect
		    (if (eq (first b) :print-forms)
			(setf print-forms t)
			(if print-forms
			  `(progn
			    (format t "Testing: ~a~%" ',b)
			    ,b
			    (format t "__"))
			  `(progn
			    ,b
			    (format t "__"))))))))
			  