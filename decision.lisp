(in-package #:cl-aco)

;;;
;;; decison rules
;;;

(defun as-decision (ant step n choice-info parameters)
  "Determines an ant next action to be executed (ant system, no candidates list)."
  (declare (ignore parameters)
	   (optimize (speed 3) (safety 1)))
  (let* ((tour (ant-tour ant))
	 (visited (ant-visited ant))
	 (sum-probabilities 0.0)
	 (c (aref tour (1- step)))
	 (nodes (make-array n :initial-element 0))
	 (nodes-probabilities (make-array n :initial-element 0.0))
	 (size 0))
    (loop 
       with nc = (- (* n c) n)
       for i from 1 to n
       unless (aref visited i) 
       do (let ((node-probability (aref choice-info (1- (+ nc i)))))
	    (setf (aref nodes size) i)
	    (setf (aref nodes-probabilities size) node-probability)
	    (incf sum-probabilities node-probability)
	    (incf size)))
    (when (zerop size) (error "All nodes have been visited!"))
    (if (<= sum-probabilities 0.0)
	(progn ;; shouldn't happen but...
	  (setf sum-probabilities 1.0)
	  (setf nodes-probabilities (make-array size :initial-element (/ 1 size))))
	(progn ;; convert to the scale of [0.0-1.0]
	  (let ((max sum-probabilities))
	    (flet ((scale (value)
		     (/ (* value 1.0) max)))
	      (setf sum-probabilities (scale sum-probabilities))
	      (loop for i below size
		 do (setf (aref nodes-probabilities i) 
			  (scale (aref nodes-probabilities i))))))))
    ;(format t "~a~%~a~%" sum-probabilities nodes-probabilities)
    (let ((node-position
	   (loop with limit = (random sum-probabilities)
	      for probability across nodes-probabilities
	      for position from 0 below size
	      sum probability into threshold
	      while (< threshold limit)
	      finally (return (if (>= position size) (1- size) position)))))
      (let ((node  (aref nodes node-position)))
	(setf (aref tour step) node)
	(setf (aref visited node) t)))))

(defun as-decision-random (ant step n choice-info parameters)
  (declare (ignore parameters choice-info))
  (let* ((tour (ant-tour ant))
	 (visited (ant-visited ant))
	 (nodes (loop for i from 1 to n
		      unless (aref visited i) 
		      collect i into free-nodes
		      finally (return free-nodes)))
	 (node (nth (random (length nodes)) nodes)))
    (setf (aref tour step) node)
    (setf (aref visited node) t)))


(defun as-q-decision  (ant step n choice-info parameters q-value)
   (let ((q (random 1.0)))
    (if (> q q-value)
	;; proportional
	(as-decision ant step n choice-info parameters)
	;; deterministic
	 (let* ((tour (ant-tour ant))
		(visited (ant-visited ant))
		(c (aref tour (1- step)))
		(value-best -1)
		(next-city n))
	   (loop with nc = (- (* n c) n)
		 for i from 1 to n 
		 unless (aref visited i)
		 do (progn
		      (when (> (aref choice-info (1- (+ nc i))) value-best)
			(setf next-city i)
			(setf value-best (aref choice-info (1- (+ nc i)))))))
	   (setf (aref tour step) next-city)
	   (setf (aref visited next-city) t)))))
