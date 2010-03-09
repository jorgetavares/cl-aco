(in-package #:cl-aco)

;;;
;;; construct solutions
;;;

(defun construct-solutions (parameters colony)
  "Construct a TSP tour."
  (let ((n (parameters-n parameters))
	(ants (colony-ants colony))
	(step 1))
    (empty-ants-memory n ants)
    (assign-initial-city n ants step)
    (construct-tour n ants step 
		    (colony-choice-info colony)
		    (parameters-decision-rule parameters) parameters)
    (compute-tour-length n ants 
			 (parameters-eval-tour parameters)
			 (parameters-distances parameters))))

(defun empty-ants-memory (n ants)
  "Mark all cities to unvisited."
  (loop for ant across ants
     do (loop for i from 1 to n
	   do (progn
		(setf (aref (ant-visited ant) i) nil)
		(setf (aref (ant-tour ant) i) 0 )))
     do (setf (aref (ant-tour ant) (1+ n)) 0)))

(defun assign-initial-city (n ants step)
  "Assign a random initial city and marks it as visited."
  (loop for ant across ants
     do (let ((position (1+ (random n))))
	  (setf (aref (ant-tour ant) step) position
		(aref (ant-visited ant) position) t))))

(defun construct-tour (n ants step choice-info decision-rule parameters)
  "Each ant constructs a complete solution (tour)."
  (loop while (< step n)
     do (progn
	  (incf step)
	  (loop for ant across ants
	     do (funcall decision-rule ant step n choice-info parameters)))))

(defun compute-tour-length (n ants fn-eval distances)
  "Computes the length of a tour. Add the return to the begin."
  (loop for ant across ants
     do (progn
	  (setf (aref (ant-tour ant) (1+ n)) (aref (ant-tour ant) 1))
	  (setf (ant-tour-length ant)
		(funcall fn-eval (ant-tour ant) n distances)))))
