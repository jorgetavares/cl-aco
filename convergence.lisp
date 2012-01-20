(in-package #:cl-aco)

;;;
;;; convergence measures
;;;

(defun update-convergence-factor (parameters colony state stats)
  "Calculates convergence of the colony and updates the state."
  (setf (state-stagnation state)
	(funcall (parameters-convergence-function parameters) state colony parameters))
  (setf (statistics-branching stats)
	(float (branching-factor state colony parameters))))

(defun std-dev (avg-cost colony)
  "Standard deviation of the colony solution cost."
  (loop for ant across (colony-ants colony)
     sum (expt (- (ant-tour-length ant) avg-cost) 2) into total
     finally (return (sqrt (/ total (colony-n-ants colony))))))

(defun convergence-std-dev (state colony parameters)
  (declare (ignore colony parameters))
  (state-pop-std-dev state))

(defun convergence-std-dev-avg (state colony parameters)
  (declare (ignore colony parameters))
  (/ (state-pop-std-dev state) (state-pop-avg state)))

(defun branching-factor (state colony parameters)
  (declare (ignore state))
  (let* ((n (parameters-n parameters))
	 (l (parameters-lambda parameters))
	 (pheromone (colony-pheromone colony))
	 (branches (make-array (1+ n) :initial-element 0)))
    (loop for i from 1 to n
       do (let ((min (aref pheromone i 1))
		(max (aref pheromone i 1))
		(cutoff 0))	      
	    (loop for j from 1 to n
	       do (let ((value (aref pheromone i j)))
		    (when (> value max) (setf max value))
		    (when (< value min) (setf min value))))
	    (setf cutoff (+ min (* l (- max min))))
	    (loop for k from 1 to n
	       when (>= (aref pheromone i k) cutoff) 
	       do (incf (aref branches i)))))
    (loop for i from 1 to n
       sum (aref branches i) into total
       finally (return (/ total n)))))
  
