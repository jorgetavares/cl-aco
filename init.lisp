(in-package #:cl-aco)

;;;
;;; initialize ACO
;;;

(defun initialize-colony (colony parameters)
  "Initializes all the data for a ACO run."
  (setf (colony-ants colony)
	(init-ants (parameters-n-ants parameters)
		   (parameters-n parameters)))
  (setf (colony-pheromone colony)
	(init-pheromone (parameters-n parameters) 
			(colony-trail-max colony)))
  (setf (colony-choice-info colony)
	(init-choice-info (parameters-n parameters)
			  (colony-pheromone colony)
			  (colony-heuristic colony)
			  (parameters-alpha parameters)
			  (parameters-beta parameters)))
  colony)

;;
;; generic init functions for the heuristic and
;; choice-info (heuristic + pheromones) matrixes
  
(defun init-heuristic (n distances)
  "Return a fresh heuristic info matrix."
  (let ((matrix (make-array `(,(1+ n) ,(1+ n)) :initial-element 0)))
    (loop for i from 1 to n
       do (loop for j from 1 to n
	     do (unless (= i j)
		  (setf (aref matrix i j)
			(/ 1 (aref distances i j)))))
       finally (return matrix))))

(defun init-choice-info (n pheromone heuristic alpha beta)
  "Return a fresh choice-info matrix."
  (let ((matrix (make-array `(,(1+ n) ,(1+ n)) :initial-element 0)))
    (loop for i from 1 to n
       do (loop for j from 1 to n
	     do (setf (aref matrix i j)
		      (* (expt (aref pheromone i j) alpha) 
			 (expt (aref heuristic i j) beta))))
       finally (return matrix))))

;;
;; init set of ants

(defun init-ants (n-ants n)
  "Return a fresh array of ants for the optimization procedure, i.e., the colony."
  (let ((ants (make-array n-ants)))
    (loop for i from 0 below n-ants
	 do (setf (aref ants i)
		  (make-ant :tour-length nil
			    :tour (make-array (+ n 2) :initial-element 0)
			    :visited (make-array (+ n 2) :initial-element nil)))
       finally (return ants))))


;;;
;;; init colony and problem parameters for TSP
;;; 

(defun read-prepare-data (tsp-filename)
  "Reads data from files (instances and parameters) and computes necessary pre-run stuff."
  (let* ((tsp-instance (cl-tsplib:parse-problem-instance tsp-filename))
	 (parameters (make-parameters
		      :n (cl-tsplib:problem-instance-dimension tsp-instance)
		      :distances (cl-tsplib:problem-instance-distance-matrix tsp-instance)))
	 (trail-max (update-trail-max-value parameters (parameters-avg-cost parameters)))
	 (initial-trail (initial-trail-value parameters trail-max))
	 (colony (make-colony :n-ants (parameters-n-ants parameters)
			      :ants (init-ants (parameters-n-ants parameters)
					       (parameters-n parameters))
			      :pheromone (init-pheromone (parameters-n parameters) initial-trail)
			      :trail-max trail-max
			      :trail-min (update-trail-min-value parameters trail-max)
			      :heuristic (init-heuristic (parameters-n parameters)
							 (parameters-distances parameters)))))
    (setf (parameters-nearest-neighbors parameters)
	  (list-nearest-neighbors (parameters-distances parameters)
				  (parameters-n parameters)
				  (parameters-n-neighbors parameters)))
    (values parameters colony)))

;;
;; auxiliary funtions to compute nearest neighbors 
;; of the TSP instance (should be moved to cl-tsplib)

(defun list-nearest-neighbors (distances n &optional (n-neighbors (- n 1)))
  "Returns the list of the nearest enighbors for a set of cities."
  (let ((nearest-list (make-array `(,(1+ n) ,(1+ n-neighbors)) :initial-element 0))
	(all-neighbors (compute-nearest-neighbors distances n)))
    (loop 
       for i from 1 to n
       for neighbors in all-neighbors
       do (loop for j from 1 to n-neighbors
	     do (setf (aref nearest-list i j) 
		      (nth j neighbors)))
       finally (return nearest-list))))
    
(defun compute-nearest-neighbors (distances n)
  "Computes all the nearest neighbors given a distance matrix."
  (loop with indexes = (loop for i from 1 to n collect i) 
     for i from 1 to n
     collect (loop for j from 0 to n
		collect (aref distances i j) into distances-list
		finally (return (remove i (sort (copy-list indexes) #'< 
						:key #'(lambda (x) 
							 (nth x distances-list))))))))

