(in-package #:cl-aco)

;;;
;;; initialize ACO
;;;

(defun initialize-colony (parameters state)
  "Prepare colony data (requires parameters already with problem data."
 (let* ((trail-max (update-trail-max-value parameters (parameters-avg-cost parameters)))
	(initial-trail (initial-trail-value parameters trail-max))
	(colony (make-colony :n-ants (parameters-n-ants parameters)
			     :ants (init-ants (parameters-n-ants parameters)
					      (parameters-n parameters))
			     :pheromone (init-pheromone (parameters-n parameters) initial-trail)
			     :trail-max trail-max
			     :trail-min (update-trail-min-value parameters state trail-max)
			     :heuristic (init-heuristic (parameters-n parameters)
							(parameters-distances parameters)))))
   (setf (colony-choice-info colony)
	 (init-choice-info (parameters-n parameters)
			   (colony-pheromone colony)
			   (colony-heuristic colony)
			   (parameters-alpha parameters)
			   (parameters-beta parameters)))
   colony))

;;
;; generic init functions for the heuristic and
;; choice-info (heuristic + pheromones) matrixes
  
(defun init-heuristic (n distances)
  "Return a fresh heuristic info matrix."
  (let ((matrix (make-array `(,(1+ n) ,(1+ n)) 
			    :initial-element 0.0
			    :element-type 'single-float)))
    (loop for i from 1 to n
       do (loop for j from 1 to n
	     do (unless (= i j)
		  (setf (aref matrix i j)
			(/ 1.0 (aref distances i j)))))
       finally (return matrix))))

(defun init-choice-info (n pheromone heuristic alpha beta)
  "Return a fresh choice-info matrix."
   (let ((matrix (make-array (* n n) 
			    :initial-element 0.0
			    :element-type 'single-float)))
     (loop for i from 1 to n
	do (loop for j from 1 to n
	      do (setf (aref matrix (1- (+ (- (* n i) n) j)))
		       (* (expt (aref pheromone i j) alpha) 
			  (if (> beta 0.0)
			      (expt (aref heuristic i j) beta)
			      1.0)))))
     matrix))


;;
;; init set of ants

(defun init-ants (n-ants n)
  "Return a fresh array of ants for the optimization procedure, i.e., the colony."
  (let ((ants (make-array n-ants)))
    (loop for i from 0 below n-ants
	 do (setf (aref ants i)
		  (make-ant :tour-length nil
			    :tour (make-array (+ n 2) 
					      :initial-element 0
					      :element-type 'node)
			    :visited (make-array (+ n 2) 
						 :initial-element nil
						 :element-type 'boolean)))
       finally (return ants))))






