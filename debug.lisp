(in-package #:cl-aco)

;;;
;;; DEBUG ONLY !!!!
;;;

(defun debug-setup-aco1 ()
  (let* ((parameters (make-parameters
		      :n 4
		      :n-ants 2
		      :max-iterations 20
		      :distances (make-array '(5 5) :initial-contents '((0 0 0 0 0)
									(0 0 1 2 1)
									(0 1 0 1 2)
									(0 2 1 0 1)
									(0 1 2 1 0)))))
	 ;(trail-max (update-trail-max-value parameters (parameters-avg-cost parameters)))
	 (trail-max 0.5)
	 (colony (make-colony :n-ants (parameters-n-ants parameters)
			      :ants (init-ants (parameters-n-ants parameters)
					       (parameters-n parameters))
			      :pheromone (init-pheromone (parameters-n parameters) trail-max)
			      :trail-max trail-max
			      :trail-min (update-trail-min-value parameters trail-max)
			      :heuristic (init-heuristic (parameters-n parameters)
							 (parameters-distances parameters)))))
    (values parameters colony)))


(defun debug-setup-aco ()
  (let* ((parameters (make-parameters
		      :n 9
		      :n-ants 9
		      :max-iterations 10
		      :distances (make-array '(10 10) 
					     :initial-contents '((0 0 0 0 0 0 0 0 0 0)
								 (0 0 1 5 1 5 5 5 5 5)
								 (0 1 0 5 5 1 5 5 5 5)
								 (0 5 5 0 5 1 1 5 5 5)
								 (0 1 5 5 0 5 5 1 5 5)
								 (0 5 1 1 5 0 5 5 5 5)
								 (0 5 5 1 5 5 0 5 5 1)
								 (0 5 5 5 1 5 5 0 1 5)
								 (0 5 5 5 5 5 5 1 0 1)
								 (0 5 5 5 5 5 1 5 1 0)
								 ))))
	 ;(trail-max (update-trail-max-value parameters (parameters-avg-cost parameters)))
	 (trail-max 0.5)
	 (colony (make-colony :n-ants (parameters-n-ants parameters)
			      :ants (init-ants (parameters-n-ants parameters)
					       (parameters-n parameters))
			      :pheromone (init-pheromone (parameters-n parameters) trail-max)
			      :trail-max trail-max
			      :trail-min (update-trail-min-value parameters trail-max)
			      :heuristic (init-heuristic (parameters-n parameters)
							 (parameters-distances parameters)))))
    (values parameters colony)))

;(defun debug-aco (&key (runs 1) (output t))
;  (multiple-value-bind (parameters colony)
;      (debug-setup-aco)
;    (run-multiple-aco-tsp parameters colony runs output)))
	   
(defun debug-branch (pheromone &optional (n 4) (l 0.05))
  (let* ((branches (make-array (1+ n) :initial-element 0)))
    (loop for i from 1 to n
       do (let ((min 100000000000)
		(max 0)
		(cutoff 0))
	    ;(format t "~a before min: ~a max:~a ~%" i min max)
	    (loop for j from 1 to n
	       unless (= i j)
	       do (let ((value (aref pheromone i j)))
		    (when (> value max) (setf max value))
		    (when (< value min) (setf min value))))
	    (format t "~a after min: ~a max:~a ~%" i min max)
	    (setf cutoff (+ min (* l (- max min))))
	    (format t "~a cutoff: ~a ~%" i cutoff)
	    (loop for k from 1 to n
	       when (>= (aref pheromone i k) cutoff) 
	       do (incf (aref branches i)))))
    (loop for i from 1 to n
       sum (aref branches i) into total
       finally (return (/ total n)))))

;; (debug-branch (make-array '(52 52) :initial-element 0.04) 51 0.05)

