;;;;
;;;; ACO for TSP
;;;; - specific code for TSP using :cl-aco 
;;;; - NOTE: at this point, :cl-aco is directly connected to 
;;;;   the TSP problem and is not suficient generalized to be 
;;;;   used with other problems. The plan is to change that 
;;;;   in the near future.
;;;;

(in-package #:cl-aco-tsp)

;;;
;;; running aco for TSP
;;;

(defun aco-tsp (&key (ant-system :as) (filename cl-tsplib:*eil51*) (runs 1) (iterations 10) (output :screen) (restart nil)
		(restart-iterations 250) (avg-cost 426) (rho 0.5) n-ants (ls nil) (ls-fn #'2-opt-tsp) (id "acotsp"))
  "Launch an ACO system for TSP."
  (case ant-system
    (:as (ant-system :runs runs :iterations iterations :output output 
		     :filename filename
		     :problem-reader #'read-problem-data 
		     :cost-function #'symmetric-tsp
		     :rho rho
		     :opt :minimization
		     :n-ants (if n-ants n-ants 0)
		     :ls ls
		     :ls-fn ls-fn
		     :id id))
    (:eas (elite-ant-system :runs runs :iterations iterations :output output 
			    :filename filename
			    :problem-reader #'read-problem-data 
			    :cost-function #'symmetric-tsp
			    :rho rho
			    :opt :minimization
			    :n-ants (if n-ants n-ants 0)
			    :ls ls
			    :ls-fn ls-fn
			    :id id))
    (:ras  (rank-ant-system :runs runs :iterations iterations :output output 
			    :filename filename
			    :problem-reader #'read-problem-data
			    :cost-function #'symmetric-tsp
			    :rho rho
			    :opt :minimization
			    :n-ants (if n-ants n-ants 0)
			    :ls ls
			    :ls-fn ls-fn
			    :id id))
    (:mmas (max-min-ant-system :runs runs :iterations iterations :output output 
			       :filename filename
			       :problem-reader #'read-problem-data 
			       :cost-function #'symmetric-tsp
			       :restart restart
			       :restart-iterations restart-iterations
			       :avg-cost avg-cost
			       :rho rho
			       :opt :minimization
			       :mmas :mmas
			       :scheduler #'cl-aco::change-update-ant
			       :n-ants (if n-ants n-ants 0)
			       :ls ls
			       :ls-fn ls-fn
			       :id id))))

;;;
;;; problem specific functions and data
;;;

(defun symmetric-tsp (route n distances)
  "Computes the length of a symmetric tsp route."
  (loop 
     for i from 1 to n
     for j from 2 to (1+ n)
     sum (aref distances (aref route i) (aref route j))))


;;;
;;; init problem parameters for TSP
;;; 

(defun read-problem-data (filename parameters)
  "read from file the problem data (still loacked to TSP.)"
  (let ((tsp-instance (cl-tsplib:parse-problem-instance filename)))
    (when (eql parameters nil)
      (setf parameters (make-parameters)))
    (setf (parameters-n parameters)
	  (cl-tsplib:problem-instance-dimension tsp-instance))
    (when (zerop (parameters-n-ants parameters))
      (setf (parameters-n-ants parameters) (cl-tsplib:problem-instance-dimension tsp-instance)))
    (setf (parameters-distances parameters)
	  (cl-tsplib:problem-instance-distance-matrix tsp-instance))
    (setf (parameters-nearest-neighbors parameters)
	  (cl-tsplib:list-nearest-neighbors (parameters-distances parameters)
					    (parameters-n parameters)
					    (parameters-n-neighbors parameters)))
    parameters))

;;;
;;; local search
;;;


(defun 2-opt-tsp (solution parameters)
  (let ((size (parameters-n parameters))
	(distances (parameters-distances parameters))
	(done nil))
    (loop 
       for k from 1 to size
       until done
       do (progn
	    (setf done t)
	    (loop for i from 1 to size 
	       do (loop for j from (+ 2 i) to size
		     do (let ((i+1 (mod (1+ i) size))
			      (j+1 (mod (1+ j) size)))
			  (let ((node-i (aref solution i))
				(node-j (aref solution j))
				(node-i+1 (aref solution i+1))
				(node-j+1 (aref solution j+1)))
			    (when (> (+ (aref distances node-i node-i+1) 
					(aref distances node-j node-j+1))
				     (+ (aref distances node-i node-j)
					(aref distances node-i+1 node-j+1)))
			      (let ((tmp (aref solution i+1)))
				(setf (aref solution i+1) (aref solution j))
				(setf (aref solution j) tmp)
				(reverse-2-opt solution (+ i 2) (- j 1) size)
				(setf done nil)))))))))))

;;;
;;; utils
;;;

(defun verify-tour (tour)
  "Return duplicates nodes in a tour; nil if tour is correct."
  (loop with list = (loop for x across tour collect x)
     for pos across tour
     for n from 0 below (length tour)
     when (member pos (remove-first pos list))
     collect n into duplicates
     finally (return duplicates)))

(defun remove-first (object list)
  (if (null list)
      nil
      (if (eql object (first list))
	  (rest list)
	  (cons (first list) 
		(remove-first object (rest list))))))

(defun verify-visited (n visited)
  (notevery #'(lambda (x) (eql x t))
	    (loop for v from 1 to n 
	       collect (aref visited v))))

(defun verify-incomplete-tour (tour)
   (loop with list = (remove 0 (loop for x across tour collect x))
     for pos across tour
     for n from 0 below (length tour)
     when (member pos (remove-first pos list))
     collect n into duplicates
     finally (return duplicates)))
