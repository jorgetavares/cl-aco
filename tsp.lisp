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

(defun aco-tsp (&key (ant-system :as) (filename eil51) (runs 1) (iterations 100) (output :screen))
  "Launch an ACO system for TSP."
  (case ant-system
    (:as (ant-system :runs runs :iterations iterations :output output 
		     :filename filename
		     :problem-reader #'read-problem-data 
		     :cost-function #'symmetric-tsp))
    (:eas (elite-ant-system :runs runs :iterations iterations :output output 
			    :filename filename
			    :problem-reader #'read-problem-data 
			    :cost-function #'symmetric-tsp))
    (:ras  (rank-ant-system :runs runs :iterations iterations :output output 
			    :filename filename
			    :problem-reader #'read-problem-data
			    :cost-function #'symmetric-tsp))
    (:mmas (min-max-ant-system :runs runs :iterations iterations :output output 
			       :filename filename
			       :problem-reader #'read-problem-data 
			       :cost-function #'symmetric-tsp))
    ))


;;;
;;; problem specific functions and data
;;;

(defparameter eil51 "/Users/jast/workspace/cl-tsplib/instances/eil51.tsp")
(defparameter burma14 "/Users/jast/workspace/cl-tsplib/instances/burma14.tsp")


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
    (setf (parameters-distances parameters)
	  (cl-tsplib:problem-instance-distance-matrix tsp-instance))
    (setf (parameters-nearest-neighbors parameters)
	  (cl-tsplib:list-nearest-neighbors (parameters-distances parameters)
					    (parameters-n parameters)
					    (parameters-n-neighbors parameters)))
    parameters))


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