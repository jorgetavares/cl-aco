;;;;
;;;; ACO for MKP
;;;; - specific code for MKP using :cl-aco 
;;;; - NOTE: at this point, :cl-aco is directly connected to 
;;;;   the TSP problem and is not suficient generalized to be 
;;;;   used with other problems. For the MKP we use the 
;;;;   strategy of using a permutation to indicate the order
;;;;   of objects that should be put in the knapsacks. This
;;;;   way the differences to the TSP are not that large.
;;;;

(in-package #:cl-aco-mkp)

;;;
;;; handles for the specific problem instances
;;;

(defparameter *ccp01* "~/datasets/knapsacklib/mkp/ccp/p1.txt")
(defparameter *ccp02* "~/datasets/knapsacklib/mkp/ccp/p2.txt")
(defparameter *ccp03* "~/datasets/knapsacklib/mkp/ccp/p3.txt")
(defparameter *ccp04* "~/datasets/knapsacklib/mkp/ccp/p4.txt")
(defparameter *ccp05* "~/datasets/knapsacklib/mkp/ccp/p5.txt")
(defparameter *ccp06* "~/datasets/knapsacklib/mkp/ccp/p6.txt")
(defparameter *ccp07* "~/datasets/knapsacklib/mkp/ccp/p7.txt")

(defparameter *gk01*  "~/datasets/knapsacklib/mkp/gk/mk_gk01.txt")


;;;
;;; running aco for MKP
;;;

(defun deposit-pheromone (ant n pheromone weight)
  "Deposit pheromone in the trail according to AS method."
  (let ((delta (*  weight (/ 1 (ant-tour-length ant))))) ;; new form
    (loop for i from 1 to n
       do (let* ((j (aref (ant-tour ant) i))
		 (l (aref (ant-tour ant) (1+ i)))
		 (value (+ (aref pheromone j l) delta)))
	    (setf (aref pheromone j l) value
		  (aref pheromone l j) value)))))

(defun aco-mkp (&key 
		(ant-system :as) (filename *ccp01*) (type :ccp) (runs 1) (iterations 10) 
		(output :screen) (restart nil) (restart-iterations 250) (avg-cost 10000) (rho 0.5))
  "Launch an ACO system for MKP."
  (case ant-system
    (:as (ant-system :runs runs :iterations iterations :output output 
		     :filename filename
		     :problem-reader (make-reader type)
		     :cost-function #'mkp-fitness
		     :rho rho
		     :opt :maximization))
    (:eas (elite-ant-system :runs runs :iterations iterations :output output 
			    :filename filename
			    :problem-reader (make-reader type)
			    :cost-function #'mkp-fitness
			    :rho rho
			    :opt :maximization))
    (:ras  (rank-ant-system :runs runs :iterations iterations :output output 
			    :filename filename
			    :problem-reader (make-reader type) 
			    :cost-function #'mkp-fitness
			    :rho rho
			    :opt :maximization))
    (:mmas (max-min-ant-system :runs runs :iterations iterations :output output 
			       :filename filename
			       :problem-reader (make-reader type) 
			       :cost-function #'mkp-fitness
			       :restart restart
			       :restart-iterations restart-iterations
			       :avg-cost avg-cost
			       :rho rho
			       :opt :maximization
			       :mmas :mmas))))

;;;
;;; problem specific functions and data
;;;

(defparameter *mkp-n* nil)
(defparameter *mkp-m* nil)
(defparameter *mkp-c* nil)
(defparameter *mkp-p* nil)
(defparameter *mkp-r* nil)

(defparameter *pmax* nil)
(defparameter *rmin* nil)

(defun set-pmax ()
  (setf *pmax* (apply #'max (coerce *mkp-p* 'list))))

(defun set-rmin ()
  (setf *rmin* (apply #'min (remove 0 (loop for i from 0 below *mkp-n*
					 append (loop for j from 0 below *mkp-m*
						   collect (aref *mkp-r* i j)))))))
(defun CV (x i)
  (max 0 (- (loop for j from 0 below *mkp-n*
	       sum (* (aref *mkp-r* j i) 
		      (aref x j))) 
	    (aref *mkp-c* i))))

(defun penalty (x)
  (* (/ (+ *pmax* 1) *rmin*)
     (apply #'max (loop for i from 0 below *mkp-m*
		     collect (CV x i)))))

(defun f (x)
  (loop for j from 0 below *mkp-n*
     sum (* (aref *mkp-p* j) 
	    (aref x j)))) 

(defun fitness-binary (x)
  (- (f x) (penalty x)))

(defun fitness-permutation (x)
  (fitness-binary (permutation-binary x)))

;;
;; representation conversions

(defun permutation-binary (x) ;; x has starts at index 1!!
  (let ((bin-x (make-array *mkp-n* :initial-element 0)))
    (loop for i from 1 to *mkp-n*
       do (let ((j (1- (aref x i)))) 
	    (setf (aref bin-x j) 1)
	    (unless (check-constraints bin-x)
	      (setf (aref bin-x j) 0))) 
       finally (return bin-x))))

(defun check-constraints (bin-x)
  (= 0 (loop for i from 0 below *mkp-m*
	  sum (max 0 (- (loop for j from 0 below *mkp-n*
			   sum (* (aref *mkp-r* j i)
				  (aref bin-x j)))
			(aref *mkp-c* i))))))


;;;
;;; AS fitness function
;;;

(defun mkp-fitness (solution n matrix)
  (declare (ignore n matrix))
  (fitness-permutation solution))


;;;
;;; init problem parameters for MKP
;;; 

(defun make-reader (dataset-type)
  #'(lambda (filename parameters)
      (let ((mkp-instance (cl-knapsacklib:parse-mkp filename :dataset dataset-type)))
	(when (eql parameters nil)
	  (setf parameters (make-parameters)))
	(setf *mkp-n* (cl-knapsacklib:objects-number mkp-instance))
	(setf *mkp-m* (cl-knapsacklib:knapsacks-number mkp-instance))
	(setf *mkp-c* (cl-knapsacklib:capacities mkp-instance))
	(setf *mkp-p* (cl-knapsacklib:profits mkp-instance))
	(setf *mkp-r* (cl-knapsacklib:constraints mkp-instance))
	(setf *pmax* (set-pmax))
	(setf *rmin* (set-rmin))
	(setf (parameters-n parameters) *mkp-n*)
	(setf (parameters-distances parameters) ;; deactivates the heuristic component in the AS
	       (make-array `(,(1+ *mkp-n*) ,(1+ *mkp-n*)) 
			   :initial-element 1.0
			   :element-type 'single-float))
	(setf (parameters-nearest-neighbors parameters) nil)
	parameters)))
