;;;;
;;;; ACO for QAP
;;;; - specific code for QAP using :cl-aco 
;;;; - NOTE: at this point, :cl-aco is directly connected to 
;;;;   the TSP problem and is not suficient generalized to be 
;;;;   used with other problems. 
;;;;

(in-package #:cl-aco-qap)

;;;
;;; handles for the specific problem instances
;;;

(defparameter *nug12*  "/Users/jast/workspace/datasets/qaplib/instances/nug12.dat")
(defparameter *bur26h* "/Users/jast/workspace/datasets/qaplib/instances/bur26h.dat")

;;;
;;; running aco for QAP
;;;

(defun aco-qap (&key 
		(ant-system :as) (filename *nug12*) (runs 1) (iterations 10) 
		(output :screen) (restart nil) (restart-iterations 250) (avg-cost 100) (rho 0.5))
  "Launch an ACO system for QAP."
  (case ant-system
    (:as (ant-system :runs runs :iterations iterations :output output 
		     :filename filename
		     :problem-reader #'qap-data-reader
		     :cost-function #'qap-fitness
		     :rho rho
		     :opt :minimization))
    (:eas (elite-ant-system :runs runs :iterations iterations :output output 
			    :filename filename
			    :problem-reader #'qap-data-reader
			    :cost-function #'qap-fitness
			    :rho rho
			    :opt :minimization))
    (:ras  (rank-ant-system :runs runs :iterations iterations :output output 
			    :filename filename
			    :problem-reader #'qap-data-reader 
			    :cost-function #'qap-fitness
			    :rho rho
			    :opt :minimization))
    (:mmas (min-max-ant-system :runs runs :iterations iterations :output output 
			       :filename filename
			       :problem-reader #'qap-data-reader
			       :cost-function #'qap-fitness
			       :restart restart
			       :restart-iterations restart-iterations
			       :avg-cost avg-cost
			       :rho rho
			       :opt :minimization
			       :mmas :mmas))))

;;;
;;; problem specific functions and data
;;;

(defparameter *qap-instance* nil)

(defun qap-fitness (solution n matrix)
  (declare (ignore matrix))
  (loop for i below n
     sum (loop for j below n
	    sum (* (ref-distances *qap-instance* i j)
		   (ref-flows *qap-instance* 
			      (1- (aref solution (1+ i)))
			      (1- (aref solution (1+ j))))))))

;;;
;;; init problem parameters for QAP
;;; 

(defun qap-data-reader (filename parameters)
  (let ((qap-instance (cl-qaplib:parse-instance filename)))
    (setf *qap-instance* qap-instance)
    (when (eql parameters nil)
      (setf parameters (make-parameters)))
    (setf (parameters-n parameters) (cl-qaplib:size *qap-instance*))
    (setf (parameters-distances parameters) ;; deactivates the heuristic component in the AS
	  (make-array `(,(1+ (parameters-n parameters)) ,(1+ (parameters-n parameters))) 
		      :initial-element 1.0
		      :element-type 'single-float))
    (setf (parameters-nearest-neighbors parameters) nil)
    parameters))
