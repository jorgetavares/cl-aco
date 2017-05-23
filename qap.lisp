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
(defparameter *nug14*  "/Users/jast/workspace/datasets/qaplib/instances/nug14.dat")
(defparameter *nug15*  "/Users/jast/workspace/datasets/qaplib/instances/nug15.dat")
(defparameter *nug17*  "/Users/jast/workspace/datasets/qaplib/instances/nug17.dat")
(defparameter *nug18*  "/Users/jast/workspace/datasets/qaplib/instances/nug18.dat")
(defparameter *nug20*  "/Users/jast/workspace/datasets/qaplib/instances/nug20.dat")

(defparameter *tai20b* "/Users/jast/workspace/datasets/qaplib/instances/tai20b.dat")

;;;
;;; running aco for QAP
;;;

(defun aco-qap (&key 
		(ant-system :mmas) (filename *nug12*) (runs 1) (iterations 100) 
		(output :screen) (restart nil) (restart-iterations 100) (avg-cost 600) (rho 0.8) 
		n-ants (ls t) (ls-fn #'2-opt-qap) (id "acoqap"))
  "Launch an ACO system for QAP."
  (case ant-system
    (:as (ant-system :runs runs :iterations iterations :output output 
		     :filename filename
		     :problem-reader #'qap-data-reader
		     :cost-function #'qap-fitness
		     :rho rho
		     :opt :minimization
		     :n-ants (if n-ants n-ants 0)
		     :ls ls
		     :ls-fn ls-fn
		     :id id))
    (:eas (elite-ant-system :runs runs :iterations iterations :output output 
			    :filename filename
			    :problem-reader #'qap-data-reader
			    :cost-function #'qap-fitness
			    :rho rho
			    :opt :minimization
			    :n-ants (if n-ants n-ants 0)
			    :ls ls
			    :ls-fn ls-fn
			    :id id))
    (:ras  (rank-ant-system :runs runs :iterations iterations :output output 
			    :filename filename
			    :problem-reader #'qap-data-reader 
			    :cost-function #'qap-fitness
			    :rho rho
			    :opt :minimization
			    :n-ants (if n-ants n-ants 0)
			    :ls ls
			    :ls-fn ls-fn
			    :id id))
    (:mmas (max-min-ant-system :runs runs :iterations iterations :output output 
			       :filename filename
			       :problem-reader #'qap-data-reader
			       :cost-function #'qap-fitness
			       :restart restart
			       :restart-iterations restart-iterations
			       :avg-cost avg-cost
			       :rho rho
			       :opt :minimization
			       :mmas :mmas
			       :scheduler #'change-update-qap
			       :n-ants (if n-ants n-ants 0)
			       :ls ls
			       :ls-fn ls-fn
			       :id id))))

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
    (when (zerop (parameters-n-ants parameters))
      (setf (parameters-n-ants parameters) (cl-qaplib:size *qap-instance*)))
    (setf (parameters-distances parameters) 
	  (make-array `(,(1+ (parameters-n parameters)) ,(1+ (parameters-n parameters))) 
		      :initial-element 0))
    (let ((matrix-e (compute-heuristic-information qap-instance)))
      (loop for i from 1 to (parameters-n parameters)
	 do (loop for j from 1 to (parameters-n parameters)
	       do (setf (aref (parameters-distances parameters) i j)
			(aref matrix-e (1- i) (1- j))))))
    (setf (parameters-nearest-neighbors parameters) nil)
    parameters))


;;;
;;; scheduler
;;;


(defun change-update-qap (parameters state)
  (declare (ignore parameters))
  (if (zerop (mod (state-iterations state) 5)) nil t))


;;;
;;; heuristic information
;;;

(defun compute-heuristic-information (instance)
  (let* ((size (size instance))
	 (vector-d (make-array size :initial-element 0))
	 (vector-f (make-array size :initial-element 0))
	 (matrix-e (make-array `(,size ,size) :initial-element 0)))
    (flet ((do-sum (num)
	     (let ((sd 0) (sf 0))
	       (loop for j below size
		  do (unless (= num j)
		       (setf sd (+ sd (ref-distances instance num j)))
		       (setf sf (+ sf (ref-flows instance num j))))
		  finally (return (values sd sf))))))
      (loop for i below size
	 do (multiple-value-bind (sum-d sum-f)
		(do-sum i)
	      (progn
		(setf (aref vector-d i) sum-d)
		(setf (aref vector-f i) sum-f)))))
    (loop for i below size
       do (loop for j below size
	     do (setf (aref matrix-e i j) 
		      (* (aref vector-f i) 
			 (aref vector-d j)))))
    matrix-e))
	

;;;
;;; local search
;;;

(defun 2-opt-qap (solution parameters)
  (let ((size (parameters-n parameters))
	(fitness (parameters-eval-tour parameters))
	(done nil))
    (loop 
       for k from 1 to size
       until done
       do (progn
	    (setf done t)
	    (loop for i from 1 to size 
	       do (loop for j from (+ 2 i) to size
		     do (let ((i+1 (mod (1+ i) size))
			      (swaped (copy-seq solution)))
			  (rotatef (aref swaped i+1) 
				   (aref swaped j))
			  (when (> (funcall fitness solution size nil)
				   (funcall fitness swaped size nil))
			    (let ((tmp (aref solution i+1)))
			      (setf (aref solution i+1) (aref solution j))
			      (setf (aref solution j) tmp)
			      (reverse-2-opt solution (+ i 2) (- j 1) size)
			      (setf done nil))))))))))
