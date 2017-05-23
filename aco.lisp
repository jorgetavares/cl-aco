;;;;
;;;; ACO in CL
;;;;

(in-package #:cl-aco)

;;;
;;; ACO run modes
;;;

;;
;; run by Ant System

(defun ant-system (&key (runs 1) (iterations 1000) (output :screen) 
		   filename problem-reader cost-function rho opt n-ants ls ls-fn (id "as"))
  "Ant System standard run."
  (let ((parameters (make-parameters :max-iterations iterations
				     :ant-system :as
				     :pheromone-update #'as-pheromone-update
				     :decision-rule #'as-decision
				     :eval-tour cost-function
				     :restart nil
				     :rho rho
				     :optimization opt
				     :n-ants (if n-ants n-ants 0)
				     :local-search ls
				     :local-search-method ls-fn)))
    (run-aco :filename filename :problem-reader problem-reader :cost-function cost-function 
	     :parameters parameters :runs runs :output output :id id)))

(defun elite-ant-system (&key (runs 1) (iterations 1000) (output :screen) 
			 filename problem-reader cost-function rho opt n-ants ls ls-fn (id "eas"))
  "Elite Ant System standard run."
  (let ((parameters (make-parameters :max-iterations iterations
				     :ant-system :eas
				     :pheromone-update #'eas-pheromone-update
				     :decision-rule #'as-decision
				     :eval-tour cost-function
				     :restart nil
				     :rho rho
				     :optimization opt
				     :n-ants (if n-ants n-ants 0)
				     :local-search ls
				     :local-search-method ls-fn)))
    (run-aco :filename filename :problem-reader problem-reader :cost-function cost-function 
	     :parameters parameters :runs runs :output output :id id)))

(defun rank-ant-system (&key (runs 1) (iterations 1000) (output :screen) 
			filename problem-reader cost-function rho opt n-ants ls ls-fn (id "ras"))
  "Rank-based Ant System standard run."
  (let ((parameters (make-parameters :max-iterations iterations
				     :ant-system :ras
				     :pheromone-update #'rank-pheromone-update
				     :decision-rule #'as-decision
				     :eval-tour cost-function
				     :restart nil
				     :rho rho
				     :optimization opt
				     :n-ants (if n-ants n-ants 0)
				     :local-search ls
				     :local-search-method ls-fn)))
    (run-aco :filename filename :problem-reader problem-reader :cost-function cost-function 
	     :parameters parameters :runs runs :output output :id id)))

(defun max-min-ant-system (&key (runs 1) (iterations 1000) (output :screen) 
			   filename problem-reader cost-function
			   (restart t) (restart-iterations 250) avg-cost rho opt mmas 
			   (scheduler #'change-update-ant) n-ants ls ls-fn (id "mmas"))
  "Min-Max Ant System standard run."
  (let ((parameters (make-parameters :max-iterations iterations
				     :ant-system mmas
				     :pheromone-update #'mmas-pheromone-update
				     :decision-rule #'as-decision
				     :eval-tour cost-function
				     :restart restart
				     :restart-iterations restart-iterations
				     :avg-cost avg-cost
				     :rho rho
				     :optimization opt
				     :scheduler scheduler
				     :n-ants (if n-ants n-ants 0)
				     :local-search ls
				     :local-search-method ls-fn)))
     (run-aco :filename filename :problem-reader problem-reader :cost-function cost-function 
	      :parameters parameters :runs runs :output output :id id)))

;;
;; generic run functions

(defun run-aco (&key (run-aco-engine #'run-single-aco) (filename nil) (parameters nil) 
		(runs 1) (output :screen) (id "aco") problem-reader cost-function)
  "Run setup and a number of runs."
  ;; NOTE: read-problem-data must be generalized for different problems
  (let ((params (if filename 
		    (funcall problem-reader filename parameters) 
		    parameters)))
    (when cost-function
      (setf (parameters-eval-tour params) cost-function))
    (loop for run from 1 to runs 
	  collect (config-output-run-aco params output run id :run-aco-engine run-aco-engine))))

(defun config-output-run-aco (parameters output run id &key (run-aco-engine #'run-single-aco))
  "Start an ACO run with or without saving data to files, and/or displaying on screen."
  (if (member output '(:files :screen+files :full))
      (with-open-file (run-stream 
		       (concatenate 'string id "-run" (format nil "~D" run) ".txt")
		       :direction :output :if-exists :supersede)
	(with-open-file (best-stream 
			 (concatenate 'string id "-best" (format nil "~D" run) ".txt")
			 :direction :output :if-exists :supersede)
	  (if (eql output :full)
	      (with-open-file (pheromone-stream 
			       (concatenate 'string id "-pheromone" (format nil "~D" run) ".txt")
			       :direction :output :if-exists :supersede)
		(funcall run-aco-engine parameters output (list run-stream best-stream pheromone-stream)))
	      (funcall run-aco-engine parameters output (list run-stream best-stream)))))
      (funcall run-aco-engine parameters output nil)))

;;;
;;; main ACO loop
;;;

(defun run-single-aco (parameters output streams)
  "Top-level ACO to solve a problem instance."
  (let* ((restart-p (parameters-restart parameters))
	 (state (make-state))
	 (colony (initialize-colony parameters state))
	 (optimization (parameters-optimization parameters))
	 (worst (if (eql optimization :minimization)
		    most-positive-fixnum
		    most-negative-fixnum))
	 (cmp (if (eql optimization :minimization) #'< #'>))
	 (stats (make-statistics :best-ant (make-ant :tour-length worst)
				 :restart-ant (make-ant :tour-length worst)))
	 (use-local-search (parameters-local-search parameters))
	 (local-search (parameters-local-search-method parameters))
	 (restart-fn (parameters-restart-fn parameters)))
    (loop until (terminate parameters state)
	  do (progn
	       (construct-solutions parameters colony)
	       (when use-local-search
		 (apply-local-search local-search parameters colony))
	       (update-statistics parameters colony stats state output streams worst cmp)
	       (update-convergence-factor parameters colony state stats)
	       (when restart-p
		 (case (parameters-ant-system parameters)
		   (:mmas
		    (restart-pheromone-trails-mmas parameters colony state stats worst))
		   (otherwise
		    (funcall restart-fn parameters colony state stats cmp worst))))
	       (update-trails parameters colony stats state)
	       ;(update-statistics parameters colony stats state output streams worst cmp)
	       ;(update-convergence-factor parameters colony state stats)
	       (increment-iteration state)
	       (output-state output state stats colony streams)))
    (setf (statistics-ants-solutions stats)
	  (loop for ant across (colony-ants colony)
		collect (ant-tour-length ant)))
    stats))


;;;
;;; simulation control
;;;

(defun terminate (parameters state)
  "Checks if the terminal condition is met."
  (cond ((= (parameters-max-iterations parameters)
	    (state-iterations state)) t)
	(t nil)))

(defun increment-iteration (state &optional (step 1))
  "Increments by step the number of iterations."
  (incf (state-iterations state) step))

(defun update-statistics (parameters colony stats state output streams worst cmp)
  "Update all the ACO stats."
  (loop with best = worst
	for ant across (colony-ants colony)
	do (when (funcall cmp (ant-tour-length ant) best)
	     (setf best (ant-tour-length ant))
	     (when (funcall cmp best (ant-tour-length (statistics-best-ant stats)))
	       (setf (statistics-best-ant stats) (safe-copy-ant ant)
		     (statistics-best-iteration stats) (state-iterations state))
	       (when (member output '(:full :files :screen+files))
		 (format (second streams) "~a~%" (list 
						  (state-iterations state) 
						  (statistics-best-ant stats))))
	       (when (member (parameters-ant-system parameters) '(:mmas :self))
		 (update-trail-limits parameters state colony best))
	       (when (funcall cmp best (ant-tour-length (statistics-restart-ant stats)))
		 (setf (statistics-restart-ant stats) (safe-copy-ant ant)
		       (statistics-restart-iteration stats) (state-iterations state)))
	       ))
	do (when (funcall cmp best (ant-tour-length (statistics-restart-ant stats)))
	     (setf (statistics-restart-ant stats) (safe-copy-ant ant)
		   (statistics-restart-iteration stats) (state-iterations state)))
	sum (ant-tour-length ant) into total
	finally (progn
		  (setf (state-pop-avg state) (/ total (colony-n-ants colony)))
		  (setf (statistics-pop-avg stats) (float (state-pop-avg state)))
		  (setf (state-pop-std-dev state) (std-dev (state-pop-avg state) colony))
		  (setf (state-current-best state) best)
		  )))

;;;
;;; output data
;;;

(defun output-state (output state stats colony streams)
  "Prints simulation information."
  (unless (eql output :none)
    (let ((iteration (state-iterations state)) 
	  (best (float (ant-tour-length (statistics-best-ant stats))))
	  (restart-best (float (ant-tour-length (statistics-restart-ant stats))))
	  (restarts  (statistics-restarts stats))
	  (current-best (float (state-current-best state))) 
	  (avg (float (state-pop-avg state)))
	  (cf (float (state-stagnation state)))
	  (min (float (colony-trail-min colony)))
	  (max (float (colony-trail-max colony))))
      (when (member output '(:screen :screen+files :full))
	(format t "~a | b: ~a | r: ~a ~a | c: ~a | avg: ~6,4F | cf: ~6,4F | trails: ~a ~a ~%"  
		iteration best restart-best restarts current-best avg cf min max))
      (when (member output '(:files :screen+files :full))
	(format (first streams)  "~a~%" 
		(list iteration best restart-best restarts current-best avg cf min max)))
      (when (member output '(:full))
	(format (third streams) "~a~%" (list (colony-pheromone colony))))
      )))
