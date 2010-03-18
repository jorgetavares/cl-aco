;;;;
;;;; ACO in CL
;;;;

(in-package #:cl-aco)

;;;
;;; data structures
;;;

;;
;; problem and parameters data

(defstruct parameters
  (n 0)
  (distances nil)
  (nearest-neighbors nil)
  (n-neighbors 20)
  (n-ants 51)
  (alpha 1.0)
  (beta 2.0)
  (rho 0.05)
  (max-iterations 2000)
  (ant-system :mmas)
  (avg-cost 426)
  (pheromone-update #'mmas-pheromone-update)
  (decision-rule #'as-decision)
  (eval-tour nil) ;; NOTE: requires generalization
  (lambda 0.05)
  (convergence-function #'branching-factor)
  (stagnation-limit 3)
  (restart t)
  (restart-iterations 250)
  )

(defstruct state 
  (iterations 0)
  (best nil)
  (flag 0)
  (stagnation -1)
  (current-best 0)
  (pop-avg 0)
  (pop-std-dev 0)  
  (bs-update nil)
  (cf 0)
  )


;;
;; representation of ants and ACO data

;; NOTE: the ant structure must be generalized for diferent problems
(defstruct ant
  tour-length
  tour
  visited
  )

(defstruct colony
  n-ants
  ants
  pheromone
  trail-max
  trail-min
  heuristic
  choice-info
  )

(defstruct statistics
  best-ant
  best-iteration
  restart-ant
  (restart-iteration 0)
  (restarts 0)
  (branching 0)
  )

;;;
;;; ACO run modes
;;;

;;
;; run by Ant System

(defun ant-system (&key (runs 1) (iterations 1000) (output :screen) 
		   filename problem-reader cost-function)
  "Ant System standard run."
  (let ((parameters (make-parameters :max-iterations iterations
				     :ant-system :as
				     :pheromone-update #'as-pheromone-update
				     :decision-rule #'as-decision
				     :eval-tour cost-function
				     :restart nil)))
    (run-aco :filename filename :problem-reader problem-reader :cost-function cost-function 
	     :parameters parameters :runs runs :output output :id "as")))

(defun elite-ant-system (&key (runs 1) (iterations 1000) (output :screen) 
			 filename problem-reader cost-function)
  "Elite Ant System standard run."
  (let ((parameters (make-parameters :max-iterations iterations
				     :ant-system :eas
				     :pheromone-update #'eas-pheromone-update
				     :decision-rule #'as-decision
				     :eval-tour cost-function
				     :restart nil)))
    (run-aco :filename filename :problem-reader problem-reader :cost-function cost-function 
	     :parameters parameters :runs runs :output output :id "eas")))

(defun rank-ant-system (&key (runs 1) (iterations 1000) (output :screen) 
			filename problem-reader cost-function)
  "Rank-based Ant System standard run."
  (let ((parameters (make-parameters :max-iterations iterations
				     :ant-system :ras
				     :pheromone-update #'rank-pheromone-update
				     :decision-rule #'as-decision
				     :eval-tour cost-function
				     :restart nil)))
    (run-aco :filename filename :problem-reader problem-reader :cost-function cost-function 
	     :parameters parameters :runs runs :output output :id "ras")))

(defun min-max-ant-system (&key (runs 1) (iterations 1000) (output :screen) 
			   filename problem-reader cost-function)
  "Min-Max Ant System standard run."
  (let ((parameters (make-parameters :max-iterations iterations
				     :ant-system :mmas
				     :pheromone-update #'mmas-pheromone-update
				     :decision-rule #'as-decision
				     :eval-tour cost-function
				     :restart t)))
    (run-aco :filename filename :problem-reader problem-reader :cost-function cost-function 
	     :parameters parameters :runs runs :output output :id "mmas")))

;;
;; generic run functions

(defun run-aco (&key (filename nil) (parameters nil) 
		(runs 1) (output :screen) (id "aco") problem-reader cost-function)
  "Run setup and a number of runs."
  ;; NOTE: read-problem-data must be generalized for different problems
  (let ((params (if filename 
		    (funcall problem-reader filename parameters) 
		    parameters)))
    (when cost-function
      (setf (parameters-eval-tour params) cost-function))
    (loop for run from 1 to runs 
       collect (config-output-run-aco params output run id))))

(defun config-output-run-aco (parameters output run id)
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
		(run-single-aco parameters output (list run-stream best-stream pheromone-stream)))
	      (run-single-aco parameters output (list run-stream best-stream)))))
      (run-single-aco parameters output nil)))

;;;
;;; main ACO loop
;;;

(defun run-single-aco (parameters output streams)
  "Top-level ACO to solve a problem instance."
  (let ((restart-p (parameters-restart parameters))
	(colony (initialize-colony parameters))
	(state (make-state))
	(stats (make-statistics :best-ant (make-ant :tour-length 10000000000)
				:restart-ant (make-ant :tour-length 10000000000))))
    (loop until (terminate parameters state)
       do (progn
	    (construct-solutions parameters colony)
	    (update-statistics parameters colony stats state output streams)
	    (update-convergence-factor parameters colony state stats)
	    (when restart-p
	      (case (parameters-ant-system parameters)
		(:mmas
		 (restart-pheromone-trails-mmas parameters colony state stats))
		(:mmas-hcf
		 (restart-pheromone-trails-mmas-hcf parameters colony state stats))))
	    (update-trails parameters colony stats state)
	    (increment-iteration state)
	    (output-state output state stats colony streams)))
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

(defun update-statistics (parameters colony stats state output streams)
  "Update all the ACO stats."
  (loop with best = 10000000000 
     for ant across (colony-ants colony)
     do (when (< (ant-tour-length ant) best)
	  (setf best (ant-tour-length ant))
	  (when (< best (ant-tour-length (statistics-best-ant stats)))
	    (setf (statistics-best-ant stats) (copy-ant ant)
		  (statistics-best-iteration stats) (state-iterations state))
	    (when (member output '(:full :files :screen+files))
	      (format (second streams) "~a~%" (list 
					       (state-iterations state) 
					       (statistics-best-ant stats))))
	    (when (eql (parameters-ant-system parameters) :mmas)
	      (update-trail-limits parameters colony best)))
	  (when (< best (ant-tour-length (statistics-restart-ant stats)))
	    (setf (statistics-restart-ant stats) (copy-ant ant)
		  (statistics-restart-iteration stats) (state-iterations state))))
     sum (ant-tour-length ant) into total
     finally (progn
	       (setf (state-pop-avg state) (/ total (colony-n-ants colony)))
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
