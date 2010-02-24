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
  (max-iterations 20000)
  (ant-system :mmas)
  (avg-cost 426)
  (pheromone-update #'mmas-pheromone-update)
  (decision-rule #'as-decision)
  (eval-tour #'symmetric-tsp)
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
  final-pheromones 
  best-ant
  best-iteration
  restart-ant
  (restart-iteration 0)
  (restarts 0)
  (branching 0)
  )


;;;
;;; problem specific functions and data
;;;

(defparameter eil51 "/Users/jast/workspace/cl-tsplib/instances/eil51.tsp")

(defparameter *tsp-parameters* nil)
(defparameter *tsp-colony* nil)

(defun symmetric-tsp (route n distances)
  "Computes the length of a symmetric tsp route."
  (loop 
     for i from 1 to n
     for j from 2 to (1+ n)
     sum (aref distances (aref route i) (aref route j))))


;;;
;;; ACO run modes
;;;

(defun aco-tsp (tsp-filename &key (runs 1) (output t))
  "Run setup and a number of runs."
  (multiple-value-bind (parameters colony)
      (setup-aco-tsp tsp-filename)
    (run-multiple-aco-tsp parameters colony runs output)))

(defun run (&optional (runs 1))
  (let ((stats (aco-tsp eil51 :runs runs :output nil)))
    (loop with best = 10000000 
       for s in stats
       collect (ant-tour-length (statistics-best-ant s)) into results
       when (< (ant-tour-length (statistics-best-ant s)) best) 
       do (setf best (ant-tour-length (statistics-best-ant s)))
       sum (ant-tour-length (statistics-best-ant s)) into total
       finally (return (list best (float (/ total runs)) results)))))


;;;
;;; ACO setup
;;;

(defun setup-aco-tsp (tsp-filename)
  "Setup the data for ACO."
  (multiple-value-bind (parameters colony)
      (read-prepare-data tsp-filename)
    (setf *tsp-parameters* parameters *tsp-colony* colony)
    (values parameters colony)))

(defun run-multiple-aco-tsp (parameters colony &optional (runs 1) (output t))
  "Run multiple runs of ACO; data is loaded." 
  (loop repeat runs
     collect (run-single-aco-tsp parameters colony output)))


;;;
;;; main ACO loop
;;;

(defun run-single-aco-tsp (parameters default-colony output-p)
  "Top-level ACO to solve a TSP instance."
  (let ((restart-p (parameters-restart parameters))
	(colony (initialize-colony default-colony parameters))
	(state (make-state))
	(stats (make-statistics :best-ant (make-ant :tour-length 10000000000)
				:restart-ant (make-ant :tour-length 10000000000))))
    (loop until (terminate parameters state)
       do (progn
	    (construct-solutions parameters colony)
	    (update-statistics parameters colony stats state)
	    (update-convergence-factor parameters colony state stats)
	    (when restart-p
	      (case (parameters-ant-system parameters)
		(:mmas
		 (restart-pheromone-trails-mmas parameters colony state stats))
		(:mmas-hcf
		 (restart-pheromone-trails-mmas-hcf parameters colony state stats))))
	    (update-trails parameters colony stats state)
	    (increment-iteration state)
	    (when output-p
	      (output-state state stats colony))))
    ;(setf (statistics-final-pheromones stats) (colony-pheromone colony))
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

(defun update-statistics (parameters colony stats state)
  "Update all the ACO stats."
  (loop with best = 10000000000 
     for ant across (colony-ants colony)
     do (when (< (ant-tour-length ant) best)
	  (setf best (ant-tour-length ant))
	  (when (< best (ant-tour-length (statistics-best-ant stats)))
	    (setf (statistics-best-ant stats) (safe-copy-ant ant)
		  (statistics-best-iteration stats) (state-iterations state))
	    (when (eql (parameters-ant-system parameters) :mmas)
	      (update-trail-limits parameters colony best)))
	  (when (< best (ant-tour-length (statistics-restart-ant stats)))
	    (setf (statistics-restart-ant stats) (safe-copy-ant ant)
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

(defun output-state (state stats colony)
  "Prints simulation information."
  (format t "~a | b: ~a | r: ~a ~a | c: ~a | avg: ~6,4F | cf: ~6,4F | trails: ~a ~a ~%"  
	  (state-iterations state) 
	  (float (ant-tour-length (statistics-best-ant stats)))
	  (float (ant-tour-length (statistics-restart-ant stats)))
	  (statistics-restarts stats)
	  (float (state-current-best state)) 
	  (float (state-pop-avg state))
	  (float (state-stagnation state))
	  (float (colony-trail-min colony))
	  (float (colony-trail-max colony))
	  ))
