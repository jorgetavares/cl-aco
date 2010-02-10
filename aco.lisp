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
  (max-iterations 1000)
  (ant-system :mmas)
  (avg-cost 500)
  (pheromone-update #'mmas-pheromone-update)
  (decision-rule #'as-decision)
  (eval-tour #'symmetric-tsp)
  )

(defstruct state 
  (iterations 0)
  (best nil)
  (flag 0)
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
  )

;;;
;;; main functions
;;;

(defun safe-copy-ant (ant)
  (make-ant
   :tour-length (ant-tour-length ant)
   :tour (copy-seq (ant-tour ant))
   :visited (copy-seq (ant-visited ant))))

(defparameter eil51 "/Users/jast/workspace/cl-tsplib/instances/eil51.tsp")


;;
;; high-level ACO algorithm for the TSP

(defparameter *tsp-parameters* nil)
(defparameter *tsp-colony* nil)

(defun aco-tsp (tsp-filename &key (runs 1) (output t))
  "Run setup and a number of runs."
  (multiple-value-bind (parameters colony)
      (setup-aco-tsp tsp-filename)
    (run-multiple-aco-tsp parameters colony runs output)))

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

(defun run-single-aco-tsp (parameters default-colony output-p)
  "Top-level ACO to solve a TSP instance."
  (let ((colony (initialize-colony default-colony parameters))
	(state (make-state))
	(stats (make-statistics :best-ant (make-ant :tour-length 10000000000))))
    (loop until (terminate parameters state)
       do (progn
	    (construct-solutions parameters colony)
	    (let ((out (update-statistics colony stats state)))
	      (increment-iteration state)
	      (update-trails parameters colony stats state)
	      (when output-p
		(output-state state out)))))
    ;(setf (statistics-final-pheromones stats) (colony-pheromone colony))
    stats))


;;;
;;; initialize data
;;;

(defun initialize-colony (colony parameters)
  "Initializes all the data for a ACO run."
  (setf (colony-ants colony)
	(init-ants (parameters-n-ants parameters)
		   (parameters-n parameters)))
  (setf (colony-pheromone colony)
	(init-pheromone (parameters-n parameters) 
			(colony-trail-max colony)))
  (setf (colony-choice-info colony)
	(init-choice-info (parameters-n parameters)
			  (colony-pheromone colony)
			  (colony-heuristic colony)
			  (parameters-alpha parameters)
			  (parameters-beta parameters)))
  colony)

(defun read-prepare-data (tsp-filename)
  "Reads data from files (instances and parameters) and computes necessary pre-run stuff."
  (let* ((tsp-instance (cl-tsplib:parse-problem-instance tsp-filename))
	 (parameters (make-parameters
		      :n (cl-tsplib:problem-instance-dimension tsp-instance)
		      :distances (cl-tsplib:problem-instance-distance-matrix tsp-instance)))
	 (trail-max (update-trail-max-value parameters (parameters-avg-cost parameters))) 
	 (colony (make-colony :n-ants (parameters-n-ants parameters)
			      :ants (init-ants (parameters-n-ants parameters)
					       (parameters-n parameters))
			      :pheromone (init-pheromone (parameters-n parameters) trail-max)
			      :trail-max trail-max
			      :trail-min (update-trail-min-value parameters trail-max)
			      :heuristic (init-heuristic (parameters-n parameters)
							 (parameters-distances parameters)))))
    (setf (parameters-nearest-neighbors parameters)
	  (list-nearest-neighbors (parameters-distances parameters)
				  (parameters-n parameters)
				  (parameters-n-neighbors parameters)))
    (values parameters colony)))

(defun list-nearest-neighbors (distances n &optional (n-neighbors (- n 1)))
  "Returns the list of the nearest enighbors for a set of cities."
  (let ((nearest-list (make-array `(,(1+ n) ,(1+ n-neighbors)) :initial-element 0))
	(all-neighbors (compute-nearest-neighbors distances n)))
    (loop 
       for i from 1 to n
       for neighbors in all-neighbors
       do (loop for j from 1 to n-neighbors
	     do (setf (aref nearest-list i j) 
		      (nth j neighbors)))
       finally (return nearest-list))))
    
(defun compute-nearest-neighbors (distances n)
  "Computes all the nearest neighbors given a distance matrix."
  (loop with indexes = (loop for i from 1 to n collect i) 
     for i from 1 to n
     collect (loop for j from 0 to n
		collect (aref distances i j) into distances-list
		finally (return (remove i (sort (copy-list indexes) #'< 
						:key #'(lambda (x) 
							 (nth x distances-list))))))))

(defun update-trail-max-value (parameters cost)
  "Return initial pheromone value according to the Ant System."
  (case (parameters-ant-system parameters)
    (:mmas (/ 1 (* (parameters-rho parameters) cost)))
    (otherwise 10)))

(defun update-trail-min-value (parameters trail-max)
  "Return initial lower bound pheromone value according to the Ant System."
  (case (parameters-ant-system parameters)
    (:mmas (/ trail-max (* 2 (parameters-n parameters))))
    (otherwise 10)))

(defun init-pheromone (n &optional (value 10))
  "Return a fresh pheromone matrix."
  (make-array `(,(1+ n) ,(1+ n)) :initial-element value))
  
(defun init-heuristic (n distances)
  "Return a fresh heuristic info matrix."
  (let ((matrix (make-array `(,(1+ n) ,(1+ n)) :initial-element 0)))
    (loop for i from 1 to n
       do (loop for j from 1 to n
	     do (unless (= i j)
		  (setf (aref matrix i j)
			(/ 1 (aref distances i j)))))
       finally (return matrix))))

(defun init-choice-info (n pheromone heuristic alpha beta)
  "Return a fresh choice-info matrix."
  (let ((matrix (make-array `(,(1+ n) ,(1+ n)) :initial-element 0)))
    (loop for i from 1 to n
       do (loop for j from 1 to n
	     do (setf (aref matrix i j)
		      (* (expt (aref pheromone i j) alpha) 
			 (expt (aref heuristic i j) beta))))
       finally (return matrix))))

(defun init-ants (n-ants n)
  "Return a fresh array of ants for the optimization procedure, i.e., the colony."
  (let ((ants (make-array n-ants)))
    (loop for i from 0 below n-ants
	 do (setf (aref ants i)
		  (make-ant :tour-length nil
			    :tour (make-array (+ n 2) :initial-element 0)
			    :visited (make-array (+ n 2) :initial-element nil)))
       finally (return ants))))


;;;
;;; state simulation control
;;;

(defun terminate (parameters state)
  "Checks if the terminal condition is met."
  (cond ((= (parameters-max-iterations parameters)
	    (state-iterations state)) t)
	(t nil)))

(defun increment-iteration (state &optional (step 1))
  "Increments by step the number of iterations."
  (incf (state-iterations state) step))


;;;
;;; output data
;;;

(defun output-state (state stats)
  "Prints simulation information."
  (format t "~a ~a ~a ~a~%" (state-iterations state) 
	  (float (first stats)) (float (second stats)) (float (third stats))))


;;;
;;; evaluation functions
;;;

(defun symmetric-tsp (route n distances)
  "Computes the length of a symmetric tsp route."
  (loop 
     for i from 1 to n
     for j from 2 to (1+ n)
     sum (aref distances (aref route i) (aref route j))))


;;;
;;; construct solutions
;;;

(defun construct-solutions (parameters colony)
  "Construct a TSP tour."
  (let ((n (parameters-n parameters))
	(ants (colony-ants colony))
	(step 1))
    (empty-ants-memory n ants)
    (assign-initial-city n ants step)
    (construct-tour n ants step 
		    (colony-choice-info colony)
		    (parameters-decision-rule parameters) parameters)
    (compute-tour-length n ants 
			 (parameters-eval-tour parameters)
			 (parameters-distances parameters))))

(defun empty-ants-memory (n ants)
  "Mark all cities to unvisited."
  (loop for ant across ants
     do (loop for i from 1 to n
	   do (setf (aref (ant-visited ant) i) nil))))

(defun assign-initial-city (n ants step)
  "Assign a random initial city and marks it as visited."
  (loop for ant across ants
     do (let ((position (1+ (random n))))
	  (setf (aref (ant-tour ant) step) position
		(aref (ant-visited ant) position) t))))

(defun construct-tour (n ants step choice-info decision-rule parameters)
  "Each ant constructs a complete solution (tour)."
  (loop while (< step n)
     do (progn
	  (incf step)
	  (loop for ant across ants
	     do (funcall decision-rule ant step n choice-info parameters)))))

(defun compute-tour-length (n ants fn-eval distances)
  "Computes the length of a tour. Add the return to the begin."
  (loop for ant across ants
     do (progn
	  (setf (aref (ant-tour ant) (1+ n)) (aref (ant-tour ant) 1))
	  (setf (ant-tour-length ant)
		(funcall fn-eval (ant-tour ant) n distances)))))


;;;
;;; decison rules
;;;

(defun verify-tour (tour)
  (loop with list = (loop for x across tour collect x)
     for pos across tour
     for n from 0 below (length tour)
     when (member pos (remove pos list))
     collect n into duplicates
     finally (return duplicates)))

(defun as-decision (ant step n choice-info parameters)
  "Determines an ant next action to be executed (ant system, no candidates list)."
  (let* ((tour (ant-tour ant))
	 (visited (ant-visited ant))
	 (sum-probabilities 0.0)
	 (selection-probability (make-array (1+ n) :initial-element 0.0))
	 (c (aref tour (1- step))))
    (loop for j from 1 to n
       do (if (equal (aref visited j) t) 
	      (setf (aref selection-probability j) 0.0)
	      (progn
		(setf (aref selection-probability j) (aref choice-info c j))
		(setf sum-probabilities (+ sum-probabilities (aref selection-probability j))))))
    ;;; DEBUG
    ;;; - SUM-PROBABILITIES cannot be 0.0!
    ;(when (= sum-probabilities 0.0)
    ;  (let ((index  (loop 
;		       for n from 0 below (length visited)
;		       for pos across visited
;		       when (eql pos nil)
;		       collect n into positions
;		       finally (return positions))))
;     (format t "~% ~a~% ~a~% ~a ~a ~a ~a~% ~a~% ~a~% ~a ~a ~a ~a ~%" 
;	      visited 
;	      tour 
;	      c 
;	      step 
;	      (length visited)
;	      (length tour)
;	      (verify-tour tour)
;	      index
;	      (aref choice-info c (first index))
;	      (aref choice-info c (second index))
	     ; (aref heuristic c (second index))
	     ; (aref pheromone c (second index))
;	      nil nil
;	      )))
    ;;; END DEBUG
    (if (= sum-probabilities 0.0)
	;; all remaining cities have 0 pheromone
	;; so, all have the same probability of being chosen
	(let* ((nodes (loop with size = (length visited)
			 for i from 0 below size
			 for j across visited
			 when (eql j nil)
			 collect i into nodes
			 finally (return nodes)))
	       (size (length nodes))
	       (last-nodes (subseq nodes 1 (1- size)))
	       (size-nodes (length last-nodes))
	       (nodes-probabilities (make-array size-nodes :initial-element (/ 1 size-nodes)))
	       (r (random 1.0))
	       (j 0)
	       (p (aref nodes-probabilities j)))
	  (loop while (< p r)
	       do (progn
		    (incf j)
		    (incf p (aref nodes-probabilities j))))
	  (setf (aref tour step) (nth j last-nodes))
	  (setf (aref visited (nth j last-nodes)) t))
	;; normal mode
	(let* ((r (random sum-probabilities))
	       (j 1) (p (aref selection-probability j)))
	  (loop while (< p r)
	     do (progn
		  (incf j)
		  (incf p (aref selection-probability j))))
	  (setf (aref tour step) j)
	  (setf (aref visited j) t)))))

(defun as-decision-with-neighbors (ant step n choice-info parameters)
  "Determines an ant next action to be executed (ant system, no candidates list)."
  (let* ((n-neighbors (parameters-n-neighbors parameters))
	 (neighbors (parameters-nearest-neighbors parameters))
	 (tour (ant-tour ant))
	 (visited (ant-visited ant))
	 (sum-probabilities 0.0)
	 (selection-probability (make-array (1+ n-neighbors) :initial-element 0.0))
	 (c (aref tour (1- step))))
    (loop for j from 1 to n-neighbors
       do (if (equal (aref visited (aref neighbors c j)) t) 
	      (setf (aref selection-probability j) 0.0)
	      (progn
		(setf (aref selection-probability j) (aref choice-info c (aref neighbors c j)))
		(setf sum-probabilities (+ sum-probabilities (aref selection-probability j))))))
    ;;; DEBUG
    ;(format t "~% n-neighbors ~a city ~a ~% ~a ~%" n-neighbors c neighbors)
    ;;;
    (if (= sum-probabilities 0.0)
	(choose-best-next step n tour visited choice-info)
	(let* ((r (random sum-probabilities))
	       (j 1) (p (aref selection-probability j)))
	  (loop while (< p r)
	     do (progn
		  (incf j)
		  (incf p (aref selection-probability j))))
	  (setf (aref tour step) (aref neighbors c j))
	  (setf (aref visited (aref neighbors c j)) t)))))
      

(defun choose-best-next (step n tour visited choice-info)
  "Choose a city when the neighbors list has been all used."
  (let ((nc 0) (v 0.0) (c (aref tour (1- step))))
    (loop for j from 1 to n
       do (if (equal (aref visited j) nil)
	      (if (>= (aref choice-info c j) v)
		  (setf nc j
			v (aref choice-info c j)))))
    (setf (aref tour step) nc)
    (setf (aref visited nc) t)))


;;;
;;; pheromone update
;;;    

(defun update-trails (parameters colony stats state)
  "Update the pheromone and heuristic trails."
  (funcall (parameters-pheromone-update parameters)
	   parameters colony stats state))

;; ant system
(defun as-pheromone-update (parameters colony stats state) 
  "Ant System pheromone update method."
  (let ((ants (colony-ants colony))
	(n (parameters-n parameters))
	(pheromone (colony-pheromone colony))
	(rho (parameters-rho parameters))
	(heuristic (colony-heuristic colony))
	(choice-info (colony-choice-info colony))
	(alpha (parameters-alpha parameters))
	(beta (parameters-beta parameters)))
    (evaporate n pheromone rho)
    (loop for ant across ants
       do (deposit-pheromone ant n pheromone 1))
    (update-choice-info n pheromone heuristic choice-info alpha beta)))

(defun evaporate (n pheromone rho)
  "Removes some pheromone from the trails -- evaporation procedure."
  (loop for i from 1 to n
     do (loop for j from i to n
	   do (let ((value (* (- 1 rho) (aref pheromone i j))))
		(setf (aref pheromone i j) value
		      (aref pheromone J i) value)))))
 
(defun deposit-pheromone (ant n pheromone weight)
  "Deposit pheromone in the trail according to AS method."
  (let ((delta (/ weight (ant-tour-length ant))))
    (loop for i from 1 to n
       do (let* ((j (aref (ant-tour ant) i))
		 (l (aref (ant-tour ant) (1+ i)))
		 (value (+ (aref pheromone j l) delta)))
	    (setf (aref pheromone j l) value
		  (aref pheromone l j) value)))))

(defun update-choice-info (n pheromone heuristic choice-info alpha beta)
  "Updates the matrix with the pheromone and heuristic combined info."
  (loop for i from 1 to n
     do (loop for j from 1 to n
	   do (setf (aref choice-info i j)
		    (* (expt (aref pheromone i j) alpha) 
		       (expt (aref heuristic i j) beta))))))

;; elitist ant system
(defun eas-pheromone-update (parameters colony stats state)
  "Elite Ant System pheromone update method."
 (let ((ants (colony-ants colony))
       (n (parameters-n parameters))
       (pheromone (colony-pheromone colony))
       (rho (parameters-rho parameters))
       (heuristic (colony-heuristic colony))
       (choice-info (colony-choice-info colony))
       (alpha (parameters-alpha parameters))
       (beta (parameters-beta parameters))
       (e (parameters-n parameters))
       (best-so-far-ant (statistics-best-ant stats)))
   (evaporate n pheromone rho)
   (loop for ant across ants
      do (deposit-pheromone ant n pheromone 1))
   (deposit-pheromone best-so-far-ant n pheromone e)
   (update-choice-info n pheromone heuristic choice-info alpha beta)))

;; rank ant system
(defun rank-pheromone-update (parameters colony stats state)
  "Rank Ant System pheromone update method."
 (let* ((ants (colony-ants colony))
	(n (parameters-n parameters))
	(pheromone (colony-pheromone colony))
	(rho (parameters-rho parameters))
	(heuristic (colony-heuristic colony))
	(choice-info (colony-choice-info colony))
	(alpha (parameters-alpha parameters))
	(beta (parameters-beta parameters))
	(best-so-far-ant (statistics-best-ant stats))
	(w 6)
	(rank-ants (rank-w-ants (1- w) (colony-n-ants colony) ants)))
   (evaporate n pheromone rho)
   (loop for r from 1 below w 
      do (deposit-pheromone (aref rank-ants (1- r)) n pheromone (- w r)))
   (deposit-pheromone best-so-far-ant n pheromone w)
   (update-choice-info n pheromone heuristic choice-info alpha beta)))

(defun safe-copy-colony (n ants)
  (loop 
     with new-ants = (make-array n) 
     for i from 0 below n
     do (setf (aref new-ants i)
	      (safe-copy-ant (aref ants i)))
     finally (return new-ants)))

(defun rank-w-ants (w n-ants ants)
  "Sort ants in increasong tour length and returns the w best."
  (let ((full-rank 
	 (sort (safe-copy-colony n-ants ants) #'< :key #'ant-tour-length))
	(w-rank (make-array w)))
    (loop for i from 0 below w 
       do (setf (aref w-rank i)
		(safe-copy-ant (aref full-rank i)))
       finally (return w-rank))))

;; MMAS
(defun mmas-pheromone-update (parameters colony stats state)
  "Mïn-Max Ant System pheromone update method."
 (let* ((ants (colony-ants colony))
	(n (parameters-n parameters))
	(pheromone (colony-pheromone colony))
	(rho (parameters-rho parameters))
	(heuristic (colony-heuristic colony))
	(choice-info (colony-choice-info colony))
	(alpha (parameters-alpha parameters))
	(beta (parameters-beta parameters))
	(best-so-far-ant (statistics-best-ant stats)))
   (evaporate n pheromone rho)
   (if (= 1 (mod (state-iterations state) (state-flag state)))
       (deposit-pheromone best-so-far-ant n pheromone 1)
       (let ((current-best-ant (find-best-ant (colony-n-ants colony) ants)))
	 (deposit-pheromone current-best-ant n pheromone 1)))
   (verify-pheromone-limits (parameters-n paramters)
			    (colony-pheromone colony)
			    (colony-trail-max colony)
			    (colony-trail-min colony))
   (update-choice-info n pheromone heuristic choice-info alpha beta)))

(defun check-pheromone-limits (n pheromone max min)
  "Checks for violations of pheromone limits and updates according to max and min values."
  (loop for i from 1 to n
     do (loop for j from 1 to n 
	   when (< (aref pheromone i j) min)
	   do (setf (aref pheromone i j) min)
	   when (> (aref pheromone i j) max)
	   do (setf (aref pheromone i j) max))))

(defun find-best-ant (n-ants ants)
  "Return the current best ant in the colony."
  (loop 
     with current = (aref ants 0)
     for i from 1 below n-ants
     when (< (ant-tour-length (aref ants i)) 
	     (ant-tour-length current)) 
     do (setf current (aref ants i))
     finally (return current)))


;; HCF
(defun hc-pheromone-update (ants n pheromone rho heuristic choice-info alpha beta)
  "Hyper-cube mode to update pheromones in AS."
  (loop for ant across ants
     do  (let ((delta-k (/ 1 (ant-tour-length ant)))
	       (delta-total (loop for ant-h across ants
			       sum (/ 1 (ant-tour-length ant-h)))))
	   (loop for i from 1 to n
	      do (let* ((j (aref (ant-tour ant) i))
			(l (aref (ant-tour ant) (1+ i)))
			(value (+ (* (- 1 rho) (aref pheromone j l))
				  (* rho (/ delta-k delta-total)))))
		   (setf (aref pheromone j l) value
			 (aref pheromone l j) value)))))
  (update-choice-info n pheromone heuristic choice-info alpha beta))


;;;
;;; high-level functions
;;;

(defun update-statistics (colony stats state)
  "Update all the ACO stats."
  (loop with best = 10000000000 
     for ant across (colony-ants colony)
     do (when (< (ant-tour-length ant) best)
	  (setf best (ant-tour-length ant))
	  (when (< best (ant-tour-length (statistics-best-ant stats)))
	    (setf (statistics-best-ant stats) (safe-copy-ant ant)
		  (statistics-best-iteration stats) (state-iterations state))))
     sum (ant-tour-length ant) into total
     finally (return (list (ant-tour-length (statistics-best-ant stats)) 
			   best (/ total (colony-n-ants colony))))))


