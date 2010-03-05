(in-package #:cl-aco)

;;;;
;;;; functions and definitions to allow the evolution of ACO components
;;;;

;;;
;;; global vars for state communication from aco to go tree
;;;

(defparameter *ants*           nil)
(defparameter *n*              nil)
(defparameter *n-ants*         nil)
(defparameter *pheromone*      nil)
(defparameter *rho*            nil)
(defparameter *iteration*      nil)
(defparameter *max-iterations* nil)
(defparameter *cf*             nil)
(defparameter *best-ant*       nil)
(defparameter *current-ant*    nil)
(defparameter *restart-ant*    nil)
(defparameter *restarts*       nil)
(defparameter *trail-max*      nil)
(defparameter *trail-min*      nil)


;;;
;;; main call for an ACO with a gp tree
;;;

(defun gp-ant-system (gp-tree filename &key (runs 1) (max-iterations 100) 
		      (output :screen) (restart nil))
  "Ant System standard run using a pre-defined tree."
  (let ((parameters (make-parameters :max-iterations max-iterations
				     :ant-system :gpas
				     :pheromone-update (gp-pheromone-update gp-tree)
				     :decision-rule #'as-decision
				     :restart restart)))
    (run-aco :filename filename :parameters parameters :runs runs :output output :id "gpas")))


(defun evaluate-aco-gp-tree (gp-tree parameters &key (runs 1) (output :none) (id "0"))
  "Evaluates a GP tree. Parameters must be created outside to avoid unnecessary computations."
  (setf (parameters-pheromone-update parameters)
	(gp-pheromone-update gp-tree))
  (run-aco :parameters parameters :runs runs :output output :id id))

;;;
;;; pheromone update for an aco with a gp tree
;;;

(defun gp-pheromone-update (gp-tree)
  "Returns the update function for ACO using a gp individual."
  #'(lambda (parameters colony stats state)
      (let* ((heuristic (colony-heuristic colony))
	     (choice-info (colony-choice-info colony))
	     (alpha (parameters-alpha parameters))
	     (beta (parameters-beta parameters)))
	(setf *ants* (colony-ants colony)
	      *n*  (parameters-n parameters)
	      *n-ants* (colony-n-ants colony)
	      *pheromone* (colony-pheromone colony)
	      *rho* (parameters-rho parameters)
	      *iteration* (state-iterations state)
	      *max-iterations* (parameters-max-iterations parameters)
	      *cf* (state-cf state)
	      *best-ant*  (statistics-best-ant stats)
	      *current-ant*  (find-best-ant (colony-n-ants colony) 
					    (colony-ants colony))
	      *restart-ant* (statistics-restart-ant stats)
	      *restarts* (statistics-restarts stats)
	      *trail-max* (colony-trail-max colony)
	      *trail-min* (colony-trail-min colony))
	(eval gp-tree)
	(update-choice-info *n* *pheromone* heuristic choice-info alpha beta))))


;;;
;;; examples of gp-trees that define the current
;;; pheromone update rules found in the different 
;;; ant systems (AS, EAS, RAS, etc). 
;;; Warning: some minor differences might exist!
;;;

(defparameter *as-gp-tree* 
  '(aco-prog2
    (aco-evaporate (aco-rho))
    (aco-deposit (aco-ants) 1))
  "Ant System using the GP function and terminal set.")

(defparameter *eas-gp-tree*
  '(aco-prog3
    (aco-evaporate (aco-rho))
    (aco-deposit (aco-ants) 1)
    (aco-deposit (aco-best-ant) *n*)) ; the constant value should be equal to n
  "Elite Ant System using the GP function and terminal set.")

(defparameter *ras-gp-tree*
  '(aco-prog3
    (aco-evaporate (aco-rho))
    (aco-deposit (aco-rank-ants 6) 6) 
    (aco-deposit (aco-best-ant) 6))
  "Rank-based Ant System using the GP function and terminal set.")

(defparameter *mmas-gp-tree*
  '(aco-prog3
    (aco-evaporate (aco-rho))
    (aco-if (aco-or  
	     (aco-< (aco-iteration) (aco-q3))
	     (aco-> (aco-mod (aco-iteration) 50) 0))
     (aco-deposit (aco-current-ant) 1)
     (aco-deposit (aco-best-ant) 1))
    (aco-verify-limits))
  "MMAS using the GP function and temrinal set.")


;;;
;;; function and terminal sets lists to be used for evolution
;;;

;; basic sets (should allow only AS and EAS)
(defparameter *fset-basic* '(aco-prog2 aco-prog3 aco-evaporate aco-deposit))
(defparameter *tset-basic* '(aco-rho aco-ants aco-best-ant aco-constant-real aco-constant-int))

;; basic set + ras
(defparameter *fset-basic+ras* (append *fset-basic* '(aco-rank-ants)))

;; extensions
(defparameter *fset-conditionals* '(aco-if))
(defparameter *fset-logic* '(aco-and aco-or aco-not))
(defparameter *fset-comparators* '(aco-< aco-<= aco-> aco->= aco-= aco-/=))
(defparameter *fset-math* '(aco-mod))

(defparameter *tset-truth* '(aco-t aco-nil))
(defparameter *tset-state* '(aco-iteration aco-max-iterations aco-cf aco-q1 aco-q2 aco-q3))
(defparameter *tset-ants* '(aco-current-ant aco-random-ant))
(defparameter *tset-restart* '(aco-restat-ant aco-restarts))
(defparameter *tset-randoms* '(aco-random-real aco-random-n))
(defparameter *tset-constants* '(aco-constant-int aco-constant-real))

;; extended sets
(defparameter *fset-extended-mmas* (append '(aco-verify-limits) 
					   *fset-conditionals* 
					   *fset-logic*
					   *fset-comparators*
					   *fset-math*))
(defparameter *tset-extended-mmas* (append *tset-state*
					   *tset-ants*
					   *tset-restart*
					   *tset-randoms*
					   *tset-constants*))

;;;
;;; function set definitions
;;;

;; conditionals
(defun aco-if (x y z)
  (if x y z))

(defun aco-and (x y)
  (and x y))

(defun aco-or (x y)
  (or x y))

(defun aco-not (x)
  (not x))

;; comparators
(defun aco-< (x y)
  (and (numberp x) (numberp y)
       (< x y)))

(defun aco-<= (x y)
  (and (numberp x) (numberp y)
       (<= x y)))

(defun aco-> (x y)
  (and (numberp x) (numberp y)
       (> x y)))

(defun aco->= (x y)
  (and (numberp x) (numberp y)
       (>= x y)))

(defun aco-= (x y)
  (and (numberp x) (numberp y)
       (= x y)))

(defun aco-/= (x y)
  (and (numberp x) (numberp y)
       (/= x y)))

;; sequencials
(defun aco-prog2 (x y)
  (progn x y))

(defun aco-prog3 (x y z)
  (progn x y z))
      
;; aco functions
(defun aco-rank-ants (w)
  (when (numberp w)
    (rank-w-ants (1- w) *n-ants* *ants*)))

(defun aco-evaporate (rho)
  (when (and (numberp rho)
	     (<= rho 1))
    (evaporate *n* *pheromone* rho) t))

(defun aco-deposit (a w)
  (let ((weight (if (numberp w) w 0)))
    (cond ((and (arrayp a)
		(< (length a) weight))
	   (loop for r from 1 below weight
	      do (deposit-pheromone (aref a (1- r)) *n* *pheromone* (- weight r))) t)
	  ((arrayp a)
	   (loop for ant across a
	      do (deposit-pheromone ant *n* *pheromone* weight)) t)
	  ((ant-p a) 
	   (deposit-pheromone a *n* *pheromone* weight) t)
	  (t nil))))

;; others
(defun aco-mod (x y)
  (when (and (numberp x) (numberp y))
    (mod x y)))


;;;
;;; terminal set definitions
;;;

;; constants (only used in a tree generation by keeping their values)

(defun aco-small-int (&optional (max 10))
  (aco-constant-int max))

(defun aco-constant-int (&optional (max 51)) ; this number should be equal to n-ants
  (random max))

(defun aco-constant-real ()
  (random 1.0))

(defun aco-t ()
  t)

(defun aco-nil ()
  nil)

;; random numbers
(defun aco-random-real ()
  (random 1.0))

(defun aco-random10 ()
  (random 10))

(defun aco-random100 ()
  (random 100))

(defun aco-random-n ()
  (random *n*))

;; aco state information
(defun aco-rho ()
  *rho*)

(defun aco-iteration ()
  *iteration*)

(defun aco-max-iterations ()
  *max-iterations*)

(defun aco-cf ()
  *cf*)

(defun aco-q1 ()
  (/ *max-iterations* 4))

(defun aco-q2 ()
  (/ *max-iterations* 2))

(defun aco-q3 ()
  (+ (aco-q1) (aco-q2)))

;; ants 
(defun aco-best-ant ()
  *best-ant*)

(defun aco-current-ant ()
  *current-ant*)

(defun aco-restart-ant ()
  *restart-ant*)

(defun aco-restarts ()
  *restarts*)

(defun aco-ants ()
  *ants*)

(defun aco-random-ant ()
  (aref *ants* (random *n-ants*)))

(defun aco-verify-limits ()
  (verify-pheromone-limits *n* *pheromone* *trail-max* *trail-min*))


;;;
;;; tree generators 
;;;


(defun ramped-half-and-half (limit fset tset)
  "A gp tree is created with half of probability for each method."
  (if (< (random 1.0) 0.5)
      (full-method-tree 0 limit fset tset)
      (grow-method-tree 0 limit fset tset)))

(defun full-method-tree (size limit fset tset)
 "Random tree according to the Full method."
 (if (= size limit)
     (process-terminal (nth (random (length tset)) tset))
     (let ((f (nth (random (length fset)) fset)))
       (cons f
	     (case f
	       (aco-evaporate
		(list (full-method-tree (1+ size) limit fset tset)))
	       ((aco-prog2 aco-deposit) 
		(list (full-method-tree (1+ size) limit fset tset)
		      (full-method-tree (1+ size) limit fset tset)))
	       (aco-prog3
		(list (full-method-tree (1+ size) limit fset tset)
		      (full-method-tree (1+ size) limit fset tset)
		      (full-method-tree (1+ size) limit fset tset))))))))

(defun grow-method-tree (size limit fset tset)
  "Random tree according to the Grow method."
  (if (= size limit)
      (process-terminal (nth (random (length tset)) tset))
      (let* ((set (append fset tset))
	     (f (nth (random (length set)) set)))
	(case f
	  (aco-evaporate
	   (cons f (list (grow-method-tree (1+ size) limit fset tset))))
	  ((aco-prog2 aco-deposit) 
	   (cons f (list (grow-method-tree (1+ size) limit fset tset)
			 (grow-method-tree (1+ size) limit fset tset))))
	  (aco-prog3
	   (cons f (list (grow-method-tree (1+ size) limit fset tset)
			 (grow-method-tree (1+ size) limit fset tset)
			 (grow-method-tree (1+ size) limit fset tset))))
	  (otherwise (process-terminal f))))))

(defun process-terminal (terminal)
  (case terminal
    (aco-small-int (aco-small-int))
    (aco-constant-int (aco-constant-int))
    (aco-constant-real (aco-constant-real))
    (otherwise (list terminal))))


;;;;
;;;; GP engine (later to be replace by cl-evo)
;;;;

;;;
;;; representation and population initialization
;;;

(defstruct individual
  (tree nil)
  (fitness 0))

(defun copy-individual (individual)
  "Fresh copy of a individual structure."
  (make-individual
   :tree (copy-tree (individual-tree individual))
   :fitness (individual-fitness individual)))

(defun make-random-individual (tree-limit fset tset)
  "Return a random generate tree without being evaluated."
  (make-individual :tree (ramped-half-and-half tree-limit fset tset)))

(defun make-population (size tree-limit fset tset)
  "Return an array filled with random gp individuals."
  (make-array size 
	      :initial-contents (loop repeat size 
				   collect (make-random-individual tree-limit fset tset))))

;;;
;;; evaluations
;;;

(defun eval-population (population size fitness-function generation)
  "Set the fitness to every element in the population."
  (loop 
     for individual across population
     for id from 1 to size
     do (setf (individual-fitness individual) 
	      (funcall fitness-function individual id generation))))


(defstruct problem-config 
  (filename eil51)
  (runs 1)
  (output :none)
  (max-iterations 100)
  (ant-system :gpas)
  (restart nil))

(defun make-fitness-function (problem)
  "Return a fitness evaluation with the problem data required for it."
  (let ((runs (problem-config-runs problem))
	(output (problem-config-output problem))
	(parameters (read-problem-data (problem-config-filename problem)
				       (make-parameters :max-iterations (problem-config-max-iterations problem)
							:ant-system (problem-config-max-iterations problem)
							:restart (problem-config-restart problem)))))
    #'(lambda (individual id generation)
	(let ((results (evaluate-aco-gp-tree (individual-tree individual)
					     parameters 
					     :runs runs 
					     :output output
					     :id (if (eql output :none) 
						     (concatenate 'string 
								  (write-to-string generation) 
								  (write-to-string id)) "aco"))))
	  (if (= runs 1)
	      (ant-tour-length (statistics-best-ant (first results)))
	      (loop 
		 for result in results
		 sum (ant-tour-length (statistics-best-ant result)) into total-best
		 finally (return (float (/ total-best runs)))))))))

;;;
;;; selection
;;;

(defun tournament (tournament-size population size)
  "Tournament selection: return best individual from a random set of a given size."
  (loop with best = (aref population (random size))
     for n from 1 below tournament-size
     do (let ((current (aref population (random size))))
	  (when (< (individual-fitness current) (individual-fitness best))
	    (setf best (copy-individual current))))
     finally (return best)))

(defun selection (population size tournament-size)
  "Return a new population."
  (loop with new-population = (make-array size)
     for i from 0 below size
     do (setf (aref new-population i)
	      (tournament tournament-size population size))
     finally (return new-population)))


;;;
;;; elitism
;;;

(defun find-best (population size comparator)
  "Return the indicies of the best or worst individuals in the population, according to the comparator."
  (loop 
     with best = 0
     for i from 1 below size 
     when (funcall comparator 
		   (individual-fitness (aref population i)) 
		   (individual-fitness (aref population best)))
     do (setf best i)
     finally (return best)))

(defun elitism (population size best-individual)
  "Replace a random individual with the best from the previous generation."
  (let ((worst-position (find-best population size #'>)))
    (setf (aref population worst-position) (copy-individual best-individual)) population))


;;;
;;; genetic operators
;;;

(defun apply-crossover (population size max-depth rate)
  "Apply tree crossover to the population."
  (loop for position from 0 below size by 2
     do (when (< (random 1.0) rate)
	  (multiple-value-bind (o1 o2)
	      (tree-crossover max-depth (aref population position) (aref population (1+ position)))
	    (setf (aref population position) o1 (aref population (1+ position)) o2)))))

(defun tree-crossover (size p1 p2)
  (multiple-value-bind (o1 o2)
      (cross-subtrees (individual-tree p1) (individual-tree p2) size)
    (values (make-individual :tree (copy-tree o1))
	    (make-individual :tree (copy-tree o2)))))

(defun cross-subtrees (p1 p2 depth)
  "Exchanges two subtrees in a random point."
  (let* ((p1-point (random (count-tree-nodes p1)))
         (p2-point (random (count-tree-nodes p2)))
         (o1 (list (copy-tree p1)))
         (o2 (list (copy-tree p2))))
    (multiple-value-bind (p1-subtree p1-fragment)
        (get-subtree (first o1) o1 p1-point)
      (multiple-value-bind
            (p2-subtree p2-fragment)
          (get-subtree
           (first o2) o2 p2-point)
        (setf (first p1-subtree) p2-fragment)
        (setf (first p2-subtree) p1-fragment)))
    (validate-crossover p1 o1 p2 o2 depth)))

(defun get-subtree (tree point index)
  "Return a subtree."
  (if (= index 0)
      (values point (copy-tree tree) index)
      (if (consp tree)
	  (do* ((tree-rest (rest tree) (rest tree-rest))
		(arg (first tree-rest) (first tree-rest)))
	       ((not tree-rest) (values nil nil index))
	    (multiple-value-bind
		  (new-point new-tree new-index)
		(get-subtree arg tree-rest (1- index))
	      (if (= new-index 0)
		  (return (values new-point new-tree new-index))
		  (setf index new-index))))
	  (values nil nil index))))

(defun validate-crossover (p1 o1 p2 o2 depth)
  "Validates the offspring. If they pass the maximum depth they are rejected."
  (let ((p1-limit (tree-depth (first o1)))
        (p2-limit (tree-depth (first o2))))
    (values
     (if (or (= 1 p1-limit) (> p1-limit depth))
         p1 (first o1))
     (if (or (= 1 p2-limit) (> p2-limit depth))
         p2 (first o2)))))

(defun count-tree-nodes (tree)
  "Count the number of nodes in a tree."
  (if (consp tree)
      (+ 1 (reduce #'+ (mapcar #'count-tree-nodes (rest tree)))) 1))
  
(defun tree-depth (tree)
 "Return the max depth of a tree."
 (if (consp tree)
     (+ 1 (if (rest tree)
	      (apply #'max (mapcar #'tree-depth (rest tree))) 0)) 1))


;;;
;;; gp engine
;;;

(defstruct gp-params
  (total-generations 100)
  (pop-size 100)
  (initial-depth 2)
  (max-depth 5)
  (fset nil)
  (tset nil)
  (fitness nil)
  (t-size 3)
  (cx-rate 0.9)
  (elitism t)
  )

(defun run-aco-gp (fset tset &key (gp-id "gp-aco") (gp-runs 1) (gp-output :screen) (generations 10)
		   (pop-size 10) (initial-depth 2) (max-depth 5) (elitism t)		  
		   (filename eil51) (aco-runs 1) (aco-output :none) 
		   (max-iterations 100) (ant-system :gpas) (restart nil))
  "Start GP."
  (let* ((problem (make-problem-config :filename filename
				       :runs aco-runs
				       :output aco-output
				       :max-iterations max-iterations
				       :ant-system ant-system
				       :restart restart)) 
	 (fitness (make-fitness-function problem))
	 (gp-params (make-gp-params :total-generations generations
				    :pop-size pop-size
				    :initial-depth initial-depth
				    :max-depth max-depth
				    :fset fset
				    :tset tset
				    :fitness fitness
				    :elitism elitism)))
    (gp-multiple-runs gp-params :runs gp-runs :output gp-output :id gp-id)))

(defun gp-multiple-runs (parameters &key (runs 1) (output :screen) (id "gp"))
  "Run the gp engine for several runs"
  (loop for run from 1 to runs
     collect (config-gp-output parameters output run id)))

(defun config-gp-output (parameters output run id)
  "Config a GP run output (:none, :screen, :files, or both)."
  (if (member output '(:files :screen+files))
      (with-open-file (run-stream (concatenate 'string id "-run" (format nil "~D" run) ".txt")
				  :direction :output :if-exists :supersede)
	(with-open-file (best-stream (concatenate 'string id "-best" (format nil "~D" run) ".txt")
				     :direction :output :if-exists :supersede)
	  (run-single-gp parameters output (list run-stream best-stream))))
      (run-single-gp parameters output nil)))

(defun run-single-gp (parameters output streams)
  "Main gp loop."
  (let* ((total-generations (gp-params-total-generations parameters))
	 (pop-size (gp-params-pop-size parameters))
	 (initial-depth (gp-params-initial-depth parameters))
	 (max-depth (gp-params-max-depth parameters))
	 (fset (gp-params-fset parameters))
	 (tset (gp-params-tset parameters))
	 (fitness (gp-params-fitness parameters))
	 (t-size (gp-params-t-size parameters))
	 (cx-rate (gp-params-cx-rate parameters))
	 (population (make-population pop-size initial-depth fset tset))
	 (elitism-p (gp-params-elitism parameters))
	 (best nil) (run-best nil))
    (eval-population population pop-size fitness 1)
    (setf best (copy-individual (aref population (find-best population pop-size #'<))))
    (setf run-best (copy-individual best))
    (output-generation 1 population pop-size best run-best output streams)
    (loop for generation from 2 to total-generations
       do (let ((new-population (selection population pop-size t-size)))
	    (apply-crossover new-population pop-size max-depth cx-rate)
	    (eval-population new-population pop-size fitness generation)
	    (when elitism-p
	      (elitism new-population pop-size best))
	    (setf population new-population)
	    (setf best (copy-individual (aref population (find-best population pop-size #'<))))
	    (when (< (individual-fitness best) (individual-fitness run-best))
	      (setf run-best (copy-individual best)))
	    (output-generation generation population pop-size best run-best output streams))
       finally (return run-best))))

(defun output-generation (generation population pop-size best run-best output streams)
  "Shows the state of a generation"
  (unless (eql output :none)
    (let ((best-fitness (float (individual-fitness best)))
	  (avg (float (average population pop-size))))
      (when (member output '(:screen :screen+files))
	(format t "~a ~a ~a ~%" generation best-fitness avg))
      (when (member output '(:files :scree+files))
	(format (first streams) "~a ~a ~a ~%" generation best-fitness avg)
	(when (<= (individual-fitness best) (individual-fitness run-best))
	  (format (second streams) "~a ~%" (list generation run-best)))))))

(defun average (population pop-size)
  "Average of population's fitness."
  (loop for individual across population
     sum (individual-fitness individual) into total
     finally (return (/ total pop-size))))



