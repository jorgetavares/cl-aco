(in-package #:cl-aco)

;;;;
;;;; functions and definitions to allow the evolution of ACO components
;;;;

;;;
;;; execute GP engine
;;;

(defun int-constants (min max)
  #'(lambda ()
      (+ min (random (1+ (- max min))))))

(defparameter *fset-basic* (mini-gp:make-fset 'aco-prog2 2
					      'aco-prog3 3 
					      'aco-evaporate 1
					      'aco-deposit 2
					      ))

(defparameter *tset-basic* '(aco-rho aco-ants aco-best-ant 
			     mini-gp:gp-constant-real mini-gp:gp-constant))


(defun run-aco-gp (fset tset &key (gp-id "gp-aco") (gp-runs 1) (gp-output :screen) (generations 10)
		   (pop-size 10) (initial-depth 2) (max-depth 5) (elitism t)		  
		   (filename eil51) (aco-runs 1) (aco-output :none) 
		   (max-iterations 10) (ant-system :gpas) (restart nil))
  "Start GP."
  (setf mini-gp:*generate-constant* (int-constants 0 10))
  (let* ((problem (make-problem-config :filename filename
				       :runs aco-runs
				       :output aco-output
				       :max-iterations max-iterations
				       :ant-system ant-system
				       :restart restart))
	 (fitness (make-fitness-function problem))
	 (gp-params (mini-gp:make-gp-params :total-generations generations
						    :pop-size pop-size
						    :initial-depth initial-depth
						    :max-depth max-depth
						    :fset fset
						    :tset tset
						    :fitness fitness
						    :elitism elitism)))
    (mini-gp:gp-multiple-runs gp-params :runs gp-runs :output gp-output :id gp-id)))


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

(defstruct problem-config 
  (filename burma14)
  (runs 1)
  (output :none)
  (max-iterations 10)
  (ant-system :gpas)
  (restart nil))

(defun make-fitness-function (problem)
  "Return a fitness evaluation with the problem data required for it."
  (let ((runs (problem-config-runs problem))
	(output (problem-config-output problem))
	(parameters (read-problem-data 
		     (problem-config-filename problem)
		     (make-parameters :max-iterations (problem-config-max-iterations problem)
				      :ant-system (problem-config-max-iterations problem)
				      :restart (problem-config-restart problem)))))
    #'(lambda (individual id generation)
	(let ((results (evaluate-aco-gp-tree 
			(mini-gp:individual-tree individual)
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


