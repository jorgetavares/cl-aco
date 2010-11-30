;;;;
;;;; ACO in CL
;;;;

(in-package #:cl-aco)

;;;
;;; types for as-decision optimization
;;;

(deftype node () '(integer *))

(deftype array-node () '(simple-array node (*)))
(deftype array-boolean () '(simple-array boolean (*)))

(deftype float-array () '(simple-array single-float (*)))
(deftype float-matrix () '(array single-float (* *)))


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
  (rho 0.5)
  (max-iterations 1000)
  (ant-system :mmas)
  (avg-cost 450)
  (pheromone-update #'mmas-pheromone-update)
  (decision-rule #'as-decision)
  (eval-tour nil) ;; NOTE: requires generalization
  (lambda 0.05)
  (convergence-function #'branching-factor)
  (stagnation-limit 3)
  (restart nil)
  (restart-iterations 250)
  (soas-replacement :always) ;; :always or :best (best value is store in state-soas-best
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
  (soas-best -1)
  )


;;
;; representation of ants and ACO data

;; NOTE: the ant structure must be generalized for different problems
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
  (best-iteration 0)
  restart-ant
  (restart-iteration 0)
  (restarts 0)
  (branching 0)
  (pop-avg 0)
  (ants-solutions nil)	
  )

