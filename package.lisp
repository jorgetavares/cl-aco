(defpackage #:cl-aco
  (:use #:common-lisp)
  (:export #:ant-system
	   #:elite-ant-system
	   #:rank-ant-system
	   #:min-max-ant-system
	   #:run-aco
	   #:config-output-run-aco
	   #:run-single-aco
	   #:safe-copy-ant
	   #:safe-copy-colony
	   #:process-stats
	   #:as-decision
	   #:restart-pheromone-trails-mmas
	   #:restart-pheromone-trails-mmas-hcf
	   #:update-trails
	   #:evaporate
	   #:deposit-pheromone
	   #:update-choice-info
	   #:as-pheromone-update
	   #:eas-pheromone-update
	   #:rank-pheromone-update
	   #:rank-w-ants
	   #:mmas-pheromone-update
	   #:verify-pheromone-limits
	   #:find-best-ant
	   #:mmas-hcf-pheromone-update
	   #:update-convergence-factor
	   #:convergence-std-dev
	   #:branching-factor
	   #:hcf-conbergence-factor
	   #:make-parameters
	   #:parameters-n
	   #:parameters-distances
	   #:parameters-nearest-neighbors
	   #:parameters-n-neighbors
	   #:parameters-n-ants
	   #:parameters-alpha
	   #:parameters-beta
	   #:parameters-rho
	   #:parameters-max-iterations
	   #:parameters-ant-system
	   #:parameters-avg-cost
	   #:parameters-pheromone-update
	   #:parameters-decision-rule
	   #:parameters-eval-tour
	   #:parameters-lambda
	   #:parameters-convergence-function
	   #:parameters-stagnation-limit
	   #:parameters-restart
	   #:parameters-restart-iterations
	   #:make-state
	   #:state-iterations
	   #:state-best
	   #:state-flag
	   #:state-stagnation
	   #:state-current-best
	   #:state-pop-avg
	   #:state-pop-std-dev
	   #:state-bs-update
	   #:state-cf
	   #:make-ant
	   #:ant-tour-length
	   #:ant-tour
	   #:ant-visited
	   #:make-colony
	   #:colony-n-ants
	   #:colony-ants
	   #:colony-pheromone
	   #:colony-trail-max
	   #:colony-trail-min
	   #:colony-heuristic
	   #:colony-choice-info
	   #:make-statistics
	   #:statistics-best-ant
           #:statistics-best-iteration
           #:statistics-restart-ant
           #:statistics-restart-iteration
           #:statistics-restarts
           #:statistics-branching
            
	   
	   ))

  
  