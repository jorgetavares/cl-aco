(in-package #:cl-aco)

;;;;
;;;; profile functions to analyse the ACO code
;;;;


(defun profile-mmas (&optional (runs 10))
  (sb-profile:reset)
  (sb-profile:profile 
   aco-tsp setup-aco-tsp run-multiple-aco-tsp read-prepare-data 
   config-output-run-aco run-single-aco-tsp terminate increment-iteration
   update-statistics output-state
   initializa-colony make-array make-state make-statistics make-ant
   construct-solutions update-convergence-factor 
   restart-pheromone-trails-mmas update-trails format member float
   evaporate deposit-pheromone update-choice-info change-update-ant
   find-best-ant verify-pheromone-limits safe-copy-ant safe-copy-colony
   verify-tour construct-tour compute-tour-length assign-initial-city
   empty-ants-memory as-decision 
   ) 
  (time
   (min-max-ant-system eil51 :runs runs :max-iterations 2000 :output :files))
  (sb-profile:report))

