(defpackage #:cl-aco-system 
  (:use #:common-lisp #:asdf))  
 
(in-package #:cl-aco-system)  
 
(defsystem :cl-aco
  :description "cl-aco: Ant Colony Optimization algorithms for the TSP."  
  :version "0.1"  
  :author "Jorge Tavares <jorge.tavares@ieee.org>"  
  :licence "MIT"  
  :depends-on (cl-tsplib)
  :components ((:file "package")
               (:file "aco"         :depends-on ("package" 
						 "utils"
						 "init"
						 "pheromone"
						 "decision"
						 "construct"
						 "convergence"
						 "debug"))
	       (:file "utils"       :depends-on ("package"))
	       (:file "init"        :depends-on ("package"))
	       (:file "pheromone"   :depends-on ("package"))
	       (:file "decision"    :depends-on ("package"))
	       (:file "construct"   :depends-on ("package"))
	       (:file "convergence" :depends-on ("package"))
	       (:file "debug"       :depends-on ("package"))
	       (:file "evolve"      :depends-on ("package"))))
