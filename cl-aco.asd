(defpackage #:cl-aco-system 
  (:use #:common-lisp #:asdf))  
 
(in-package #:cl-aco-system)  
 
(defsystem :cl-aco
  :description "cl-aco: Ant Colony Optimization library."  
  :version "0.2"  
  :author "Jorge Tavares <jorge.tavares@ieee.org>"  
  :licence "MIT"  
  :components ((:file "package")
	       (:file "definitions" :depends-on ("package"))
	       (:file "utils"       :depends-on ("package"))
	       (:file "init"        :depends-on ("package"))
	       (:file "pheromone"   :depends-on ("package"))
	       (:file "decision"    :depends-on ("package"))
	       (:file "construct"   :depends-on ("package"))
	       (:file "convergence" :depends-on ("package"))
	       (:file "aco"         :depends-on ("package" 
						 "utils"
						 "init"
						 "pheromone"
						 "decision"
						 "construct"
						 "convergence"))
	       ))
