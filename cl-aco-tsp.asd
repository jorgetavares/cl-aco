(defpackage #:cl-aco-tsp-system 
  (:use #:common-lisp #:asdf))  
 
(in-package #:cl-aco-tsp-system)  
 
(defsystem :cl-aco-tsp
  :description "cl-aco-tsp: application of the :cl-aco library on the TSP."  
  :version "0.1"  
  :author "Jorge Tavares <jorge.tavares@ieee.org>"  
  :licence "MIT"
  :depends-on (cl-aco cl-tsplib)  
  :components ((:file "package-tsp")
	       (:file "tsp" :depends-on ("package-tsp"))))

