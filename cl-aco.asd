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
               (:file "aco" :depends-on ("package"))))
