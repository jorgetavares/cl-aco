(defpackage #:cl-aco-mkp-system 
  (:use #:common-lisp #:asdf))  
 
(in-package #:cl-aco-mkp-system)  
 
(defsystem :cl-aco-mkp
  :description "cl-aco-mkp: application of the :cl-aco library on the MKP."  
  :version "0.1"  
  :author "Jorge Tavares <jorge.tavares@ieee.org>"  
  :licence "MIT"
  :depends-on (cl-aco cl-knapsacklib)  
  :components ((:file "package-mkp")
	       (:file "mkp" :depends-on ("package-mkp"))))

