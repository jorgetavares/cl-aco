(defpackage #:cl-aco-qap-system 
  (:use #:common-lisp #:asdf))  
 
(in-package #:cl-aco-qap-system)  
 
(defsystem :cl-aco-qap
  :description "cl-aco-qap: application of the :cl-aco library on the QAP."  
  :version "0.1"  
  :author "Jorge Tavares <jorge.tavares@ieee.org>"  
  :licence "MIT"
  :depends-on (cl-aco cl-qaplib)  
  :components ((:file "package-qap")
	       (:file "qap" :depends-on ("package-qap"))))

