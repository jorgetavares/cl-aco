(in-package #:cl-aco)

;;;
;;; utils
;;;

;; types for as-decision optimization
;(deftype node () '(integer 0 *))
(deftype node () 'fixnum)

(deftype array-node () '(simple-array node (*)))
(deftype array-boolean () '(simple-array boolean (*)))

(deftype float-array () '(simple-array single-float (*)))
(deftype float-matrix () '(array single-float (* *)))


;;;
;;; functions
;;;

(defun safe-copy-ant (ant)
  "Return a fresh structure of a given ant."
  (make-ant
   :tour-length (ant-tour-length ant)
   :tour (copy-seq (ant-tour ant))
   :visited (copy-seq (ant-visited ant))))

(defun safe-copy-colony (n ants)
  "Return a fresh array with safe copies of the ants."
  (loop 
     with new-ants = (make-array n) 
     for i from 0 below n
     do (setf (aref new-ants i)
	      (copy-ant (aref ants i)))
     finally (return new-ants)))


;;;
;;; stats analysis
;;;

(defun process-stats (results)
  (let ((tours (loop for r in results
		  collect (ant-tour-length (statistics-best-ant r))))
	(size (length results)))
    (values (apply #'max tours)
	    (apply #'min tours)
	    (1+ (position (apply #'min tours) tours))
	    (float (/ (apply #'+ tours) size))
	    (sort (copy-list tours) #'<))))

