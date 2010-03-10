(in-package #:cl-aco)

;;;
;;; utils
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
	      (safe-copy-ant (aref ants i)))
     finally (return new-ants)))

(defun verify-tour (tour)
  "Return duplicates nodes in a tour; nil if tour is correct."
  (loop with list = (loop for x across tour collect x)
     for pos across tour
     for n from 0 below (length tour)
     when (member pos (remove-first pos list))
     collect n into duplicates
     finally (return duplicates)))

(defun remove-first (object list)
  (if (null list)
      nil
      (if (eql object (first list))
	  (rest list)
	  (cons (first list) 
		(remove-first object (rest list))))))

(defun verify-visited (n visited)
  (notevery #'(lambda (x) (eql x t))
	    (loop for v from 1 to n 
	       collect (aref visited v))))

(defun verify-incomplete-tour (tour)
   (loop with list = (remove 0 (loop for x across tour collect x))
     for pos across tour
     for n from 0 below (length tour)
     when (member pos (remove-first pos list))
     collect n into duplicates
     finally (return duplicates)))
  

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

