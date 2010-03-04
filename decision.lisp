(in-package #:cl-aco)

;;;
;;; decison rules
;;;

(defun as-decision (ant step n choice-info parameters)
  "Determines an ant next action to be executed (ant system, no candidates list)."
  (declare (ignore parameters))
  (let* ((tour (ant-tour ant))
	 (visited (ant-visited ant))
	 (sum-probabilities 0.0)
	 (selection-probability (make-array (1+ n) :initial-element 0.0))
	 (c (aref tour (1- step))))
    (loop for j from 1 to n
       do (if (equal (aref visited j) t) 
	      (setf (aref selection-probability j) 0.0)
	      (progn
		(setf (aref selection-probability j) (aref choice-info c j))
		(setf sum-probabilities (+ sum-probabilities (aref selection-probability j))))))
    ;;; DEBUG
    ;;; - SUM-PROBABILITIES cannot be 0.0!
    ;(when (= sum-probabilities 0.0)
    ;  (let ((index  (loop 
;		       for n from 0 below (length visited)
;		       for pos across visited
;		       when (eql pos nil)
;		       collect n into positions
;		       finally (return positions))))
;     (format t "~% ~a~% ~a~% ~a ~a ~a ~a~% ~a~% ~a~% ~a ~a ~a ~a ~%" 
;	      visited 
;	      tour 
;	      c 
;	      step 
;	      (length visited)
;	      (length tour)
;	      (verify-tour tour)
;	      index
;	      (aref choice-info c (first index))
;	      (aref choice-info c (second index))
	     ; (aref heuristic c (second index))
	     ; (aref pheromone c (second index))
;	      nil nil
;	      )))
    ;;; END DEBUG
    (if (= sum-probabilities 0.0)
	;; all remaining cities have 0 pheromone
	;; so, all have the same probability of being chosen
	(let* ((nodes (loop with size = (length visited)
			 for i from 0 below size
			 for j across visited
			 when (eql j nil)
			 collect i into nodes
			 finally (return nodes)))
	       (size (length nodes))
	       (last-nodes (subseq nodes 1 (1- size)))
	       (size-nodes (length last-nodes))
	       (nodes-probabilities (make-array size-nodes :initial-element (/ 1 size-nodes)))
	       (r (random 1.0))
	       (j 0)
	       (p (aref nodes-probabilities j)))
	  ;(format t "zero ~%")
	  (loop while (< p r)
	       do (progn
		    (incf j)
		    (incf p (aref nodes-probabilities j))))
	  (setf (aref tour step) (nth j last-nodes))
	  (setf (aref visited (nth j last-nodes)) t))
	;; normal mode
	(let* ((r (random sum-probabilities))
	       (j 1) (p (aref selection-probability j)))
	  (loop while (< p r)
	     do (progn
		  (incf j)
		  (incf p (aref selection-probability j))))
	  (setf (aref tour step) j)
	  (setf (aref visited j) t)))))

(defun as-decision-with-neighbors (ant step n choice-info parameters)
  "Determines an ant next action to be executed (ant system, no candidates list)."
  (let* ((n-neighbors (parameters-n-neighbors parameters))
	 (neighbors (parameters-nearest-neighbors parameters))
	 (tour (ant-tour ant))
	 (visited (ant-visited ant))
	 (sum-probabilities 0.0)
	 (selection-probability (make-array (1+ n-neighbors) :initial-element 0.0))
	 (c (aref tour (1- step))))
    (loop for j from 1 to n-neighbors
       do (if (equal (aref visited (aref neighbors c j)) t) 
	      (setf (aref selection-probability j) 0.0)
	      (progn
		(setf (aref selection-probability j) (aref choice-info c (aref neighbors c j)))
		(setf sum-probabilities (+ sum-probabilities (aref selection-probability j))))))
    ;;; DEBUG
    ;(format t "~% n-neighbors ~a city ~a ~% ~a ~%" n-neighbors c neighbors)
    ;;;
    (if (= sum-probabilities 0.0)
	(choose-best-next step n tour visited choice-info)
	(let* ((r (random sum-probabilities))
	       (j 1) (p (aref selection-probability j)))
	  (loop while (< p r)
	     do (progn
		  (incf j)
		  (incf p (aref selection-probability j))))
	  (setf (aref tour step) (aref neighbors c j))
	  (setf (aref visited (aref neighbors c j)) t)))))
      
(defun choose-best-next (step n tour visited choice-info)
  "Choose a city when the neighbors list has been all used."
  (let ((nc 0) (v 0.0) (c (aref tour (1- step))))
    (loop for j from 1 to n
       do (if (equal (aref visited j) nil)
	      (if (>= (aref choice-info c j) v)
		  (setf nc j
			v (aref choice-info c j)))))
    (setf (aref tour step) nc)
    (setf (aref visited nc) t)))
