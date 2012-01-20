(in-package #:cl-aco)

;;;;
;;;; File contains all the generic functions for pheromone init and 
;;;; update, as well as the specific functions for each ant system.
;;;;
;;;; Current implemented ant systems:
;;;;   :as       - Ant System (original and most simple AS)
;;;;   :eas      - Elit Ant System
;;;;   :ras      - Rank-based Ant System
;;;;   :mmas     - Max-Min Ant System
;;;;   :mmas-hcf - Max-Min Ant System with Hyper-Cube Framework 
;;;;


;;;
;;; pheromone setup
;;;

(defun update-trail-limits (parameters state colony new-cost)
  "Update the values for the trail limits according to a new best tour found."
  (let* ((old-max (colony-trail-max colony))
	 (new-max (update-trail-max-value parameters new-cost))
	 (new-min (update-trail-min-value parameters state old-max)))
    (when (> new-min new-max)
      (setf new-max new-min))
    (setf (colony-trail-max colony) new-max)
    (setf (colony-trail-min colony) new-min)))


(defun update-trail-max-value (parameters cost)
  "Return initial pheromone value according to the Ant System."
  (case (parameters-ant-system parameters)
    (:mmas (/ 1.0 (* (parameters-rho parameters) cost)))
    (:self (/ 1.0 (* (parameters-rho parameters) cost)))
    (otherwise 1.0)))

(defun update-trail-min-value (parameters state trail-max)
  "Return initial lower bound pheromone value according to the Ant System."
  ;(declare (ignore state))
  (case (parameters-ant-system parameters)
    ;; reverted to previous formula. The 2nd drastically worsen the results (see below)
    (:mmas (let ((p-x (exp (/ (log 0.05) (parameters-n parameters))))
		 (avg (state-stagnation state)))
	     (* (/ (1- p-x)
		   (* p-x (1- avg)))
		trail-max)))
    (:self (let ((p-x (exp (/ (log 0.05) (parameters-n parameters))))
		 (avg (state-stagnation state)))
	     (* (/ (1- p-x)
		   (* p-x (1- avg)))
		trail-max)))
    (otherwise 1.0)))

;;; makes results worse on std mmas (see above)
; (let ((new-min 
;	(if (parameters-local-search parameters)
;	    (/ trail-max (* 2.0 (parameters-n parameters)))
;	    (let* ((p-x (exp (/ (log 0.05) (parameters-n parameters))))
;		   (avg (/ (1+ (parameters-n parameters)) 2)))
;	      (* (/ (1- p-x) (* p-x (1- avg))) trail-max)))))
;  (if (> new-min 0.0) new-min 0.0005)))
;;;


(defun initial-trail-value (parameters max)
  "Return initial pheromone value according to the Ant System."
  (case (parameters-ant-system parameters)
    (:mmas max)
    (:self max)
    (otherwise 1.0)))

(defun init-pheromone (n &optional (value 1.0))
  "Return a fresh pheromone matrix."
  (make-array `(,(1+ n) ,(1+ n)) 
	      :initial-element value
	      :element-type 'single-float))


;;;
;;; pheromone restart
;;;

(defun restart-pheromone-trails-mmas (parameters colony state stats worst)
  "Re-init the pheromone trails and keeps re-inits the restart-ant."
  (when (and (< (state-stagnation state)
		(parameters-stagnation-limit parameters))
	     (> (- (state-iterations state)
		   (statistics-restart-iteration stats))
		(parameters-restart-iterations parameters)))
    (apply-pheromone-restart (parameters-n parameters)
			     (colony-trail-max colony)
			     (colony-pheromone colony)
			     stats worst (state-iterations state) :pheromone)))

(defun apply-pheromone-restart (n max pheromone stats worst iteration type)
  (loop for i from 1 to n
	do (loop for j from 1 to n 
		 do (setf (aref pheromone i j) max)))
  (setf (statistics-restart-ant stats) (make-ant :tour-length worst))
  (if (eql type :pheromone) 
      (progn
	(setf (statistics-restart-iteration stats) iteration)
	(incf (statistics-restarts stats)))
      (progn
	(setf (statistics-restart-iteration-self stats) iteration)
	(incf (statistics-self-restarts stats)))))


;;;
;;; pheromone update
;;;    


;;
;; common functions

(defun update-trails (parameters colony stats state)
  "Update the pheromone and heuristic trails."
  (funcall (parameters-pheromone-update parameters)
	   parameters colony stats state))

(defun evaporate (n pheromone rho)
  "Removes some pheromone from the trails -- evaporation procedure."
  (loop for i from 1 to n
     do (loop for j from i to n
	   do (let ((value (* (- 1 rho) (aref pheromone i j))))
		(setf (aref pheromone i j) value
		      (aref pheromone J i) value)))))
 
(defun deposit-pheromone (ant n pheromone weight)
  "Deposit pheromone in the trail according to AS method."
  (let ((delta (/ weight (ant-tour-length ant))))
    (loop for i from 1 to n
       do (let* ((j (aref (ant-tour ant) i))
		 (l (aref (ant-tour ant) (1+ i)))
		 (value (+ (aref pheromone j l) delta)))
	    (setf (aref pheromone j l) value
		  (aref pheromone l j) value)))))

(defun update-choice-info (n pheromone heuristic choice-info alpha beta)
  "Updates the matrix with the pheromone and heuristic combined info."
  (loop for i from 1 to n
     do (loop for j from 1 to n
	   do (setf (aref choice-info (1- (+ (- (* n i) n) j)))
		    (* (expt (aref pheromone i j) alpha)
		       (if (> beta 0.0)
			   (expt (aref heuristic i j) beta)
			   1.0))))))

(defun update-choice-info-at (n pheromone heuristic choice-info alpha beta i j)
  "Updates the matrix with the pheromone and heuristic combined info."
  (let ((value (* (expt (aref pheromone i j) alpha)
		  (if (> beta 0.0)
		      (expt (aref heuristic i j) beta)
		      1.0))))
    (setf (aref choice-info (1- (+ (- (* n j) n) i))) value)
    (setf (aref choice-info (1- (+ (- (* n i) n) j))) value)))
    

;;
;; ant system

(defun as-pheromone-update (parameters colony stats state) 
  "Ant System pheromone update method."
  (declare (ignore stats state))
  (let ((ants (colony-ants colony))
	(n (parameters-n parameters))
	(pheromone (colony-pheromone colony))
	(rho (parameters-rho parameters))
	(heuristic (colony-heuristic colony))
	(choice-info (colony-choice-info colony))
	(alpha (parameters-alpha parameters))
	(beta (parameters-beta parameters)))
    (evaporate n pheromone rho)
    (loop for ant across ants
       do (deposit-pheromone ant n pheromone 1))
    (update-choice-info n pheromone heuristic choice-info alpha beta)))

;;
;; elitist ant system

(defun eas-pheromone-update (parameters colony stats state)
  "Elite Ant System pheromone update method."
  (declare (ignore state))
  (let ((ants (colony-ants colony))
	(n (parameters-n parameters))
	(pheromone (colony-pheromone colony))
	(rho (parameters-rho parameters))
	(heuristic (colony-heuristic colony))
	(choice-info (colony-choice-info colony))
	(alpha (parameters-alpha parameters))
	(beta (parameters-beta parameters))
	(e (parameters-n parameters))
	(best-so-far-ant (statistics-best-ant stats)))
    (evaporate n pheromone rho)
    (loop for ant across ants
       do (deposit-pheromone ant n pheromone 1))
    (deposit-pheromone best-so-far-ant n pheromone e)
    (update-choice-info n pheromone heuristic choice-info alpha beta)))


;;
;; rank ant system

(defun rank-pheromone-update (parameters colony stats state)
  "Rank Ant System pheromone update method."
  (declare (ignore state))
  (let* ((ants (colony-ants colony))
	 (n (parameters-n parameters))
	 (pheromone (colony-pheromone colony))
	 (rho (parameters-rho parameters))
	 (heuristic (colony-heuristic colony))
	 (choice-info (colony-choice-info colony))
	 (alpha (parameters-alpha parameters))
	 (beta (parameters-beta parameters))
	 (best-so-far-ant (statistics-best-ant stats))
	 (w 6)
	 (rank-ants (rank-w-ants (1- w) (colony-n-ants colony) ants)))
     (evaporate n pheromone rho)
     (loop for r from 1 below w 
	do (deposit-pheromone (aref rank-ants (1- r)) n pheromone (- w r)))
     (deposit-pheromone best-so-far-ant n pheromone w)
     (update-choice-info n pheromone heuristic choice-info alpha beta)))

(defun rank-w-ants (w n-ants ants)
  "Sort ants in increasong tour length and returns the w best."
  (let ((full-rank 
	 (sort (safe-copy-colony n-ants ants) #'< :key #'ant-tour-length))
	(w-rank (make-array w)))
    (loop for i from 0 below w 
       do (setf (aref w-rank i)
		(copy-ant (aref full-rank i)))
       finally (return w-rank))))

;; max-min
(defun mmas-pheromone-update (parameters colony stats state)
  (let* ((ants (colony-ants colony))
	 (n (parameters-n parameters))
	 (pheromone (colony-pheromone colony))
	 (rho (parameters-rho parameters))
	 (heuristic (colony-heuristic colony))
	 (choice-info (colony-choice-info colony))
	 (alpha (parameters-alpha parameters))
	 (beta (parameters-beta parameters))
	 (best-so-far-ant (statistics-best-ant stats)))
    (evaporate n pheromone rho)
    (if (funcall (parameters-scheduler parameters) parameters state)
	(let ((current-best-ant (find-best-ant (colony-n-ants colony) ants)))
	  (deposit-pheromone current-best-ant n pheromone 1))
	(progn 
	  (deposit-pheromone best-so-far-ant n pheromone 1)))
    (verify-pheromone-limits (parameters-n parameters)
			     (colony-pheromone colony)
			     (colony-trail-max colony)
			     (colony-trail-min colony))
    (update-choice-info n pheromone heuristic choice-info alpha beta)))

(defun change-update-ant (parameters state)
  "Change the ant for update acording to number of iterations. Return t or nil."
  (let* ((max (parameters-max-iterations parameters))
	 (mid (/ max 2))
	 (q3 (+ mid (/ max 4)))
	 (i (state-iterations state)))
    (or
     (< i q3)
     (> (mod i 50) 0))))

(defun verify-pheromone-limits (n pheromone max min)
  "Checks for violations of pheromone limits and updates according to max and min values."
  (loop for i from 1 to n
     do (loop for j from 1 to n 
	   when (< (aref pheromone i j) min)
	   do (setf (aref pheromone i j) min)
	   when (> (aref pheromone i j) max)
	   do (setf (aref pheromone i j) max))))

(defun find-best-ant (n-ants ants)
  "Return the current best ant in the colony."
  (loop 
     with current = (aref ants 0)
     for i from 1 below n-ants
     when (< (ant-tour-length (aref ants i)) 
	     (ant-tour-length current)) 
     do (setf current (aref ants i))
     finally (return current)))
