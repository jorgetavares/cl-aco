(in-package #:cl-aco)

;;;;
;;;; File contains all the generic functions for pheromone init and 
;;;; update, as well as the specific functions for each ant system.
;;;;
;;;; Current implemented ant systems:
;;;;   :as       - Ant System (original and most simple AS)
;;;;   :eas      - Elit Ant System
;;;;   :ras      - Rank-based Ant System
;;;;   :mmas     - Min-Max Ant System
;;;;   :mmas-hcf - Min-Max Ant System with Hyper-Cube Framework 
;;;;


;;;
;;; pheromone setup
;;;

(defun update-trail-limits (parameters colony new-cost)
  "Update the values for the trail limits according to a new best tour found."
  (setf (colony-trail-max colony)
	(update-trail-max-value parameters new-cost))
  (setf (colony-trail-min colony)
	(update-trail-min-value parameters (colony-trail-max colony))))

(defun update-trail-max-value (parameters cost)
  "Return initial pheromone value according to the Ant System."
  (case (parameters-ant-system parameters)
    (:mmas (/ 1 (* (parameters-rho parameters) cost)))
    (:mmas-hcf 1)
    (otherwise 1)))

(defun update-trail-min-value (parameters trail-max)
  "Return initial lower bound pheromone value according to the Ant System."
  (case (parameters-ant-system parameters)
    (:mmas (/ trail-max (* 2 (parameters-n parameters))))
    (:mmas-hcf 0)
    (otherwise 1)))

(defun initial-trail-value (parameters max)
  "Return initial pheromone value according to the Ant System."
  (case (parameters-ant-system parameters)
    (:mmas max)
    (:mmas-hcf 0.5)
    (otherwise 1)))

(defun init-pheromone (n &optional (value 1))
  "Return a fresh pheromone matrix."
  (make-array `(,(1+ n) ,(1+ n)) :initial-element value))


;;;
;;; pheromone restart
;;;

(defun restart-pheromone-trails-mmas (parameters colony state stats)
  "Re-init the pheromone trails and keeps re-inits the restart-ant."
  (when (and (< (state-stagnation state)
		(parameters-stagnation-limit parameters))
	     (> (- (state-iterations state)
		   (statistics-restart-iteration stats))
		(parameters-restart-iterations parameters)))
    (let ((n (parameters-n parameters))
	  (max (colony-trail-max colony))
	  (pheromone (colony-pheromone colony)))
      (loop for i from 1 to n
	 do (loop for j from 1 to n 
	       do (setf (aref pheromone i j) max)))
      (setf (statistics-restart-ant stats) (make-ant :tour-length 10000000000))
      (setf (statistics-restart-iteration stats) 0)
      (incf (statistics-restarts stats)))))

(defun restart-pheromone-trails-mmas-hcf (parameters colony state stats)
  "Re-init the pheromone trails and keeps re-inits the restart-ant."
  (if (and (state-bs-update state) 
	   (> (state-cf state) 0.99))
      (let ((n (parameters-n parameters))
	    (pheromone (colony-pheromone colony)))
	(loop for i from 1 to n
	   do (loop for j from 1 to n 
		 do (setf (aref pheromone i j) 0.5)))
	(setf (statistics-restart-ant stats) 
	      (make-ant :tour-length 10000000000))
	(setf (statistics-restart-iteration stats) 0)
	(incf (statistics-restarts stats))
	(setf (state-bs-update state) nil))
      (when (> (state-cf state) 0.99)
	(setf (state-bs-update state) t))))


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
	   do (setf (aref choice-info i j)
		    (* (expt (aref pheromone i j) alpha) 
		       (expt (aref heuristic i j) beta))))))

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

;;
;; Min-Max ant system

(defun mmas-pheromone-update (parameters colony stats state)
  "Mïn-Max Ant System pheromone update method."
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
   (if (change-update-ant parameters state)
       (let ((current-best-ant (find-best-ant (colony-n-ants colony) ants)))
	 ;(format t "mmas deposit current best ~%")
	 (deposit-pheromone current-best-ant n pheromone 1))
       (progn 
	 ;(format t "mmas deposit best so far ~%")
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

;;
;; MMAS with hyper-cube framework

(defun mmas-hcf-pheromone-update (parameters colony stats state)
  "Mïn-Max Ant System with Hyper Cube framework pheromone update method."
 (let* ((ants (colony-ants colony))
	(n (parameters-n parameters))
	(pheromone (colony-pheromone colony))
	(rho (parameters-rho parameters))
	(heuristic (colony-heuristic colony))
	(choice-info (colony-choice-info colony))
	(alpha (parameters-alpha parameters))
	(beta (parameters-beta parameters))
	(cf (state-cf state))
	(restart-ant (statistics-restart-ant stats))
	(best-so-far-ant (statistics-best-ant stats)))
   (evaporate n pheromone rho)
   (if (state-bs-update state)
       (let ((current-ant (find-best-ant (colony-n-ants colony) ants)))
	 (cond ((< cf 0.04)
		(deposit-pheromone current-ant n pheromone 1))
	       ((< cf 0.06)
		(deposit-pheromone current-ant n pheromone 2/3)
		(deposit-pheromone restart-ant n pheromone 1/3))
	       ((< cf 0.08)
		(deposit-pheromone current-ant n pheromone 1/3)
		(deposit-pheromone restart-ant n pheromone 2/3))
	       (t (deposit-pheromone restart-ant n pheromone 1))))
       (deposit-pheromone best-so-far-ant n pheromone 1))
   (verify-pheromone-limits (parameters-n parameters)
			    (colony-pheromone colony)
			    (colony-trail-max colony)
			    (colony-trail-min colony))
   (update-choice-info n pheromone heuristic choice-info alpha beta)))
