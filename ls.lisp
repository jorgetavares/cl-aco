(in-package :cl-aco)

(defun apply-local-search (operator parameters colony)
  (loop for ant across (colony-ants colony)
     do (funcall operator (ant-tour ant) parameters)))

(defun reverse-2-opt (solution start end size)
  (unless (or (> start end) 
	      (> start size) 
	      (< end 0))
    (loop while (< start end)
       do (let ((tmp (aref solution start)))
	    (setf (aref solution start) (aref solution end))
	    (setf (aref solution end) tmp)
	    (incf start)
	    (decf end)))))

