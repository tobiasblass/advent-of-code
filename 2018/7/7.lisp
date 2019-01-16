(in-package :aoc2018)

(defparameter *rules* (iter (for line in-file "7/input" using 'read-line)
			    (collect (map 'list
					  #'intern
					  (nth-value 1 (cl-ppcre:scan-to-strings
							"Step \(.\) must be finished before step \(.\) can begin."
							line))))))

(defun steps (rules)
  (sort (remove-duplicates (reduce #'append rules) :test 'eq)
	(lambda (a b) (string< (symbol-name a) (symbol-name b)))))
(defun predecessor-map (rules)
  (let ((map (make-hash-table :test 'eq)))
    (iter (for (req res) in rules)
	  (push req (gethash res map)))
    map))

(defun startable-step (steps predecessor-map)
  (find-if (compose #'not (lambda (step) (gethash step predecessor-map)))
	   steps))

(defun solution1 ()
  (iter (with pred-map = (predecessor-map *rules*))
	(for remaining-steps initially (steps *rules*) then (delete next-step remaining-steps))
	(for next-step = (startable-step remaining-steps pred-map))
	(while next-step)
	(maphash (lambda (key _) (deletef (gethash key pred-map) next-step)) pred-map)
	(collect next-step)))

(defun step-processing-time (step) (+ 61 (position step (steps *rules*))))

(defparameter *num-workers* 5)

(defstruct (worker-state (:constructor make-worker-raw))
  step
  remaining-work)

(defun make-worker (step)
  (make-worker-raw :step step :remaining-work (step-processing-time step)))
(defun worker-state-tick (state)
  (alet (copy-worker-state state)
    (decf (worker-state-remaining-work it))
    it))
(defun worker-done (state)
  (not (plusp (worker-state-remaining-work state))))

(defun partition (predicate seq)
  "Splits SEQ into two parts, those that fulfil PREDICATE and those that don't. Returns both as values."
  (values
   (remove-if-not predicate seq)
   (remove-if predicate seq)))

(defun updatehash (f table)
  "Like maphash, but sets the entry to the return value of f (which should not have side effects)"
  (maphash (lambda (k v) (setf (gethash k table) (funcall f k v))) table))
(defun solution2-tick (remaining-steps &optional worker-states predecessor-map (time 0))
  (unless predecessor-map
    (setf predecessor-map (predecessor-map *rules*)))
  (assert (<= (length worker-states) *num-workers*))
  
  (multiple-value-bind (finished-workers busy-workers)
      (partition #'worker-done worker-states)
    ;; Remove the finished steps from the predecessor map,
    ;; to potentially free up steps that waited for those steps
    (let ((finished-steps (mapcar #'worker-state-step finished-workers)))
      (updatehash (lambda (step preconditions)
		    (set-difference preconditions finished-steps))
		  predecessor-map))

    (acond
      ;; If we can assign a job, recurse without advancing time
      ((and (< (length busy-workers) *num-workers*)
	    (startable-step remaining-steps predecessor-map))
       (solution2-tick (remove it remaining-steps)
		       (cons (make-worker it) busy-workers)
		       predecessor-map
		       time))
      ;; Otherwise, if there are still busy workers step time
      (busy-workers
       (solution2-tick remaining-steps
		       (mapcar #'worker-state-tick busy-workers)
		       predecessor-map
		       (+ time 1)))
      ;; Otherwise, if there are not busy workers we are finished
      (t
       (assert (not remaining-steps))
       time))))

(defun solution2 ()
  (solution2-tick (steps *rules*)))


(deftest solution1-example
 (let ((*rules* '((C A) (C F) (A B) (A D) (B E) (D E) (F E))))
   (solution1))
  (C A B D F E))
(deftest solution2-example
    (let ((*rules* '((C A) (C F) (A B) (A D) (B E) (D E) (F E)))
	  (*num-workers* 2))
      (solution2))
  258)

(sb-rt:do-tests)

;; Solution 1
(coerce (mapcar (lambda (x) (coerce x 'character)) (solution1)) 'string)
;; Solution 2
(solution2)
