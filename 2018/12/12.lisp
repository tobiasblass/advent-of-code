(in-package :aoc2018)

(defstruct (state (:constructor make-state (alive &optional (first-pot-num 0)))
		  (:copier nil))
  (alive "" :type string)
  (first-pot-num 0 :type fixnum))

(defun parse-state (str)
  (make-state str))

(defparameter *initial-state*
  (let ((line (with-open-file (in "12/input")  (read-line in))))
    (register-groups-bind (state) ("initial state: \([.#]*\)" line)
      (parse-state state))))

(defparameter *rules*
  (iter (for line in-file "12/input" using #'read-line)
	(register-groups-bind (con res) ("\([#.]{5}\) => \(.\)" line)
	  (collect (cons con (character res))))))

; Weird things happen when plants spawn from nothing
(assert (char= (cdr (assoc "....." *rules* :test #'equal)) #\.))

(defun elt-or (sequence index &key default)
  (if (< -1 index (length sequence))
      (elt sequence index)
      default))

(defun subseq-or (sequence start &key (end (length sequence)) default)
  (iter (for i from start below end)
	(collect (elt-or sequence i :default default))))

(defun prune (state)
  "Removes redundant dead plants from the front and the back"
  (with-slots (alive) state
    (let ((left-trimmed (string-left-trim "." alive)))
      (incf (state-first-pot-num state) (- (length alive) (length left-trimmed)))
      (setf alive (string-right-trim "." left-trimmed))))
  state)

(defun state-step (state)
  (declare (optimize (speed 3) (debug 0) (safety 1)))
  (let ((result (make-state (make-string (+ 2 (length (state-alive state)) 2))
			    (- (state-first-pot-num state) 2))))
    (iter (for i index-of-vector (state-alive result))
	  (for i-old = (- i 2)) ; The old list is shifted by two
	  (for surr = (coerce (subseq-or (state-alive state) (- i-old 2) :end (+ i-old 3) :default #\.)
			      'string))
	  (for rule = (assoc surr *rules* :test #'equal))
	  (assert rule)
	  (setf (aref (state-alive result) i) (cdr rule)))
    (prune result)))
      
(defun sum-plants (state)
  (with-slots (alive first-pot-num) state
    (iter (for s in-vector alive with-index i)
	  (when (char= s #\#)
	    (sum (+ first-pot-num i))))))

(assert (= (sum-plants (make-state ".#....##....#####...#######....#.#..##." -3)) 325))

(defun solution1 (&optional (cnt 20))
  (sum-plants (iter (repeat cnt)
		    (for s initially *initial-state* then (state-step s))
		    (finally (return s)))))

;; For solution 2, we have to find a pattern or, alternatively, compute 50 steps per nanosecond.
;; compute the deltas for the first 100 steps
(defvar *deltas*
  (maplist
   (lambda (l) (if (second l) (- (second l) (first l))))
   (mapcar #'solution1 (iter (for i from 1 to 200) (collect i)))))


(defvar *stable-point*
  (iter (for d on *deltas*)
	(for idx from 1)
	(finding idx such-that (= (first d) (second d)))))

(defun solution2 ()
  (let ((last (solution1 *stable-point*))
	(delta (nth *stable-point* *deltas*)))
    (+ last (* delta (- 50000000000 *stable-point*)))))
