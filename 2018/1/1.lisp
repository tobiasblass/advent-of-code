(ql:quickload :iterate)
(use-package :iterate)
(defun read-frequencies (file)
  (iter (for l in-file file using #'read-line)
	(collect (parse-integer l))))

(defun sum-frequencies (frequencies)
  (reduce #'+ frequencies))

(defun first-cycle (frequencies)
  (iter (with seen = (make-hash-table))
	(for next-freqs = (or (cdr next-freqs) frequencies)) ; move next-freqs in a circle
	(for f = (car next-freqs))
	(for state initially 0 then (progn
				      (setf (gethash state seen) t)
				      (+ state f)))
	(finding state such-that (gethash state seen))))

(assert (= (sum-frequencies '(0 1 2 -1)) 2))
(assert (= (sum-frequencies '()) 0))
(sum-frequencies (read-frequencies "input"))

(assert (= (first-cycle '(1 -1)) 0))
(assert (= (first-cycle '(3 3 4 -2 -4)) 10))
(assert (= (first-cycle '(-6 3 8 5 -6)) 5))
(assert (= (first-cycle '(7 7 -2 -7 -4)) 14))

(first-cycle (read-frequencies "input"))
