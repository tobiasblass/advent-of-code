(ql:quickload :iterate)
(use-package :iterate)
(defun read-frequencies (file)
  (iter (for l in-file file using #'read-line)
	(collect (parse-integer l))))

(defun sum-frequencies (frequencies)
  (reduce #'+ frequencies))

(defun first-cycle (frequencies)
  (iter	(for next-freqs = (or (cdr next-freqs) frequencies)) ; move next-freqs in a cir
	(for f = (car next-freqs))
	(for seen initially nil then (cons cur seen))
	(for cur initially 0 then (+ cur f))
	(finding cur such-that (member cur seen)))))

(assert (= (sum-frequencies '(0 1 2 -1)) 2))
(assert (= (sum-frequencies '()) 0))
(sum-frequencies (read-frequencies "input"))

(assert (= (first-cycle '(1 -1)) 0))
(assert (= (first-cycle '(3 3 4 -2 -4)) 10))
(assert (= (first-cycle '(-6 3 8 5 -6)) 5))
(assert (= (first-cycle '(7 7 -2 -7 -4)) 14))

;; TODO there is some optimization potential, I think
;; * (time (first-cycle (read-frequencies "input")))
;; Evaluation took:
;;  28.866 seconds of real time
;;  28.826103 seconds of total run time (28.825998 user, 0.000105 system)
;;  99.86% CPU
;;  74,820,715,906 processor cycles
;;  2,359,296 bytes consed
(first-cycle (read-frequencies "input"))
