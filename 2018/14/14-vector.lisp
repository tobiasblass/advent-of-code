(in-package :aoc2018)
(defparameter *input* 681901)

(defun digit-list (n)
  (if (= n 0)
      (list 0)
      (iter (for (values div rem) = (truncate n 10))
	    (collect rem at beginning)
	    (until (zerop div))
	    (setf n div))))
(defun digits->num (digits)
  (iter (for d in-sequence digits)
	(reducing d by (lambda (n d) (+ (* 10 n) d)))))

(assert (equal (digit-list 1234567) '(1 2 3 4 5 6 7)))
(assert (= (digits->num '(1 2 3 4 5 6)) 123456))
(assert (equal (digit-list 0) '(0)))
(assert (= (digits->num '(0)) 0))

(let ((scores (make-array '(2) :adjustable t :fill-pointer 2
			  :initial-contents '(3 7) 
			  :element-type '(integer 0 9)))
      (search-pattern (digit-list *input*)))
  (assert (> *input* (length search-pattern)))
  (iter (for elf-1 initially 0 then (rem (+ elf-1 (aref scores elf-1) 1) (length scores)))
	(for elf-2 initially 1 then (rem (+ elf-2 (aref scores elf-2) 1) (length scores)))
	(for new-els = (digit-list (+ (aref scores elf-1) (aref scores elf-2))))
	(mapc (lambda (x) (vector-push-extend x scores *input*)) new-els)
	(for len = (length scores))
	;; solution two
	(with last-digit = (lastcar search-pattern))
	(for pattern-start = (- len (length search-pattern) 1))
	(until (and (>= len (+ *input* 10))
		    (= (lastcar new-els) last-digit)
		    (every #'=
			   search-pattern
			   (subseq scores pattern-start len))))
	(finally (return (values (digits->num (subseq scores *input* (+ *input* 10)))
				 pattern-start)))))


