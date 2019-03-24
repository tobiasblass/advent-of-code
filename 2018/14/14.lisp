(in-package :aoc2018)

(eval-when (:load-toplevel :execute)
  (setq *print-circle* t))

(defparameter *input* 681901)

(defun digit-list-rev* (n)
  "Helper function for digit-list. Returns a reversed digit list of n. Note: returns the empty list for n=0."
  (if (zerop n)
      '()
      (multiple-value-bind (div rem) (truncate n 10)
	(cons rem (digit-list-rev* div)))))
(defun digit-list (n)
  (if (zerop n)
      (list 0)
      (nreverse (digit-list-rev* n))))
(defun digits->num (digits &optional (acc 0))
  (if digits
      (digits->num (cdr digits) (+ (* 10 acc) (car digits)))
      acc))

(assert (equal (digit-list 1234567) '(1 2 3 4 5 6 7)))
(assert (= (digits->num '(1 2 3 4 5 6)) 123456))
(assert (equal (digit-list 0) '(0)))
(assert (= (digits->num '(0)) 0))
	
(defun circularize-list (list)
  (setf (cdr (last list)) list))

;; We cache the earlier results of last/circular, so we only have to walk over the newly-added elements.
;; list is almost always the score list, so this should give a nice speedup
(let ((cache (make-hash-table :test 'eq :weakness :key-and-value)))
  (defun last/circular (list &optional (n 1))
    "Returns the nth-last cons of a circular list, where the last cons is the one that circles back to the beginning"
    (let ((start (or (gethash list cache) list)))
      (setf (gethash list cache)
	    (iter (for cons on start)
		  (finding cons such-that (eq (nthcdr n cons) list)))))))
(defun append/circular (circle list)
  "Appends list to the circular list circle,i.e., the circle is broken, list is appended, and the end of list is directed back to circle"
  (let ((last (last/circular circle)))
    (setf (cdr last) list)
    (setf (cdr (last list)) circle)))

(defun length/circular (circle)
  (iter (for cons on circle)
	(for len from 1)
	(finding len such-that (eq (cdr cons) circle))))

(defun combine (elf-1 elf-2)
  (+ (car elf-1) (car elf-2)))
;; elfs are conses in the scores-list
(defun step-elves (elf-1 elf-2 scores)
  "Given a circular scoreboard list, Combines the two elves (i.e. conses in scores) and returns the two new elves"
  (append/circular scores
		   (digit-list (combine elf-1 elf-2)))
  (values (nthcdr (+ 1 (car elf-1)) elf-1)
	  (nthcdr (+ 1 (car elf-2)) elf-2)))

(defun solution1 (num-skip)
  (iter (with scores = (circularize-list (list 3 7)))
	(for (values elf-1 elf-2)
	     initially (values scores (cdr scores))
	     then (step-elves elf-1 elf-2 scores))
	(for len first 0 then (+ len
				 (if (>= (combine elf-1 elf-2) 10)
				     2
				     1)))
	(until (> len (+ num-skip 10)))
	(finally (return (subseq (nthcdr num-skip scores) 0 10)))))

(assert (equal (solution1 9) (digit-list 5158916779)))
(assert (equal (solution1 5) (cons 0 (digit-list 124515891))))
(assert (equal (solution1 18) (digit-list 9251071085)))
(assert (equal (solution1 2018) (digit-list 5941429882)))

(digits->num (solution1 *input*))

(defparameter *search-delta* 1000)

(defun prefix-p (prefix list &key (test #'eql))
  (every test prefix list))

(assert (prefix-p '(1 2 3) '(1 2 3 4 5 6)))
(assert (not (prefix-p '(1 2 3) '(1 2 4 5 6))))

;; WARNING: this is truly awful code, and it apparently leaks memory like a sieve
;; (i.e. sbcl crashes if you run it too often)
;; However, it gives you a solution, and that's all that counts :-)
(defun solution2 (search-pattern)
  "Finds the number of recipes needed until search-pattern appears."
  ;; It does so by computing the sequence for *search-delta* steps, and
  ;;  then looking at these entries to find the pattern. This avoids walking through the list all the time.
  (iter (with scores = (circularize-list (list 3 7)))
	(with pattern = (digit-list search-pattern))
	(for iteration from 0)
	(for (values elf-1 elf-2)
	     initially (values scores (cdr scores))
	     then (step-elves elf-1 elf-2 scores))
	(when (= iteration *search-delta*)
	  (setf iteration 0)
	  (with last-stop-idx = 0)
	  (with last-stop = scores)
	  (iter (for i from last-stop-idx)
		(for list on last-stop)
		(until (eq (cdr list) scores))
		(for plist previous list)
		(when (prefix-p pattern list)
		  (return-from solution2 i))
		(finally (setf last-stop-idx (- i 1))
			 (setf last-stop plist))))))
		
(assert (= (solution2 51589) 9))
(assert (= (solution2 124515891) 6))
(assert (= (solution2 59414) 2018))

(solution2 *input*)
	
  
