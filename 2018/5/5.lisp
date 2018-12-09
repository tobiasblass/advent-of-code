(in-package :aoc2018)

(defun opposite-case? (a b)
  (assert (every #'both-case-p (list a b)))
  (cond
    ((upper-case-p a) (lower-case-p b))
    ((lower-case-p a) (upper-case-p b))))
(defun annihilating-units (a b)
  (and (opposite-case? a b)
       (char= (char-upcase a)
	      (char-upcase b))))

(defvar *react-expensive-asserts* nil "Perform expensive assertions during react")
(defun react (tail &optional reversed-head)
  "Reduce the given polymer (append (reverse reversed-head) tail).
   reversed-head is guaranteed to not contain any annihilating units"
  ;; move elements from tail to reversed-head. If they match, annihilate them.
  (when *react-expensive-asserts*
    ; Check if reversed-head contains any annihilating pairs
    (assert (not (find t
		       (maplist (lambda (list)
				  (when (cadr list)
				    (annihilating-units (car list) (cadr list))))
				reversed-head)))))
  (cond
    ((not tail)
     (nreverse reversed-head))
    ((and reversed-head
	  (annihilating-units (car tail) (car reversed-head)))
     (react (cdr tail) (cdr reversed-head)))
    (t (react (cdr tail) (cons (car tail) reversed-head)))))
     
(defun string->list (s)
  (map 'list #'identity s))
(defun list->string (l)
  (map 'string #'identity l))

(deftest trivial-react-tests
    (mapcar (compose #'list->string #'react #'string->list) '("aA" "abBA" "abAB" "aabAAB"))
  ("" "" "abAB" "aabAAB"))

(deftest larger-example
    (list->string (react (string->list "dabAcCaCBAcCcaDA")))
  "dabCBAcaDA")
    
(sb-rt:do-tests)

(defvar *input* (string->list
		 (remove #\newline
			 (read-file-into-string "5/input"))))
(defvar *solution1*
  (length (react *input*)))

(defun polymer-units (polymer)
  "Returns a list of (lower-case) units contained in the polymer"
  (remove-duplicates (mapcar #'char-downcase polymer)))
(defun remove-unit (unit polymer)
  "Returns a polymer with  all (upper- and lowercase) instances of unit removed"
  (remove (char-downcase unit) polymer :key #'char-downcase))

(defun solution2-unit ()
  "Returns the unit that, when removed, yields the shortest polymer"
  (iter (for unit in (polymer-units *input*))
	(finding unit minimizing (length (react (remove-unit unit *input*))))))

(deftest test-solution2
    (let ((*input* (string->list "dabAcCaCBAcCcaDA")))
      (solution2-unit))
  #\c)

(defvar *solution2*
  (length (react (remove-unit (solution2-char) *input*))))
