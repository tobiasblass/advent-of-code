(in-package :aoc2018)
(defparameter *input* (iter (for n in-file "8/input" using 'read)
			    (collect n)))

(defstruct node
  children
  meta)

(defun parse (numbers)
  "Parses a tree from numbers and returns two values:
   The parsed tree, and the unprocessed part of numbers"
  (destructuring-bind (num-children num-meta &rest data) numbers
    (if (zerop num-children)
	(values (make-node :children nil :meta (subseq data 0 num-meta))
		(nthcdr num-meta data))
	(multiple-value-bind (child1 rest) (parse data)
	  (multiple-value-bind (child2-n rest) (parse (list* (- num-children 1) num-meta rest))
	    (values (make-node :children (cons child1 (node-children child2-n))
			       :meta (node-meta child2-n))
		    rest))))))

(defun sum-metadata (tree)
  (+ (reduce #'+ (node-meta tree))
     (reduce #'+ (mapcar #'sum-metadata (node-children tree)))))

(deftest ex1
    (sum-metadata
     (parse
      '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2)))
  138)

;; Solution 1
(sum-metadata (parse *input*))

(defun tree-value (tree)
  (cond
    ((not tree)
     0)
    ((node-children tree)
     (reduce #'+
	     (mapcar #'tree-value
		     (mapcar (rcurry #'nth (node-children tree))
			      (mapcar #'1- (node-meta tree))))))

    (t
     (reduce #'+ (node-meta tree)))))

(deftest ex2
    (tree-value
     (parse
      '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2)))
  66)

(sb-rt:do-tests)


(tree-value (parse *input*))	  
			   
	

