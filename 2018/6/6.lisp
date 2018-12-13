(in-package :aoc2018)

(defun point (x y) (vector x y))
(defun point-x (p) (elt p 0))
(defun point-y (p) (elt p 1))

(defun manhattan-distance (a b)
  (declare (type (vector integer) a b))
  (declare (optimize (speed 3)))
  (iter (for an in-vector a)
	(for bn in-vector b)
	(sum (abs (- an bn)))))

(deftest manhattan-distance
    (manhattan-distance #(0 0) #(3 5))
  8)

(defun read-input (fname)
  (mapcar (lambda (line)
	    (apply #'vector
		   (map 'list #'parse-integer
			(nth-value 1
				   (cl-ppcre:scan-to-strings "^\([0-9]*\), \([0-9]*\)$" line)))))
	  (iterate (for l in-file fname using #'read-line)
		   (collect l))))

(defparameter *input*  (read-input "6/input"))

(defun range (a b)
  "Constructs a range object representing the closed interval [a,b]"
  (cons a b))

(defun range-max (r) (cdr r))
(defun range-min (r) (car r))
(defun range-len (r) (+ 1 (- (range-max r) (range-min r))))

(defun in-range (val range)
  "Returns whether the given value is inside the number range"
  (<= (car range) val (cdr range)))
(defun strictly-in-range (val range)
  "Returns whether the given value is inside the number range, but not on the edge"
  (< (car range) val (cdr range)))
  
(defun range->list (range)
  (iter (for i from (car range) to (cdr range))
	(collect i)))

(defmacro-clause (FOR var IN-RANGE range)
  (with-gensyms (r)
    `(progn
       (with ,r = ,range)
       (for ,var from (range-min ,r) to (range-max ,r)))))
(defmacro-clause (FOR var IN-PRODUCT-RANGE ranges-list)
  (assert (= (length ranges-list) 2)) ; restriction for now
  (with-gensyms (r1 r2 tuple)
    `(progn
       (with ,r1 = ,(first ranges-list))
       (with ,r2 = ,(second ranges-list))
       (initially (setq ,tuple (vector (range-min ,r1) (- (range-min ,r2) 1))))
       (for ,tuple 
	    next (progn
		   (cond ((and (= (elt ,tuple 0) (range-max ,r1))
			       (= (elt ,tuple 1) (range-max ,r2)))
			  (terminate))
			 ((= (elt ,tuple 1) (range-max ,r2))
			  (incf (elt ,tuple 0))
			  (setf (elt ,tuple 1) (range-min ,r2)))
			 (t 
			  (incf (elt ,tuple 1))))
		   ,tuple))
       (for ,var = ,tuple))))

(deftest in-product-range-clause
    (iter (for (a b) in-product-range ((range 1 10) (range 2 10)))
	  (collect (list a b))
	  (maximize (* (- 5 a) b) into foobar))
  40)

(defun bounding-box (coords)
  "Returns the bounding box of coords as a pair of bounds ((xmin xmax) (ymin ymax))"
  (iterate (for point in coords)
	   (for x = (point-x point))
	   (for y = (point-y point))
           (minimizing x into x-min)
           (minimizing y into y-min)
           (maximizing x into x-max)
           (maximizing y into y-max)
           (finally (return (values (range x-min x-max)
				    (range y-min y-max))))))

(defstruct (distance-map (:constructor make-distance-map-raw))
  (map (make-array 0) :type (simple-array list)) ; maps a point #(x y) to a list of (coordinate distance) pairs
  xrange
  yrange)

(defun distance-map-ranges (map)
  (values (distance-map-xrange map) (distance-map-yrange map)))

(defun make-empty-distance-map (chronal-coordinates)
  "Given the chronal coordinates, returns a map mapping points to their closest chronal coordinate"
  ;; Cut out a bounding rectangle that contains all points. Optimal would be a bounding circle, but that is harder to compute.
  (multiple-value-bind (xbounds ybounds) (bounding-box chronal-coordinates)
    (make-distance-map-raw :xrange xbounds
			   :yrange ybounds
			   :map (make-array `(,(range-len xbounds) ,(range-len ybounds))
					    :element-type 'list
					    :initial-element nil))))

(defun distance-map-ref (map point)
  (declare (optimize (speed 3)))
    (multiple-value-call #'aref
	   (distance-map-map map)
	   (multiple-value-bind (xrange yrange) (distance-map-ranges map)
	     (values (- (point-x point) (range-min xrange))
		     (- (point-y point) (range-min yrange))))))
(defsetf distance-map-ref (map point) (val)
  (declare (optimize (speed 3)))
  (with-gensyms (%point)
    `(let ((,%point ,point))
       (setf (aref (distance-map-map ,map)
		   (- (point-x ,%point) (range-min (distance-map-xrange ,map)))
		   (- (point-y ,%point) (range-min (distance-map-yrange ,map))))
	     ,val))))


(defun map-distance-map! (f map)
  "Invokes f on every point of the distance map, updating the distance value with it's return value.
   f should take a point and the previous value of the map entry."
  (declare (type (function ((vector integer) list) list) f))
  (iter (for point in-product-range ((distance-map-xrange map) (distance-map-yrange map)))
	(slet (distance-map-ref map point)
	  (setf it (funcall f point it)))))

(defun map-distance-map (f map)
  "Like map-distance-map!, but does not update the map"
  (iter (for point in-product-range ((distance-map-xrange map) (distance-map-yrange map)))
	(funcall f point (distance-map-ref map point))))

(defun distance-map-add-chronal (map coordinate)
  (assert (every #'in-range coordinate (multiple-value-list (distance-map-ranges map))))
  (map-distance-map!
   (lambda (point prev)
     (cons (list coordinate (manhattan-distance point coordinate)) prev))
   map))

(defun make-distance-map (chronal-coordinates)
  (let ((map (make-empty-distance-map chronal-coordinates)))
    (mapc (curry #'distance-map-add-chronal map) chronal-coordinates)
    map))

(defun unique-extremum (sequence predicate &key (key #'identity) (start 0) end)
  "Returns the element that would be first in the result if sequence was sorted with the given parameters, but only, if it is unique (with equality defined as (and (not (funcall predicate x y)) (not (funcall predicate y x))))"
  (let ((sorted (sort (subseq sequence start end) predicate :key key)))
    (when (funcall predicate
		       (funcall key (elt sorted 0))
		       (funcall key (elt sorted 1)))
      (elt sorted 0))))
		       
(defun infinite-area-chronals (distance-map)
  (let (infinites)
    (map-distance-map
     (lambda (point distances)
       (unless (every #'strictly-in-range point (multiple-value-list (distance-map-ranges distance-map)))
	 ;; If the point is on the edge
	 (awhen (unique-extremum distances #'< :key #'second)
	   (push (first it) infinites))))
     distance-map)
    (remove-duplicates infinites :test #'equalp)))

(defun solution1 ()
  (let ((distance-map (make-distance-map *input*))
	(area-sizes (make-hash-table :test #'equalp)))
    (map-distance-map
     (lambda (point distances)
       (declare (ignore point))
       (awhen (unique-extremum distances #'< :key #'second)
	 (incf (gethash (first it) area-sizes 0))))
     distance-map)
    (let ((infinites (infinite-area-chronals distance-map)))
      (cdr (extremum (remove-if (rcurry #'member infinites) 
			   (hash-table-alist area-sizes)
			   :key #'car)
		#'>
		:key #'cdr)))))
       
(deftest solution1-example
    (let ((*input* '( #(1 1)
		     #(1 6)
		     #(8 3)
		     #(3 4)
		     #(5 5)
		     #(8 9))))
      (solution1))
  17)
