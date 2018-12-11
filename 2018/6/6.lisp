(in-package :aoc2018)
(defun manhattan-distance (point1 point2)
  (apply #'+ (map 'list (compose #'abs #'-) point1 point2)))

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


(defun bounding-box (coords)
  "Returns the bounding box of coords as a pair of bounds ((xmin xmax) (ymin ymax))"
  (iterate (for point in coords)
	   (for x = (elt point 0))
	   (for y = (elt point 1))
           (minimizing x into x-min)
           (minimizing y into y-min)
           (maximizing x into x-max)
           (maximizing y into y-max)
           (finally (return `((,x-min ,x-max)
			      (,y-min ,y-max))))))

(defstruct (distance-map (:constructor make-distance-map-raw))
  closest-neighbour-map ; maps a point #(x y) to the list (coordinate distance) pointing to the nearest neighbour
  xbounds
  ybounds)

(defun map-distance-map! (f map)
  "Invokes f on every point of the distance map, updating the distance value with it's return value.
   f should take a point and the previous value of the map entry."
  (with-slots (xbounds ybounds closest-neighbour-map) map
    (iter (for x from (first xbounds) to (second xbounds))
	  (iter (for y from (first ybounds) to (second ybounds))
		(for point = (vector x y))
		(slet (gethash point closest-neighbour-map)
		  (setf it (funcall f point it)))))))

(defun map-distance-map (f map)
  "Like map-distance-map!, but does not update the map"
  (map-distance-map! (lambda (point prev) (funcall f point prev) prev) map))

(defun distance-map-add-chronal (map coordinate)
  (with-slots (xbounds ybounds closest-neighbour-map) map
    (assert (<= (first xbounds) (elt coordinate 0) (second xbounds)))
    (assert (<= (first ybounds) (elt coordinate 1) (second ybounds)))
    (map-distance-map!
     (lambda (point prev)
       (let ((distance (manhattan-distance point coordinate)))
	 (cond
	   ((or (not prev) (> (second prev) distance))
	    (list coordinate distance))
	   ((= (second prev) distance)
	    (list nil distance))
	   ((< (second prev) distance)
	    prev))))
     map)))
	     

(defun make-empty-distance-map (chronal-coordinates)
  "Given the chronal coordinates, returns a map mapping points to their closest chronal coordinate"
  ;; Cut out a bounding rectangle that contains all points. Optimal would be a bounding circle, but that is harder to compute.
  (let ((bounds (bounding-box chronal-coordinates)))
    (make-distance-map-raw :xbounds (first bounds)
			   :ybounds (second bounds)
			   :closest-neighbour-map (make-hash-table :test #'equalp))))

(defun make-distance-map (chronal-coordinates)
  (let ((map (make-empty-distance-map chronal-coordinates)))
    (mapc (curry #'distance-map-add-chronal map) chronal-coordinates)
    map))

(defun infinite-area-chronals (distance-map)
  (with-slots (xbounds ybounds) distance-map
    (let (infinites)
      (map-distance-map
       (lambda (point entry)
	 (when (or (member (elt point 0) xbounds)
		   (member (elt point 1) ybounds))
	   ;; If the point is on the edge
	   (push (first entry) infinites)))
       distance-map)
      (remove-duplicates infinites :test #'equalp))))

(defun solution1 ()
  (let ((distance-map (make-distance-map *input*))
	(area-sizes (make-hash-table :test #'equalp)))
    (map-distance-map
     (lambda (point value)
       (let ((chronal (first value)))
	 (when chronal
	   (incf (gethash chronal area-sizes 0)))))
     distance-map)
    (let ((infinites (infinite-area-chronals distance-map)))
      (reduce #'max
       (mapcar #'cdr
	       (remove-if (rcurry #'member infinites) 
			  (hash-table-alist area-sizes)
			  :key #'car))))))
       
(deftest solution1-example
    (let ((*input* '( #(1 1)
		     #(1 6)
		     #(8 3)
		     #(3 4)
		     #(5 5)
		     #(8 9))))
      (solution1))
  17)
      

      

	  
