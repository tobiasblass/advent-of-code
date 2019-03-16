(in-package :aoc2018)

; a star is a position #(x y) and a velocity #(dx dy)
(defstruct star
  pos
  vel)

(defun star-position-at (star time)
  "extrapolates the position of star after the given number of time steps"
  (map 'vector
       (lambda (x dx) (+ x (* time dx)))
       (star-pos star) (star-vel star)))

(defun min&max (xs)
  "Returns the minimum and the maximum of xs"
  (iter (for x in xs)
	(maximize x into max)
	(minimize x into min)
	(finally (return (values min max)))))

; This function draws the stars, using the maximum distance between stars as a heuristic for whether the picture is "interesting"
(defun sketch-points (points &optional (max-delta-x 100) (max-delta-y 200))
  "Returns a sketch graphic of the points, unless they are more than max-delta-? apart"
  (multiple-value-bind (xmin xmax)
      (min&max (mapcar (lambda (p) (aref p 0)) points))
    (multiple-value-bind (ymin ymax)
	(min&max (mapcar (lambda (p) (aref p 1)) points))
    (when (and xmax xmin ymin ymax
	       (< (- xmax xmin) max-delta-x)
	       (< (- ymax ymin) max-delta-y))
      (sketch:defsketch sky ((unit 5))
	(iter (for p in points)
	      (sketch:rect (* unit (- (aref p 0) xmin)) (* unit (- (aref p 1) ymin)) unit unit)))
      (make-instance 'sky)))))
      

(let ((line-re (apply #'format nil
		      "position=<~A,~A> velocity=<~A,~A>"
		      (make-list 4 :initial-element " *([-0-9]*) *"))))
  (defun parse-line (line)
    (nth-value 1 (cl-ppcre:scan-to-strings line-re line))))
(defun parse-input (lines)
  (iter (for entry in lines)
	(for (x y dx dy) = (map 'list #'parse-integer (parse-line entry)))
	(collect (make-star :pos (vector x y) :vel (vector dx dy)))))

(defparameter *stars* (parse-input (read-lines "10/input")))

(let ((raw-input '(
"position=< 9,  1> velocity=< 0,  2>"
"position=< 7,  0> velocity=<-1,  0>"
"position=< 3, -2> velocity=<-1,  1>"
"position=< 6, 10> velocity=<-2, -1>"
"position=< 2, -4> velocity=< 2,  2>"
"position=<-6, 10> velocity=< 2, -2>"
"position=< 1,  8> velocity=< 1, -1>"
"position=< 1,  7> velocity=< 1,  0>"
"position=<-3, 11> velocity=< 1, -2>"
"position=< 7,  6> velocity=<-1, -1>"
"position=<-2,  3> velocity=< 1,  0>"
"position=<-4,  3> velocity=< 2,  0>"
"position=<10, -3> velocity=<-1,  1>"
"position=< 5, 11> velocity=< 1, -2>"
"position=< 4,  7> velocity=< 0, -1>"
"position=< 8, -2> velocity=< 0,  1>"
"position=<15,  0> velocity=<-2,  0>"
"position=< 1,  6> velocity=< 1,  0>"
"position=< 8,  9> velocity=< 0, -1>"
"position=< 3,  3> velocity=<-1,  1>"
"position=< 0,  5> velocity=< 0, -1>"
"position=<-2,  2> velocity=< 2,  0>"
"position=< 5, -2> velocity=< 1,  2>"
"position=< 1,  4> velocity=< 2,  1>"
"position=<-2,  7> velocity=< 2, -2>"
"position=< 3,  6> velocity=<-1, -1>"
"position=< 5,  0> velocity=< 1,  0>"
"position=<-6,  0> velocity=< 2,  0>"
"position=< 5,  9> velocity=< 1, -2>"
"position=<14,  7> velocity=<-2,  0>"
		   "position=<-3,  6> velocity=< 2, -1>")))
  (iter (for time from 0 to 4)
	(sketch-points (mapcar (rcurry #'star-position-at time) (parse-input raw-input)))
	(sleep 1)))

; Solution 1: CONTINUE through the breaks, until legible text shows up. You can figure out the time from the debugger
(iter (for time from 0)
      (when (sketch-points (mapcar (rcurry #'star-position-at time) *stars*))
	(break)))
