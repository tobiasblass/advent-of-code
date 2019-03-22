(in-package :aoc2018)

(defparameter *grid-serial* 1955)

(defun cell-power (x y)
  "Returns the power level of the cell"
  (let ((rack (+ x 10)))
    (- 
     (rem
      (truncate (* (+ (* rack y)
		      *grid-serial*)
		   rack)
		100)
      10)
     5)))

(assert (=
	 (let ((*grid-serial* 8))
	   (cell-power 3 5))
	 4))
(assert (=
	 (let ((*grid-serial* 57))
	   (cell-power 122 79))
	 -5))
(assert (=
	 (let ((*grid-serial* 39))
	   (cell-power 217 196))
	 0))
(assert (=
	 (let ((*grid-serial* 71))
	   (cell-power 101 153))
	 4))

(defun square (x y &key (xlen 3) (ylen 3))
  "Returns a list of cells in the square with (x,y) in the top-left corner"
  (iter (for dx from 0 below xlen)
	(nconcing (iter (for dy from 0 below ylen)
			(collect (list (+ x dx) (+ y dy)))))))

(defun cells-power (cells)
  (reduce #'+ (mapcar (lambda (coord) (apply #'cell-power coord)) cells)))

(assert (= (let ((*grid-serial* 18))
	     (cells-power (square 33 45)))
	   29))
(assert (= (let ((*grid-serial* 42))
	     (cells-power (square 21 61)))
	   30))

;; The cached powers array, and the serial number that belongs to it
(let (powers serial)
  (defun cache-powers ()
   (setf powers (make-array '(300 300)))
    (setf serial *grid-serial*)
    (iter (for i from 0 below 300)
	  (iter (for j from 0 below 300)
		(setf (aref powers i j) (cell-power (+ 1 i) (+ 1 j))))))
  (defun power-at-idx (x y)
    "Returns the power at the (zero-based) position (x, y). Caches the power level between calls"
    (unless (and powers (= serial *grid-serial*))
      (cache-powers))
    (aref powers x y))
  (defun power-at (x y)
    "Returns the power at the (one-based) position (x, y). Caches the power level between calls"
    (power-at-idx (- x 1) (- y 1))))

(declaim (ftype (function (fixnum fixnum) (integer -5 4)) power-at-idx))

(defun power-in-square (x y size)
  (iter (for i from x below (+ x size))
	(sum (iter (for j from y below (+ y size))
		   (sum (power-at i j))))))

(defun most-powerful-square (&key (size 3))
  (iter (with max = (- 300 size))
	(for i from 0 to (* max max))
	(for (values x-1 y-1) = (truncate i max))
	(for x = (+ 1 x-1))
	(for y = (+ 1 y-1))
	(finding (cons x y)
		 maximizing (power-in-square x y size))))

(assert (equal (let ((*grid-serial* 18)) (most-powerful-square))
	       '(33 . 45)))
(assert (equal (let ((*grid-serial* 42)) (most-powerful-square))
	       '(21 . 61)))

	  
;; Solution 1
(most-powerful-square)

;; Solution 2

(defvar *sqsum-cache* (make-array '(301 301) :initial-element 0))

;; sqsum contains for each index (i j) the overall power of the rectangle at (1, 1) with length (i j)
(defun sqsum-at-idx/noinval (x y)
  "Returns the squaresum at index (x y), without checking for cache invalidation"
  (if (and (<= 0 x 300) (<= 0 y 300))
      (aref *sqsum-cache* x y)
      0))

(defun cache-sqsum ()
  (setf (get '*sqsum-cache* 'serial) *grid-serial*)
  (iter (for i from 0 below 300)
	(iter (for j from 0 below 300)
	      (setf (aref *sqsum-cache* i j) (+ (power-at-idx i j)
						(sqsum-at-idx/noinval i (- j 1))
						(sqsum-at-idx/noinval (- i 1) j)
						(- (sqsum-at-idx/noinval (- i 1) (- j 1))))))))

(defun sqsum-at-idx (x y)
  (unless (equal (get '*sqsum-cache* 'serial) *grid-serial*)
    (cache-sqsum))
  (sqsum-at-idx/noinval x y))

(defun sqsum-at (x y)
  (sqsum-at-idx (- x 1) (- y 1)))

(defun sqsum (x y size)
  "Returns the power sum of the square starting at (x y) of size SIZE"
  (let ((xmax (+ x size -1))
	(ymax (+ y size -1)))
    ;; Our cache contains the precomputed sums starting at the origin (1 1).
    ;; If X is the square we are looking for, we can separate the 300x300 square like this:
    ;; ABC
    ;; DXE
    ;; FGH
    ;; We can thus take the sum-starting-at-origin in the lower right corner of X (yielding the sum ABDX), and substract AD and AB. However, sice this counts A twice, we have to add it again at the end
    (+ 
     (- (sqsum-at xmax ymax)
	(sqsum-at (- x 1) ymax)
	(sqsum-at xmax (- y 1)))
     (sqsum-at (- x 1) (- y 1)))))
	

(defun most-powerful-square/anysize ()
  (iter (for i from 1 to (* 300 300 300))
	(for (values coor size) = (truncate i 300))
	(for (values xidx yidx) = (truncate coor 300))
	(for x = (+ 1 xidx))
	(for y = (+ 1 yidx))
	(when (and (<= (+ x size) 300)
		   (<= (+ y size) 300))
	  (finding (list x y size) maximizing (sqsum x y size)))))

#+nil
(progn
  (sb-profile:profile most-powerful-square power-in-square)
  (sqsum 2 2 15))

(assert (equal (let ((*grid-serial* 18)) (sqsum 90 269 16))
	       113))

(let ((*grid-serial* 18)) (sqsum 300 2 299)
(assert (equal (let ((*grid-serial* 18)) (most-powerful-square/anysize))
	       (list 90 269 16)))

; Solution 2
(most-powerful-square/anysize)
