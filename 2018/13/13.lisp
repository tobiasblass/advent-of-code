(in-package :aoc2018)

(defstruct track
  dir
  occupant)


(defvar *directions* '(left up right down left) "The movement directions, in clockwise order. The first item is mentioned twice, so any unique element always has a successor.")
(defvar *turn-directions* '(left straight right left) "The turning directions, in cart-decision order. The first item is mentioned twice, so any unique element always has a successor.")

(defun direction-vector (dir)
  "Returns the 2-D vector corresponding to the given direction"
  (ecase dir
    (left #(-1 0))
    (right #(1 0))
    (up #(0 -1))
    (down #(0 1))))

(defgeneric turn (cur turn-dir)
  (:documentation "Returns the direction of applying turn-dir (left, right, or straight) to cur"))

(defmethod turn (cur (turn-dir (eql 'right)))
  (second (member cur *directions*)))
(defmethod turn (cur (turn-dir (eql 'left)))
  (iter (for x on *directions*)
	(finding (first x) such-that (eq (second x) cur))))
(defmethod turn (cur (turn-dir (eql 'straight)))
    cur)

(assert (eq (turn 'down 'right) 'left))
(assert (eq (turn 'left 'left) 'down))
(assert (eq (turn 'up 'straight) 'up))
	
(defstruct cart
  dir
  pos
  (next-turn (first *turn-directions*)))
(defun parse-cart (chr pos)
  "Returns a cart object corresponding to character \"chr\" at the given position. You usually do not want to call this directly; call parse-track-char instead"
  (make-cart :dir (ecase chr
		    (#\^ 'up)
		    (#\v 'down)
		    (#\> 'right)
		    (#\< 'left))
	     :pos pos))

(defun parse-track-char (chr pos)
  "Returns a track object corresponding to character \"chr\" at the given position. The track might be empty
 or might contain a cart."
  (if (char= chr #\ )
      nil
      (make-track
       :dir (ecase chr
	      (#\/ 'right)
	      (#\\ 'left)
	      ((#\^ #\v #\|) 'up)
	      ((#\- #\> #\<) 'across)
	      (#\+ 'intersection))
       :occupant (if (find chr "^v><")
		     (parse-cart chr pos)))))
		 
(defun track-turn (cart track)
  "Returns the new direction of cart when moving on track. As a side effect, changes the cart's next-turn slot at an intersection"
  (ecase (track-dir track)
    (right (if (member (cart-dir cart) '(up down)) 'right 'left))
    (left (if (member (cart-dir cart) '(up down)) 'left 'right))
    ((up across) 'straight)
    (intersection (with-slots (next-turn) cart
		    (prog1
			next-turn
		      (assert (second (member next-turn *turn-directions*)))
		      (setf next-turn
			    (second (member next-turn *turn-directions*))))))))

;; The tracks and carts. These are usually bound by with-tracks-from.
(defparameter *tracks* nil)
(defparameter *carts* nil)

(defun parse-track-file (file)
  "Parses the given file and returns the list of tracks (containing the carts) and the list of carts"
  (let ((tracks (make-hash-table :test 'equalp))
	 carts)
     (iter (for line in-file file using #'read-line)
	   (for y from 0)
	   (iter (for chr in-string line)
		 (for x from 0)
		 (for pos = (vector x y))
		 (awhen (parse-track-char chr pos)
		   (setf (gethash pos tracks) it)
		   (awhen (track-occupant (gethash pos tracks))
		     (push it carts)))))
     (values tracks carts)))
(defmacro with-tracks-from (file &body body)
  "Evaluate body with *carts* and *tracks* bound to the results of parse-track-file"
  `(multiple-value-bind (*tracks* *carts*) (parse-track-file ,file)
     ,@body))


(define-condition crash (error)
  ((pos :initarg :pos :reader crash-pos))
  (:report (lambda (c stream)
	     (format stream "Crash at ~A" (crash-pos c)))))

(defun delete-cart (cart)
  "Removes a cart from both *tracks* and *carts* and invalidates it's position."
  (setf (track-occupant (gethash (cart-pos cart) *tracks*)) nil)
  ;; Note that we cannot delete here, since sim-step still holds a reference to the old *carts* list.
  ;; We therefore also invalidate the position, so sim-step can skip it.
  (setf *carts* (remove cart *carts* :test #'equalp))
  (setf (cart-pos cart) nil))
	
  
(defun drive (cart)
  "Drives the given cart one step. This function is called for side effects only."
  (with-slots (dir pos next-turn) cart
    (assert pos)
    (let* ((track (gethash pos *tracks*))
	   (new-dir (turn dir (track-turn cart track)))
	   (new-pos (map 'vector #'+ pos (direction-vector new-dir)))
	   (new-track (gethash new-pos *tracks*)))
      (assert  new-track)
      (when (track-occupant new-track)
	(restart-case (error 'crash :pos new-pos)
	  (remove-carts () (progn
			     (delete-cart (track-occupant new-track))
			     (delete-cart cart)
			     (return-from drive)))))
      (setf (track-occupant track) nil)
      (setf dir new-dir)
      (setf pos new-pos)
      (setf (track-occupant new-track) cart))))

(defun vector-rev-lex< (as bs)
  "Compares as and bs in reverse lexicographic order"
  (assert (= (length as) (length bs)))
  (iter (for a in-vector as downto 0)
	(for b in-vector bs downto 0)
	(cond ((< a b) (return t))
	      ((> a b) (return nil)))
	(finally (return nil))))

(assert (vector-rev-lex< #(1 0) #(0 1)))
(assert (vector-rev-lex< #(1 1) #(1 2)))
(assert (not (vector-rev-lex< #(1 1) #(1 1))))

(defun sim-step ()
  "Drives all cars once, in row order"
  (setf *carts* (sort *carts*
		      #'vector-rev-lex<
		      :key #'cart-pos))
  ;; Driving a cart might destroy a cart further up in the list. We therefore have to skip carts with invalid positions
  (mapc (lambda (cart) (when (cart-pos cart) (drive cart)))
	*carts*))

(defun solution1 ()
  (handler-case
      (iter (sim-step))
    (crash (c) (crash-pos c))))

(assert (equalp (with-tracks-from "13/test1"
		  (solution1))
		#(7 3)))

;; part one
(with-tracks-from "13/input"
  (solution1))

(defun solution2 ()
  (handler-bind ((crash
		  (lambda (c)
		    (declare (ignore c))
		    (invoke-restart 'remove-carts))))
    (iter (while (cdr *carts*))
	  (sim-step)
	  (finally (return (cart-pos (car *carts*)))))))


(assert (equalp (with-tracks-from "13/test2"
		  (solution2))
		#(6 4)))
(with-tracks-from "13/input"
  (solution2))
