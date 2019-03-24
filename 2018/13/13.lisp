(in-package :aoc2018)


(defun vector-append-extend (ext vec)
  "Like vector-push-extend, but appends an entire sequence at once"
  (map nil (lambda (x) (vector-push-extend x vec)) ext))

(defstruct track
  dir
  occupant)

(defvar *directions* '(left up right down left))
(defvar *turn-directions* '(left straight right left))

(defun direction-vector (dir)
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
  (make-cart :dir (ecase chr
		    (#\^ 'up)
		    (#\v 'down)
		    (#\> 'right)
		    (#\< 'left))
	     :pos pos))
(defun parse-track-char (chr pos)
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
  
(defparameter *tracks* nil)
(defparameter *carts* nil)

(defun parse-track-file (file)
  (declare (optimize (debug 0)))
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
  `(multiple-value-bind (*tracks* *carts*) (parse-track-file ,file)
     ,@body))


(define-condition crash (error)
  ((pos :initarg :pos :reader crash-pos))
  (:report (lambda (c stream)
	     (format stream "Crash at ~A" (crash-pos c)))))

(defun drive (cart)
  (declare (optimize (debug 3)))
  (with-slots (dir pos next-turn) cart
    (let ((track (gethash pos *tracks*)))
      (setf (track-occupant track) nil)
      (setf dir (turn dir (track-turn cart track)))
      (setf pos (map 'vector #'+ pos (direction-vector dir)))
      (let ((track (gethash pos *tracks*)))
	(assert track)
	(when (track-occupant track)
	  (error 'crash :pos pos))
	  (setf (track-occupant track) cart))))
  cart)

(defun vector-rev-lex< (as bs)
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
  (setf *carts* (sort *carts* #'vector-rev-lex< :key #'cart-pos))
  (mapc #'drive *carts*))

(defun simulate ()
  (iter (for time from 0)
	(sim-step)))


(with-tracks-from "13/test1"
  (handler-case
      (simulate)
    (crash (c) (crash-pos c))))

; part one
(with-tracks-from "13/input"
  (handler-case
      (simulate)
    (crash (c) (crash-pos c))))
