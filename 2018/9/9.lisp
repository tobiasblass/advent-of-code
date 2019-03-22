(in-package :aoc2018)

;; A cursor into the marble ring. Everything to the right is in "right", everything to the left is in "left" (in inverse order
;; Invariants: (not right) => (not left)
(defstruct (cursor (:constructor make-cursor (left right)))
  left
  right)

(defun normalize (cursor)
  "Ensures the invariant. Must be called after every operation that might break it."
  (if (not (cursor-right cursor))
      (make-cursor nil (reverse (cursor-left cursor)))
      cursor))

(defun cursor-print (cursor)
  (print (append (reverse (cursor-left cursor)) '("|") (cursor-right cursor))))
(defun current-marble (cursor)
  (car (cursor-right cursor)))

(defun mirror (cursor)
  (normalize
   (make-cursor (cursor-right cursor)
		(cursor-left cursor))))

(defun move-clockwise (cursor)
  (with-slots (left right) cursor
    (if (not right)
	cursor
	(normalize
	 (make-cursor (cons (car right) left) (cdr right))))))

(defun move (cursor n)
  (cond
    ((zerop n) cursor)
    ((plusp n) (move (move-clockwise cursor) (- n 1)))
    (t (mirror (move (mirror cursor) (- n))))))

(defun place (cursor val)
  (make-cursor (cursor-left cursor)
	       (cons val (cursor-right cursor))))
(defun take (cursor)
  "Takes the current marble. Returns two values; the new cursor and the value of the marble" 
  (with-slots (left right) cursor
    (values
     (normalize (make-cursor left
			     (cdr right)))
     (current-marble cursor))))

(defun score (cursor)
  "Takes the current marble and the one 7 to the left.
   Returns two values: The new cursor and the points just scored"
    (multiple-value-bind (cursor score) (take (move cursor -7))
      (values cursor score)))

(defun game (players max-marble)
  "Plays the game with the given number of players and maximum marble value.
   Returns two values: the score of the winning player, and an alist of the scores"
  (let ((score (make-hash-table)))
    (iter (for p from 0 to players) (setf (gethash p score) 0))
    (iter (for i from 0 to max-marble)
	  (for cursor initially (make-cursor nil nil)
	       then (if (and (plusp i) (zerop (rem i 23)))
			(multiple-value-bind (new-cursor points) (score cursor)
			  (incf (gethash (rem i players) score) (+ i points))
			  new-cursor)
			(place (move cursor 2) i))))
    (let ((score-alist (hash-table-alist score)))
      (values
       (iter (for pair in score-alist) (maximize (cdr pair)))
       score-alist))))
	  
	
(deftest examples
    (values (game 9 25)
	    (game 10 1618)
	    (game 13 7999)
	    (game 17 1104)
	    (game 21 6111)
	    (game 30 5807))
  32 8317 146373 2764 54718 37305)

; Solution 1
(nth-value 0 (game 430 71588))

; Solution 1
(nth-value 0 (game 430 (* 100 71588)))
