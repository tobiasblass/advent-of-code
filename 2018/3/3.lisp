(in-package :aoc2018)

(defstruct claim
  id
  left-border
  top-border
  right-border
  bottom-border)

(defun read-claim (&optional (stream *standard-input*) (eof-error-p t) (eof-value nil) (recursive-p nil))
  "Reads a claim specification from the stream and returns a claim struct"
  (let ((line (read-line stream eof-error-p eof-value recursive-p)))
    (if (equal line eof-value)
	eof-value
	(destructuring-bind (id left-border top-border width height)
	    (map 'list #'parse-integer
		 (nth-value 1
			    (cl-ppcre:scan-to-strings "^#([0-9]*) @ ([0-9]*),([0-9]*): ([0-9]*)x([0-9]*)$" line)))
	  (make-claim :id id
		      :left-border left-border
		      :top-border top-border
		      :right-border (+ left-border width -1)
		      :bottom-border (+ top-border height -1))))))

(defvar claims (iter (for claim in-file "3/input" using #'read-claim)
		     (collect claim)))

(defun claim->points (claim)
  (iter (for x from (claim-left-border claim) to (claim-right-border claim))
	(appending (iter (for y from (claim-top-border claim) to (claim-bottom-border claim))
			 (collect (list x y))))))

(defun mark-claim (claim map)
  "Marks the given claim in the map, which maps from points to claimants. Returns the map." 
  (mapc (lambda (point) (push (claim-id claim) (gethash point map))) (claim->points claim))
  map)

(defun claims->map (claims)
  "Returns a map that maps each point in the fabric to it's claimants"
  (let ((m (make-hash-table :test #'equal)))
    (mapc (rcurry #'mark-claim m) claims)
    m))

			  
(defun count-disputed-squares (fabric-map)
  (count-if (lambda (claimants) (> (length claimants) 1)) (hash-table-values fabric-map)))

(deftest overlap
    (count-disputed-squares
     (claims->map
      (let ((in (make-string-input-stream (format nil "#1 @ 1,3: 4x4~%#2 @ 3,1: 4x4~%#3 @ 5,5: 2x2"))))
	(iter (for claim in-stream in using #'read-claim)
	      (collect claim)))))
  4)


(do-tests)
(count-disputed-squares (claims->map claims))

(let ((ids (mapcar #'claim-id claims))
      (contentions (remove-if (lambda (list) (<= (length list) 1))
			     (hash-table-values (claims->map claims)))))
  (let ((contenders (reduce #'append (remove-duplicates contentions :test #'equal))))
    (iter (for id in ids)
	  (finding id such-that (not (member id contenders))))))
	
