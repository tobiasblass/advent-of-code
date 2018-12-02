(ql:quickload :iterate)
(use-package :iterate)

(defun has-letter-n-times (sorted-word n)
  (let ((letters (remove-duplicates sorted-word)))
    (find-if (lambda (c) (= (count c sorted-word) n))
	     letters)))

(defun checksum (ids)
  (*
   (count-if (lambda (id) (has-letter-n-times id 2)) ids)
   (count-if (lambda (id) (has-letter-n-times id 3)) ids)))


(let ((test-ids '("abcdef"
		  "bababc"
		  "abbcde"
		  "abcccd"
		  "aabcdd"
		  "abcdee"
		  "ababab")))
      (assert (= (checksum test-ids) 12)))

(defvar ids (iter (for l in-file "input" using #'read-line)
		  (collect l)))
(checksum ids)


(defun count-mismatches (seq1 seq2)
  "Returns the number mismatch positions"
  (count nil (map 'list #'eql seq1 seq2)))

(assert (= (count-mismatches "abcde" "axcye") 2))
(assert (= (count-mismatches "fghij" "fguij") 1))

(defun find-near-id (id ids)
  "Finds an id in IDS that only differs from id by a single character"
  (find-if (lambda (id2) (= (count-mismatches id id2) 1)) ids))

(defun find-close-ids (ids)
  "Finds two IDs that only differ by a single character"
  (if (> (length ids) 0)
      (let ((first (elt ids 0))
	    (rest (subseq ids 1)))
	(let ((partner (find-near-id first rest)))
	  (if partner
	      (list first partner)
	      (find-close-ids rest))))))
  
      
(let ((ids '("abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz")))
  (assert (equal (find-close-ids ids) '("fghij" "fguij"))))

(destructuring-bind (id1 id2) (find-close-ids ids)
  (remove (elt id1 (mismatch id1 id2)) id1 :count 1))
