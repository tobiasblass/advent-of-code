(in-package :aoc2018)
(defun read-lines (file)
  (iter (for l in-file file)
	(collect l)))
