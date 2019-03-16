(ql:quickload '(:iterate :alexandria :anaphora :cl-ppcre :sb-rt :local-time :sketch :lla))
(defpackage :de.t-blass.aoc2018
  (:nicknames :aoc2018)
  (:use :cl :iterate :alexandria :anaphora :cl-ppcre :local-time :sb-rt :lla))

(load "common.lisp")
