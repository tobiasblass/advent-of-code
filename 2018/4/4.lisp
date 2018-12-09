(in-package :aoc2018)
(defparameter *input* (iter (for l in-file "4/input" using #'read-line)
		      (collect l)))

(define-constant +record-time-format+ '((:year 4) #\- (:month 2) #\- (:day 2)
					#\space (:hour 2) #\: (:min 2))
  :test #'equal
  :documentation "The format of timestamps in the input records")

(defun parse-timestamp (timestamp)
  "Parses a timestamp and returns a timestamp, where day is a string and minute a number"
  (let ((*default-timezone +utc-zone+))
    (let ((fields (nth-value 1 (cl-ppcre:scan-to-strings "(....)-(..)-(..) (..):(..)" timestamp))))
      (apply #'encode-timestamp 0 0 (nreverse (map 'list #'parse-integer fields))))))


(deftest parse-timestamp
    (let ((ts (parse-timestamp "1234-11-12 04:05")))
      (format-timestring nil ts :format +record-time-format+))
  "1234-11-12 04:05")

(defun parse-record (record)
  "Takes a record and returns a list of the form (timestamp event &optional id),
   where timestamp is one of falls-asleep, wakes-up and begin-shift"
  (cl-ppcre:register-groups-bind (timestamp event) ("^\\[(....-..-.. ..:..)\\] (.*)$" record)
    (apply #'list (parse-timestamp timestamp)
	   (cond
	     ((equal event "falls asleep") '(falls-asleep))
	     ((equal event "wakes up") '(wakes-up))
	     (t (cl-ppcre:register-groups-bind (id) ("Guard #([0-9]*) begins shift" event)
		  (list 'begin-shift (parse-integer id))))))))

(deftest parse-sleep-record
    (destructuring-bind (timestamp activity &optional id)
	(parse-record "[2222-11-11 11:11] falls asleep")
      (values (format-timestring nil timestamp :format +record-time-format+) activity id))
  ("2222-11-11 11:11" falls-asleep nil))
(deftest parse-wakeup-record
    (destructuring-bind (timestamp activity &optional id)
	(parse-record "[2222-11-11 11:11] wakes up")
      (values (format-timestring nil timestamp :format +record-time-format+) activity id))
  "2222-11-11 11:11" wakes-up nil)
(deftest parse-switch-record
    (destructuring-bind (timestamp activity &optional id)
	(parse-record "[2222-11-11 11:11] Guard #4711 begins shift")
      (values (format-timestring nil timestamp :format +record-time-format+) activity id))
  "2222-11-11 11:11" begin-shift 4711)

(defun sleep-timetable (records)
  "Returns a map mapping a minute to a list of (guard day) pairs.
   This means that at the given minute, the guard was asleep on that day."
  (let ((sleep-timetable (make-hash-table :test #'equal))
	(active-guard)
	(last-sleep-time))
    (dolist (record records)
      (destructuring-bind (time activity &optional id) record
	(ecase activity
	  (falls-asleep (setf last-sleep-time time))
	  (wakes-up
	   (assert (= (timestamp-hour last-sleep-time) (timestamp-hour time) 0))
	   (iter (for minute from (timestamp-minute last-sleep-time) below (timestamp-minute time))
		 (push (list active-guard (adjust-timestamp time (set :minute minute)))
		       (gethash minute sleep-timetable))))
  	  (begin-shift (setf active-guard id)))))
    sleep-timetable))

(defparameter *sleep-timetable*
  (sleep-timetable (sort (mapcar #'parse-record *input*)
			 #'timestamp<
			 :key #'car)))

(defun guard-list (timetable)
  (iter (for guard in (remove-duplicates
		       (mapcar #'caar
			       (hash-table-values timetable))))
	(collect guard)))

(defun minutes-spent-asleep (guard)
  (iter (for (minute per-minute-records) in-hashtable *sleep-timetable*)
	(sum (funcall #'count guard per-minute-records :key #'car))))

(defun days-slept-in-minute (guard minute)
  (count guard (gethash minute *sleep-timetable*) :key #'car))
(defun minute-slept-most-often (guard)
  (iter (for minute from 0 below 60)
	(finding minute maximizing (days-slept-in-minute guard minute))))

(defun solution1 ()
  (let ((guard (iter (for guard in (guard-list *sleep-timetable*))
		     (finding guard maximizing #'minutes-spent-asleep))))
    (* guard (minute-slept-most-often guard))))

(defun solution2 ()
  ; Buggy: Result is too high
  (iter (for guard in (guard-list *sleep-timetable*))
	(for minute = (minute-slept-most-often guard))
	(finding (* minute guard) maximizing (days-slept-in-minute guard minute))))
	
(deftest example1
    (let ((input '( "[1518-11-01 00:00] Guard #10 begins shift" 
		   "[1518-11-01 00:05] falls asleep"
		   "[1518-11-01 00:25] wakes up"
		   "[1518-11-01 00:30] falls asleep"
		   "[1518-11-01 00:55] wakes up"
		   "[1518-11-01 23:58] Guard #99 begins shift"
		   "[1518-11-02 00:40] falls asleep"
		   "[1518-11-02 00:50] wakes up"
		   "[1518-11-03 00:05] Guard #10 begins shift"
		   "[1518-11-03 00:24] falls asleep"
		   "[1518-11-03 00:29] wakes up"
		   "[1518-11-04 00:02] Guard #99 begins shift"
		   "[1518-11-04 00:36] falls asleep"
		   "[1518-11-04 00:46] wakes up"
		   "[1518-11-05 00:03] Guard #99 begins shift"
		   "[1518-11-05 00:45] falls asleep"
		   "[1518-11-05 00:55] wakes up")))
      (let ((*sleep-timetable* (sleep-timetable 
				(mapcar #'parse-record input))))
	(values (sort (guard-list *sleep-timetable*) #'<)
	        (minutes-spent-asleep 10)
		(minutes-spent-asleep 99)
		(minute-slept-most-often 10)
		(solution1)
		(solution2))))
  (10 99) 50 30 24 240 4455)
(do-tests)

(solution1)
(solution2)
