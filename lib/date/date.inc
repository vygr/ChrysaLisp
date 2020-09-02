(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'lib/substr/substr.inc)
(import 'lib/date/timezones.inc)
(import 'apps/login/pupa.inc)

(structure 'time 0
	(byte 'second 'minute 'hour 'date 'month 'year 'week))

;;;USAGE;;;
;The functions date encode-date (date) and decode-date (encode-date (date))
;are the primary functions that you might want to use here.

(defq EPOCH_TIME '(0 0 0 1 0 1970 0) EPOCH_YEAR (elem (const time_year) EPOCH_TIME)
	EPOCH_DAY (elem (const time_date) EPOCH_TIME)
	days_per_year 365 days_per_leap_year 366 week_abbr (list "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
	month_abbr (list "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec") offset 0 tz *env_clock_timezone*)

;for decoding date-time strings
(defmacro-bind s2i (s)
	`(let ((final 0) (index 1) (isnum t))
		(each (lambda (c) (if (>= 48 (code c) 57)
			(setq final (+ final (* (- (code c) 48) index)) 
				index (* index 10)) (setq isnum nil))) (reverse ,s))
		(if (eql isnum t) final nil)))

;use a global instead, for realtime applications.
;offset number n by offset o with a max n. returns offset and carry values.
(defmacro-bind offset-number (n o m)
	`(let ((a (+ ,n ,o)) (c 0) (ls (list)))
		(if (> ,n ,m) nil
			(cond
				((= a 0) (push ls a))
				((< 0 a ,m) (push ls a))
				((= (% a ,m) 0) (setq c (/ a ,m))
					(push ls 0 c))
				((> a ,m) (setq c (/ a ,m) a (% a ,m))
					(push ls a c))
				((> (abs a) ,m) (setq c (/a ,m) a (+ ,m (% a ,m)))
					(push ls a c))
				((< a 0) (push ls (+ ,m (% a ,m))))
				(t nil)))))

;ensure that leapyears are not divisible by 100 unless also divisible by 400.
(defun-bind leapyear? (y)
	(cond 
		((and (= (% y 100) 0) (/= (% y 400) 0)) nil)
		((= (% y 4) 0) t)
		(t nil)))

(defun-bind dotw (wd)
	(cond
		((<= wd 3) (+ wd 3))
		((> wd 3) (- wd 4))
		(t nil)))

(defun-bind timezone-lookup ()
 	(each (lambda (_) (if (eql *env_clock_timezone* (elem 0 _)) (setq offset (* (elem 1 _) 60)))) timezones))

(defun-bind day-of-the-week (d)
	(elem d week_abbr))

(defun-bind month-of-the-year (m)
	(elem m month_abbr))

(defun-bind days-in-month (m y)
	(let ((months (list 31 28 31 30 31 30 31 31 30 31 30 31)))
			(if (and (= m 1) (leapyear? y)) 29 (elem m months))))

(defun-bind leapyears-since-epoch (year)
	(let ((leap_count 0))
		(each (lambda (y) 
			(if (eql (leapyear? y) t) (setq leap_count (inc leap_count)) nil))
			(range EPOCH_YEAR (inc year))) leap_count))

(defun-bind days-in-year (year)
	(if (leapyear? year) 366 365))

(defun-bind get-year (days)
	(defq year EPOCH_YEAR yeardays (days-in-year year))
	(while (> days (setq yeardays (days-in-year year)))
		(setq days (- days yeardays) year (inc year))) year)

;;;since day starts at 1 in unix, yearday is incremented by one
(defun-bind get-yeardays (days)
	;exclude current year
	(defq year (get-year days) years (- year EPOCH_YEAR)
		leapyears (leapyears-since-epoch (dec year))
		nonleapyears (- years leapyears))
		(inc (- days (+ (* leapyears days_per_leap_year)
			(* nonleapyears days_per_year)))))

(defun-bind get-date (days)
	(let ((month 0))
		(defq year (get-year days) date (get-yeardays days) monthdays (days-in-month month year))
		(while (> date (setq monthdays (days-in-month month year)))
			(setq date (- date monthdays) month (inc month)))
		(list date month year)))

(defun-bind check-date (td)
	(defq maxarg (list 59 59 23 (days-in-month (elem time_month td) (elem time_year td)) 11 nil 6)
		 minarg (list 0 0 0 1 0 nil 0) index -1)
	(notany (lambda (a)
		(setq index (inc index))
		(eql (and (not (= index 5)) (or (> a (elem index maxarg)) (< a (elem index minarg)))) t)) td))

(defun-bind date ()
	(timezone-lookup)
	(defq seconds (/ (time) 1000000) minutes (+ offset (/ seconds 60))
		hours (/ minutes 60) days (/ hours 24) weeks (/ days 7))
	(bind '(date month year) (get-date days))
	(defq second (% seconds 60) minute (% minutes 60) hour (% hours 24) weekday (% weeks 7))
	(list second minute hour date month year (dotw weekday)))	

(defun-bind encode-date (td)
	(bind '(s m h dy mo yr wk) td)
	(if (eql (check-date td) t)
		(cat (day-of-the-week wk) " " (month-of-the-year mo) " " 
			(str dy) " " (pad h 2 "0") ":" (pad m 2 "0") ":" (pad s 2 "0") " " *env_timezone* " " (str yr)) nil))

(defun-bind decode-date (dts)
	(defq space_split (split dts " ") rdt (list) index 0)
	(bind '(wd mo dy hms tz yr) (defq space_split (split dts " ")))
	(some (lambda (_) (if (eql _ wd) (setq wd index)) (setq index (inc index))) week_abbr)
	(setq index 0)
	(some (lambda (_) (if (eql _ mo) (setq mo index)) (setq index (inc index))) month_abbr)
	(defq smh (reverse (split hms ":")))
	(each (lambda (_) (push rdt (s2i _))) (reverse (split hms ":")))
	(push rdt (s2i dy) mo (s2i yr) wd))
