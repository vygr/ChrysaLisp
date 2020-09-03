(import 'lib/date/timezones.inc)
(import 'apps/login/pupa.inc)

;Limitations: Only defined for time since Unix Epoch.

(structure 'time 0
	(byte 'second 'minute 'hour 'date 'month 'year 'week))

(defq EPOCH_TIME '(0 0 0 1 0 1970 0) EPOCH_YEAR (elem (const time_year) EPOCH_TIME)
	EPOCH_DAY (elem (const time_date) EPOCH_TIME) days_per_year 365 days_per_leap_year 366 offset 0
	week_abbr (list "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
	month_abbr (list "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

;for decoding date string integers
(defun-bind s2i (s)
	(let ((final 0) (index 1) (iszero nil) (isnum t) (isneg nil))
		(cond
			((and (= (length s) 1) (eql (code s) 48)) (setq iszero t))
			((starts-with "-" s) (setq isneg t s (slice 1 -1 s)))
			(t nil))
		(if (some (lambda (c) (not (< 47 (code c) 58))) s) (setq final nil))
		(if (and final (not iszero)) (each (lambda (c)
			(setq final (+ final (* (- (code c) 48) index)) index (* index 10))) (reverse s)))
		(if isneg (setq final (- 0 final)))
		final))

;ensure that leapyears are not divisible by 100 unless also divisible by 400.
(defmacro-bind leapyear? (y)
	`(cond 
		((and (= (% ,y 100) 0) (/= (% ,y 400) 0)) nil)
		((= (% ,y 4) 0) t)
		(t nil)))

(defmacro-bind timezone-lookup ()
 	`(each (lambda (_) (if (eql *env_clock_timezone* (elem 0 _)) (setq offset (* (elem 1 _) 60)))) timezones))

(defmacro-bind day-of-the-week (d)
	`(elem ,d week_abbr))

(defmacro-bind month-of-the-year (m)
	`(elem ,m month_abbr))

(defmacro-bind days-in-month (m y)
	`(let ((months (list 31 28 31 30 31 30 31 31 30 31 30 31)))
			(if (and (= ,m 1) (leapyear? ,y)) 29 (elem ,m months))))

(defmacro-bind days-in-year (year)
	`(if (leapyear? ,year) 366 365))

;the epoch was on a Thursday, adjust for calendar
(defmacro-bind get-weekday (wd)
		`(if (<= 0 ,wd 6) (if (< ,wd 4) (+ ,wd 4) (- ,wd 3)) nil))

(defun-bind leapyears-since-epoch (year)
	(let ((leap_count 0))
		(each (lambda (y) 
			(if (eql (leapyear? y) t) (setq leap_count (inc leap_count)) nil))
			(range EPOCH_YEAR (inc year))) leap_count))

;;;since day starts at 1 add 1.
(defun-bind get-yeardays (days)
	(defq year (get-year days) years (- year EPOCH_YEAR)
		leapyears (leapyears-since-epoch (dec year))
		nonleapyears (- years leapyears))
		(inc (- days (+ (* leapyears days_per_leap_year)
			(* nonleapyears days_per_year)))))

(defun-bind get-year (days)
	(defq year EPOCH_YEAR yeardays (days-in-year year))
	(while (> days (setq yeardays (days-in-year year)))
		(setq days (- days yeardays) year (inc year))) year)

(defun-bind get-date (days)
	(let ((month 0))
		(defq year (get-year days) date (get-yeardays days) monthdays (days-in-month month year))
		(while (> date (setq monthdays (days-in-month month year)))
			(setq date (- date monthdays) month (inc month)))
		(list date month year)))

(defun-bind check-date (td)
	(defq maxarg (list 59 59 23 (days-in-month (elem time_month td) (elem time_year td)) 11 nil 6)
		 minarg (list 0 0 0 1 0 nil 0) index -1)
	(notany (lambda (a_)
		(eql (and (not (= _ 5)) (or (> a_ (elem _ maxarg)) (< a_ (elem _ minarg)))) t)) td))

;takes a time value in seconds or uses default (time)
(defun-bind date (&optional ts)
	(timezone-lookup)
	(if (not ts) (defq ts (/ (time) 1000000)))
	(defq seconds ts minutes (+ offset (/ seconds 60))
		hours (/ minutes 60) days (/ hours 24) weeks (/ days 7))
	(bind '(monthday month year) (get-date days))
	(list (% seconds 60) (% minutes 60) (% hours 24) monthday month year (get-weekday (% days 7))))

(defun-bind encode-date (td)
	(bind '(s m h dy mo yr wk) td)
	(if (eql (check-date td) t)
		(cat (day-of-the-week wk) " " (month-of-the-year mo) " " 
			(str dy) " " (pad h 2 "0") ":" (pad m 2 "0") ":" (pad s 2 "0") " " 
			*env_clock_timezone* " " (str yr)) nil))

(defun-bind decode-date (dts)
	(defq space_split (split dts " ") rdt (list) index 0)
	(bind '(wd mo dy hms tz yr) (defq space_split (split dts " ")))
	(each (lambda (w_) (if (eql w_ wd) (setq wd _) nil) (setq index (inc index))) week_abbr)
	(setq index 0)
	(each (lambda (m_) (if (eql m_ mo) (setq mo _)) (setq index (inc index))) month_abbr)
	(defq smh (reverse (split hms ":")))
	(each (lambda (_) (push rdt (s2i _))) (reverse (split hms ":")))
	(push rdt (s2i dy) mo (s2i yr) wd))
