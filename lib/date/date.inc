(import "lib/date/timezones.inc")

;to set your local timezone use the tz-init function. tz-init uses locations
;not abbreviations.

;You can use timezone-lookup to find a location near you. 
;Locations with spaces in the name may contain an underscore (eg New_York).

;Currently defined only for time since unix epoch Jan 1, 1970

(structure +time 0
	(byte second minute hour date month year week dls tz))

(structure +tz 0
	(byte abbreviation offset dls title locations))

(defmacro leapyear? (y)
	`(cond 
		((and (= (% ,y 100) 0) (/= (% ,y 400) 0)) nil)
		((= (% ,y 4) 0) t)
		(t nil)))

(defq unix_epoch '(0 0 0 1 1 1970 0) dls_flag nil 
	;week_abbr (list "Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
	week_abbr (list "Thu" "Fri" "Sat" "Sun" "Mon" "Tue" "Wed")
	month_abbr (list "Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

;for decoding date string integers
(defun s2i (s)
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

;returns the first list as out by finding v at elem n of a list within in.
(defun get-by-val (n v in)
	(let ((out nil))
		(cond 
			((list? (defq element (elem n (elem 0 in))))
				(each (lambda (e) (if (elem-find v (elem +tz_locations e)) (setq out e))) in))
			((num? element) (each (lambda (e) (if (=  (elem n e) v) (setq out e))) in))
			(t (each (lambda (e) (if (eql (elem n e) v) (setq out e))) in)))
		out))

;returns first occurrence index or nil if not found
(defmacro elem-find (v in)
	`(let ((out nil))
		(each (lambda (e_) (if (eql e_ ,v) (unless out (setq out _)))) ,in)
			out))

;timezone-lookup will attempt return tz list
(defmacro timezone-lookup (p v)
	`(if (not ,p)
			(get-by-val +tz_abbreviation ,v timezones)
			(cond
				((eql ,p :abbreviation) (get-by-val +tz_abbreviation ,v timezones))
				((eql ,p :offset) (get-by-val +tz_offset ,v timezones))
				((eql ,p :title) (get-by-val +tz_title ,v timezones))
				((eql ,p :location) (get-by-val +tz_locations ,v timezones))
				(t nil))))

;timezone-init creates the timezone. Use this once you have a location.
(defmacro timezone-init (tz_loc)
	`(let ((ltz (list)))
		(setq ltz (timezone-lookup :location ,tz_loc))
		(elem-set +tz_locations ltz ,tz_loc)
		(def (penv (env)) :local_timezone ltz)))

;ensure that leapyears are not divisible by 100 unless also divisible by 400.
(defmacro leapyear? (y)
	`(cond 
		((and (= (% ,y 100) 0) (/= (% ,y 400) 0)) nil)
		((= (% ,y 4) 0) t)
		(t nil)))

(defmacro day-of-the-week (d)
	`(elem ,d week_abbr))

(defmacro month-of-the-year (m)
	`(elem ,m month_abbr))

(defmacro days-in-month (m y)
	`(let ((months (list 31 28 31 30 31 30 31 31 30 31 30 31)))
			(if (and (= (% ,m 12) 1) (leapyear? ,y)) 29 (elem (% ,m 12) months))))

(defmacro days-in-year (year)
	`(if (leapyear? ,year) 366 365))

(defun leapyears-since-epoch (year)
	(let ((leap_count 0))
		(each (lambda (y) 
			(if (eql (leapyear? y) t) (setq leap_count (inc leap_count)) nil))
			(range (elem +time_year unix_epoch) (inc year))) leap_count))

;;;since day starts at 1 add 1.
(defun get-yeardays (days)
	(defq year (get-year days) years (- year (elem +time_year unix_epoch))
		leapyears (leapyears-since-epoch (dec year))
		nonleapyears (- years leapyears))
		(inc (- days (+ (* leapyears 366)
			(* nonleapyears 365)))))

(defun get-year (days)
	(defq year (elem +time_year unix_epoch) yeardays (days-in-year year))
	(while (> days (setq yeardays (days-in-year year)))
		(setq days (- days yeardays) year (inc year))) year)

(defun get-date (days)
	(let ((month 0))
		(defq year (get-year days) date (get-yeardays days) monthdays (days-in-month month year))
		(while (> date (setq monthdays (days-in-month month year)))

			(setq date (- date monthdays) month (inc month))
			(if (> month 11) (setq month 0)))
		(list date month year)))

(defun check-date (td)
	(defq maxarg (list 59 59 23 (days-in-month (elem +time_month td) (elem +time_year td)) 11 nil 6)
		 minarg (list 0 0 0 1 0 nil 0) index -1)
	(notany (lambda (a_)
		(eql (and (not (= _ 5)) (or (> a_ (elem _ maxarg)) (< a_ (elem _ minarg)))) t)) td))

(defun float-time ()
	(defq seconds (n2f (/ (pii-time) 1000000)) minutes (+ (* 60.0 (n2f 
		(elem +tz_offset (get :local_timezone)))) (/ seconds 60.0)) 
		hours (% (/ minutes 60.0) 12.0))
	(list seconds minutes hours))

;takes a time value in seconds or uses default (pii-time)
(defun date (&optional secs)
	(defq seconds (/ (pii-time) 1000000))
	(if secs (setq seconds secs))
	(defq minutes (+ (* 60 (elem +tz_offset (get :local_timezone))) (/ seconds 60))
		hours (/ minutes 60) days (/ hours 24) weeks (/ days 7))
	(bind '(monthday month year) (get-date days))
	(list (% seconds 60) (% minutes 60) (% hours 24) monthday month year (% days 7)))

(defun encode-date (&optional td)
	(when (not td) (setq td (date))) 
	(bind '(s m h dy mo yr wk) td)
	(when (check-date td)
		(cat (day-of-the-week wk) " " (month-of-the-year mo) " " 
			(str dy) " " (pad h 2 "0") ":" (pad m 2 "0") ":" (pad s 2 "0") " " 
			(elem +tz_abbreviation (get :local_timezone)) " " (str yr))))

(defun decode-date (dts)
	(defq space_split (split dts " ") rdt (list) index 0)
	(bind '(wd mo dy hms tz yr) (defq space_split (split dts " ")))
	(each (lambda (w_) (if (eql w_ wd) (setq wd _) nil) (setq index (inc index))) week_abbr)
	(setq index 0)
	(each (lambda (m_) (if (eql m_ mo) (setq mo _)) (setq index (inc index))) month_abbr)
	(defq smh (reverse (split hms ":")))
	(each (lambda (_) (push rdt (s2i _))) (reverse (split hms ":"))) 
	(push rdt (s2i dy) mo (s2i yr) wd))

(when (not (get :local_timezone))
	(def (penv (env)) :local_timezone 
		(list "UTC" 0 nil "(UTC) Coordinated Universal Time" "Etc/GMT")))
