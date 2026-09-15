(import "./timezones.inc")

;module
(env-push)

(enums +time 0
	(enum second minute hour date month year week dls tz))

(enums +tz 0
	(enum abbreviation offset dls title locations))

(defq week_abbr '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat")
	month_abbr '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defun leapyear? (y)
	; (leapyear? year) -> :t | :nil
	(and (= (% y 4) 0)
		(or (/= (% y 100) 0) (= (% y 400) 0))))

(defun days-in-year (y)
	; (days-in-year year) -> 365 | 366
	(if (leapyear? y) 366 365))

(defun days-in-month (m y)
	; (days-in-month month year) -> days (28..31)
	(defq mo (% m 12))
	(if (and (= mo 1) (leapyear? y))
		29
		(elem-get '(31 28 31 30 31 30 31 31 30 31 30 31) mo)))

(defun day-of-the-week (d)
	; (day-of-the-week dotw) -> str
	(elem-get week_abbr (% (+ (% d 7) 7) 7)))

(defun month-of-the-year (m)
	; (month-of-the-year month) -> str
	(elem-get month_abbr (% (+ (% m 12) 12) 12)))

(defun timezone-offset-seconds (tz)
	; (timezone-offset-seconds tz) -> seconds
	(defq off (elem-get tz +tz_offset))
	(if (fixed? off)
		(n2i (* off 3600.0))
		(* off 3600)))

(defun timezone-lookup (arg1 &optional arg2)
	; (timezone-lookup query [field]) -> tz | :nil
	(defq field (if (find arg1 '(:location :abbreviation :offset :title)) arg1
			(if (find arg2 '(:location :abbreviation :offset :title)) arg2 :nil))
		q (if (eql field arg1) arg2 arg1))
	(cond
		((eql field :location)
			(some (# (if (find q (elem-get %0 +tz_locations)) %0)) timezones))
		((eql field :abbreviation)
			(some (# (if (eql q (elem-get %0 +tz_abbreviation)) %0)) timezones))
		((eql field :offset)
			(some (# (if (eql q (elem-get %0 +tz_offset)) %0)) timezones))
		((eql field :title)
			(some (# (if (eql q (elem-get %0 +tz_title)) %0)) timezones))
		(:t
			(or (some (# (if (find q (elem-get %0 +tz_locations)) %0)) timezones)
				(some (# (if (eql q (elem-get %0 +tz_abbreviation)) %0)) timezones)
				(some (# (if (eql q (elem-get %0 +tz_title)) %0)) timezones)))))

(defun timezone-init (tz_loc)
	; (timezone-init tz_loc) -> tz
	(defq tz (or (timezone-lookup tz_loc :location)
			(timezone-lookup tz_loc)
			(timezone-lookup "UTC" :abbreviation)))
	(setq *local_timezone* (cat tz))
	(elem-set *local_timezone* +tz_locations tz_loc)
	(def *root_env* :local_timezone *local_timezone*)
	(def *root_env* '*local_timezone* *local_timezone*)
	*local_timezone*)

(defun civil-from-days (days)
	; (civil-from-days days) -> (year month day)
	; O(1) arithmetic calendar conversion (month 0..11, day 1..31)
	(defq z (+ days 719468)
		era (if (>= z 0) (/ z 146097) (/ (- z 146096) 146097))
		doe (- z (* era 146097))
		yoe (/ (- (+ (- doe (/ doe 1460)) (/ doe 36524)) (/ doe 146096)) 365)
		y (+ yoe (* era 400))
		doy (- doe (- (+ (* 365 yoe) (/ yoe 4)) (/ yoe 100)))
		mp (/ (+ (* 5 doy) 2) 153)
		d (inc (- doy (/ (+ (* 153 mp) 2) 5)))
		m (if (< mp 10) (+ mp 3) (- mp 9))
		yr (if (<= m 2) (inc y) y))
	(list yr (dec m) d))

(defun days-from-civil (yr mo d)
	; (days-from-civil year month day) -> days
	(defq m (inc mo)
		y (if (<= m 2) (dec yr) yr)
		era (if (>= y 0) (/ y 400) (/ (- y 399) 400))
		yoe (- y (* era 400))
		doy (+ (/ (+ (* 153 (if (> m 2) (- m 3) (+ m 9))) 2) 5) (dec d))
		doe (+ (- (+ (* yoe 365) (/ yoe 4)) (/ yoe 100)) doy))
	(- (+ (* era 146097) doe) 719468))

(defun date (&optional secs)
	; (date [secs]) -> (sec min hour day month year dotw)
	(defq s (ifn secs (/ (pii-time) 1000000))
		local_s (+ s (timezone-offset-seconds *local_timezone*))
		days (if (>= local_s 0)
			(/ local_s 86400)
			(/ (- local_s 86399) 86400))
		rem_s (- local_s (* days 86400))
		sec (% rem_s 60)
		min (% (/ rem_s 60) 60)
		hour (/ rem_s 3600)
		dotw (% (+ (% (+ days 4) 7) 7) 7))
	(bind '(year month day) (civil-from-days days))
	(list sec min hour day month year dotw))

(defun float-time (&optional smooth)
	; (float-time [smooth]) -> (sec min hour)
	(defq now (pii-time)
		offset_sec (timezone-offset-seconds *local_timezone*)
		sec_day (% (+ (/ now 1000000) offset_sec) 86400)
		sec_day (if (< sec_day 0) (+ sec_day 86400) sec_day)
		sec_int (n2f (% sec_day 60))
		s (if smooth
			(+ sec_int (/ (n2f (% now 1000000)) 1000000.0))
			sec_int)
		m (+ (n2f (% (/ sec_day 60) 60)) (/ s 60.0))
		h (+ (n2f (% (/ sec_day 3600) 12)) (/ m 60.0)))
	(list s m h))

(defun check-date (td)
	; (check-date td) -> :t | :nil
	(bind '(s m h dy mo yr &rest tail) td)
	(and (<= 0 s 59)
		(<= 0 m 59)
		(<= 0 h 23)
		(<= 0 mo 11)
		(<= 1 dy (days-in-month mo yr))))

(defun encode-date (&optional td)
	; (encode-date [td]) -> str
	(setd td (date))
	(when (check-date td)
		(bind '(s m h dy mo yr wk) td)
		(cat (day-of-the-week wk) " " (month-of-the-year mo) " "
			(pad dy 2 " ") " " (pad h 2 "0") ":" (pad m 2 "0") ":" (pad s 2 "0") " "
			(elem-get *local_timezone* +tz_abbreviation) " " (str yr))))

(defun decode-date (dts)
	; (decode-date str) -> td
	(bind '(wd mo dy hms tz yr) (split (trim dts) " "))
	(defq w_idx (or (find wd week_abbr) 0)
		m_idx (or (find mo month_abbr) 0)
		d_num (str-to-num dy)
		y_num (str-to-num yr))
	(bind '(h m s) (map (const str-to-num) (split hms ":")))
	(list s m h d_num m_idx y_num w_idx))

(defun time-from-date (td)
	; (time-from-date td) -> secs
	(bind '(s m h dy mo yr &rest tail) td)
	(defq days (days-from-civil yr mo dy)
		local_s (+ (* days 86400) (* h 3600) (* m 60) s))
	(- local_s (timezone-offset-seconds *local_timezone*)))

(defq *local_timezone*
	(list "UTC" 0 :nil "(UTC) Coordinated Universal Time" (list "Etc/GMT")))
(def *root_env* :local_timezone *local_timezone*)
(def *root_env* '*local_timezone* *local_timezone*)

;module
(export-symbols '(+time_size +tz_size leapyear? days-in-year days-in-month
	week_abbr month_abbr day-of-the-week month-of-the-year
	timezone-lookup timezone-offset-seconds timezone-init
	civil-from-days days-from-civil date float-time
	check-date encode-date decode-date time-from-date
	*local_timezone*))
(env-pop)