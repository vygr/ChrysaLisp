(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/vector.inc")
(import "lib/math/trig.inc")

; Fixed-point trigonometric helper constants
(defq +geo_deg_to_rad+ (/ +fp_pi 180.0)
	+geo_rad_to_deg+ (/ 180.0 +fp_pi))

(defun geo-deg-to-rad (deg)
	(* deg +geo_deg_to_rad+))

(defun geo-rad-to-deg (rad)
	(* rad +geo_rad_to_deg+))

(defun norm-angle-pi (a)
	; Normalize angle to [-pi, +pi)
	(defq two_pi +fp_2pi
		wrapped (% (+ (% a two_pi) two_pi) two_pi))
	(if (>= wrapped +fp_pi) (- wrapped two_pi) wrapped))

(defun norm-angle-2pi (a)
	; Normalize angle to [0, 2pi)
	(defq two_pi +fp_2pi)
	(% (+ (% a two_pi) two_pi) two_pi))

(defun norm-angle-360 (deg)
	; Normalize angle in degrees to [0, 360)
	(% (+ (% deg 360.0) 360.0) 360.0))

(defun norm-angle-180 (deg)
	; Normalize angle in degrees to [-180, +180)
	(defq wrapped (norm-angle-360 deg))
	(if (>= wrapped 180.0) (- wrapped 360.0) wrapped))

;the inverse turns are of doubles, lib/math/trig.inc: a fixed made a
;double for one, and its answer the fixed again

(defun tan (x)
	; Tangent of angle in radians
	(defq c (cos x))
	;the least that is not 0 to a fixed is 0.00002: a number written
	;smaller than that is 0, and nothing is less than it
	(if (< (abs c) 0.0001)
		(* (sign (sin x)) 1000000.0)
		(/ (sin x) c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Solar Ephemeris (Astronomical Almanac / Meeus)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solar-ephemeris (unix_sec)
	; Given UTC timestamp in seconds, returns:
	; (sun_lon_deg sun_lat_deg eot_min season_str)
	(defq d (/ (- (n2q unix_sec) 946728000.0) 86400.0) ; Days since J2000.0 (2000-01-01 12:00 UTC)
		m_deg (norm-angle-360 (+ 357.529 (* 0.98560028 d)))
		l_deg (norm-angle-360 (+ 280.459 (* 0.98564736 d)))
		m_rad (geo-deg-to-rad m_deg)
		l_rad (geo-deg-to-rad l_deg)
		; Ecliptic longitude of Sun
		lambda_deg (+ l_deg (* 1.915 (sin m_rad)) (* 0.020 (sin (* 2.0 m_rad))))
		lambda_rad (geo-deg-to-rad lambda_deg)
		; Obliquity of ecliptic
		eps_deg (- 23.439 (/ (* 0.36 d) 1000000.0))
		eps_rad (geo-deg-to-rad eps_deg)
		; Solar declination delta
		sin_delta (* (sin eps_rad) (sin lambda_rad))
		delta_rad (n2q (asin (n2d sin_delta)))
		delta_deg (geo-rad-to-deg delta_rad)
		; Equation of time in minutes
		y_val (* (tan (/ eps_rad 2.0)) (tan (/ eps_rad 2.0)))
		e_ecc 0.01671
		term1 (* y_val (sin (* 2.0 l_rad)))
		term2 (* (* 2.0 e_ecc) (sin m_rad))
		term3 (* (* 4.0 (* e_ecc y_val)) (* (sin m_rad) (cos (* 2.0 l_rad))))
		term4 (* 0.5 (* y_val y_val) (sin (* 4.0 l_rad)))
		term5 (* 1.25 (* e_ecc e_ecc) (sin (* 2.0 m_rad)))
		eot_rad (- (- (+ term1 term3) term2) (+ term4 term5))
		eot_min (* 4.0 (geo-rad-to-deg eot_rad))
		; UTC hour of the day
		utc_sec (% unix_sec 86400)
		utc_sec (if (< utc_sec 0) (+ utc_sec 86400) utc_sec)
		utc_hours (/ (n2q utc_sec) 3600.0)
		; Solar time at Greenwich
		solar_hours (+ utc_hours (/ eot_min 60.0))
		; Subsolar longitude in degrees [-180, +180)
		subsolar_lon_deg (norm-angle-180 (* (- 12.0 solar_hours) 15.0))
		; Approximate season description
		season_str (cond
			((and (>= delta_deg 22.0) (<= delta_deg 23.5)) "Summer Solstice (North)")
			((and (<= delta_deg -22.0) (>= delta_deg -23.5)) "Winter Solstice (North)")
			((and (>= delta_deg -1.5) (<= delta_deg 1.5))
				(if (< (sin lambda_rad) 0.0) "Autumnal Equinox" "Vernal Equinox"))
			((> delta_deg 0.0) "Northern Spring/Summer")
			(:t "Northern Autumn/Winter")))
	(list subsolar_lon_deg delta_deg eot_min season_str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Solar Elevation & Daylight Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solar-elevation (lat_deg lon_deg sun_lat_deg sun_lon_deg)
	; Calculates solar elevation above horizon in degrees for location (lat, lon)
	(defq phi (geo-deg-to-rad lat_deg)
		delta (geo-deg-to-rad sun_lat_deg)
		d_lon (geo-deg-to-rad (- lon_deg sun_lon_deg))
		cos_zenith (+ (* (sin phi) (sin delta))
			(* (* (cos phi) (cos delta)) (cos d_lon)))
		clamped (clamp cos_zenith -1.0 1.0)
		elev_rad (n2q (asin (n2d clamped))))
	(geo-rad-to-deg elev_rad))

(defun daylight? (lat_deg lon_deg sun_lat_deg sun_lon_deg)
	; Returns :t if sun is above the geometric horizon, :nil if in night
	(> (solar-elevation lat_deg lon_deg sun_lat_deg sun_lon_deg) 0.0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Local Sunrise, Solar Noon, Sunset, Day Length
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun solar-times (lat_deg lon_deg sun_lat_deg eot_min)
	; Calculates sunrise, solar noon, sunset (UTC hours), and day length in hours
	; Returns: (sunrise_utc noon_utc sunset_utc day_length_hours state)
	(defq phi (geo-deg-to-rad lat_deg)
		delta (geo-deg-to-rad sun_lat_deg)
		cos_zenith -0.0145439 ; cos(90.833 deg)
		denom (* (cos phi) (cos delta)))
	(if (< (abs denom) 0.0001)
		(setq denom 0.0001))
	(defq cos_h0 (/ (- cos_zenith (* (sin phi) (sin delta))) denom)
		noon_utc (+ (- 12.0 (/ lon_deg 15.0)) (/ (neg eot_min) 60.0))
		noon_utc (% (+ (% noon_utc 24.0) 24.0) 24.0))
	(cond
		((> cos_h0 1.0)
			; Polar night: Sun never rises
			(list :nil noon_utc :nil 0.0 :polar_night))
		((< cos_h0 -1.0)
			; Midnight sun: Sun never sets
			(list :nil noon_utc :nil 24.0 :polar_day))
		(:t
			(defq h0_rad (n2q (acos (n2d (clamp cos_h0 -1.0 1.0))))
				h0_hours (* (/ h0_rad +fp_pi) 12.0)
				sunrise_utc (% (+ (- noon_utc h0_hours) 24.0) 24.0)
				sunset_utc (% (+ (+ noon_utc h0_hours) 24.0) 24.0)
				day_len (* 2.0 h0_hours))
			(list sunrise_utc noon_utc sunset_utc day_len :normal))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Terminator Curve Point Computation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun terminator-lat (lon_rad sun_lon_rad sun_lat_rad)
	; Calculates latitude in radians of the day/night terminator at a given longitude
	(defq tan_delta (tan sun_lat_rad))
	(if (< (abs tan_delta) 0.0001)
		(setq tan_delta (if (>= tan_delta 0.0) 0.0001 -0.0001)))
	(defq d_lon (- lon_rad sun_lon_rad)
		tan_phi (/ (neg (cos d_lon)) tan_delta))
	(n2q (atan (n2d tan_phi))))
