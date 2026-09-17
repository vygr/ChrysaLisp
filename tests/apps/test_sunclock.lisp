(import "usr/env.inc")
(import "././utils.inc")
(import "./../../apps/desktop/sunclock/astronomy.inc")
(import "./../../apps/desktop/sunclock/continents.inc")

(report-header "World Sun Clock Tests")

; 1. Trigonometric Inverses
(assert-true "atan 0.0" (< (abs (atan 0.0)) 0.0001))
(assert-true "atan 1.0 is pi/4" (< (abs (- (atan 1.0) (/ +fp_pi 4.0))) 0.0002))
(assert-true "atan2 1 1 is pi/4" (< (abs (- (atan2 1.0 1.0) (/ +fp_pi 4.0))) 0.0002))
(assert-true "atan2 0 1 is 0" (< (abs (atan2 0.0 1.0)) 0.0001))
(assert-true "atan2 1 0 is pi/2" (< (abs (- (atan2 1.0 0.0) +fp_hpi)) 0.0001))
(assert-true "asin 0.0 is 0" (< (abs (asin 0.0)) 0.0001))
(assert-true "asin 1.0 is pi/2" (< (abs (- (asin 1.0) +fp_hpi)) 0.0001))
(assert-true "acos 1.0 is 0" (< (abs (acos 1.0)) 0.0001))
(assert-true "acos 0.0 is pi/2" (< (abs (- (acos 0.0) +fp_hpi)) 0.0001))

; 2. Solar Ephemeris on Known Dates
; June 21, 2024 ~ Summer Solstice (t = 1718971200)
(defq summer_eph (solar-ephemeris 1718971200)
	summer_lat (elem-get summer_eph 1))
(assert-true "summer solstice declination ~ +23.4 deg" (and (> summer_lat 23.0) (< summer_lat 23.6)))

; Dec 21, 2023 ~ Winter Solstice (t = 1703160000)
(defq winter_eph (solar-ephemeris 1703160000)
	winter_lat (elem-get winter_eph 1))
(assert-true "winter solstice declination ~ -23.4 deg" (and (< winter_lat -23.0) (> winter_lat -23.6)))

; Sep 22, 2024 ~ Autumnal Equinox (t = 1727006400)
(defq equinox_eph (solar-ephemeris 1727006400)
	equinox_lat (elem-get equinox_eph 1))
(assert-true "equinox declination near 0 deg" (< (abs equinox_lat) 1.5))

; Subsolar longitude at 12:00 UTC should be near 0 deg longitude
(defq noon_eph (solar-ephemeris 1727006400) ; 12:00 UTC
	noon_lon (elem-get noon_eph 0))
(assert-true "noon subsolar longitude near Greenwich meridian" (< (abs noon_lon) 4.0))

; 3. Solar Elevation & Zenith
(defq elev_zenith (solar-elevation summer_lat (elem-get summer_eph 0) summer_lat (elem-get summer_eph 0)))
(assert-true "elevation at subsolar point is 90 deg" (< (abs (- elev_zenith 90.0)) 0.1))

(defq elev_nadir (solar-elevation (neg summer_lat) (norm-angle-180 (+ (elem-get summer_eph 0) 180.0)) summer_lat (elem-get summer_eph 0)))
(assert-true "elevation at antipodal point is -90 deg" (< (abs (- elev_nadir -90.0)) 0.1))

(assert-true "daylight test at zenith is true" (daylight? summer_lat (elem-get summer_eph 0) summer_lat (elem-get summer_eph 0)))
(assert-true "daylight test at nadir is nil" (not (daylight? (neg summer_lat) (norm-angle-180 (+ (elem-get summer_eph 0) 180.0)) summer_lat (elem-get summer_eph 0))))

; 4. Local Solar Times (Sunrise, Noon, Sunset)
(defq st (solar-times 0.0 0.0 0.0 0.0) ; Equator at equinox
	rise (elem-get st 0)
	noon (elem-get st 1)
	set_val (elem-get st 2)
	day_len (elem-get st 3))
(assert-true "equator equinox day length is ~12.1 hours" (and (> day_len 11.9) (< day_len 12.3)))
(assert-true "equator equinox solar noon is 12:00 UTC" (< (abs (- noon 12.0)) 0.01))
(assert-true "equator sunrise before noon" (< rise noon))
(assert-true "equator sunset after noon" (> set_val noon))

; 5. Terminator Curve Geometry
(defq term_lat_eq (terminator-lat 0.0 0.0 0.409)) ; 23.44 deg = ~0.409 rad
(assert-true "terminator latitude at subsolar meridian is negative" (< term_lat_eq 0.0))

; 6. Cartographic Projection & Continent Paths
(bind '(cx cy) (geo-to-canvas 0.0 0.0 480.0 240.0))
(assert-true "center (0,0) maps to canvas center (240, 120)" (and (< (abs (- cx 240.0)) 0.01) (< (abs (- cy 120.0)) 0.01)))

(bind '(fills outlines) (create-continent-paths 480.0 240.0))
(assert-true "9 continent fill polygons generated" (= (length fills) 9))
(assert-true "9 continent outline strokes generated" (= (length outlines) 9))

; 7. Config Tree Serialization
(defq test_conf_file (cat *env_home* "test_sunclock.tre"))
(when (defq stream (file-stream test_conf_file +file_open_write))
	(tree-save stream (scatter (Emap) :selected_city 3)))
(defq loaded_conf (when (defq stream (file-stream test_conf_file)) (tree-load stream)))
(assert-true "config roundtrip selected city" (= (. loaded_conf :find :selected_city) 3))

(print-summary)

(stream-flush (io-stream "stdout"))
(task-sleep 500000)
(pii-exit)
