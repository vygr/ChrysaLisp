;debug options
(case :nil
(0 (import "lib/debug/frames.inc"))
(1 (import "lib/debug/profile.inc"))
(2 (import "lib/debug/debug.inc")))

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/vector.inc")
(import "lib/date/date.inc")
(import "./astronomy.inc")
(import "./continents.inc")

(enums +select 0
	(enum main timer tip))

(enums +event 0
	(enum close btn_now btn_prev_h btn_next_h btn_prev_d btn_next_d btn_play
		btn_city_0 btn_city_1 btn_city_2 btn_city_3 btn_city_4 btn_city_5))

(defq +font_tiny (create-font "fonts/OpenSans-Regular.ctf" 10)
	+font_small (create-font "fonts/OpenSans-Regular.ctf" 11)
	+font_sub (create-font "fonts/OpenSans-Regular.ctf" 13)
	+font_title (create-font "fonts/OpenSans-Bold.ctf" 15)
	+font_btn (create-font "fonts/OpenSans-Regular.ctf" 11))

(defq *canvas_width* 480
	*canvas_height* 240
	*sim_offset_sec* 0
	*animating* :nil
	*selected_city_id* 0)

; Cities directory: (name country lat lon utc_offset_hours)
(defq *cities* (list
	(list "London" "UK" 51.5 0.0 1.0)
	(list "New York" "USA" 40.7 -74.0 -4.0)
	(list "San Francisco" "USA" 37.8 -122.4 -7.0)
	(list "Tokyo" "Japan" 35.7 139.7 9.0)
	(list "Sydney" "Australia" -33.9 151.2 10.0)
	(list "Dubai" "UAE" 25.3 55.3 4.0)
	(list "Singapore" "Singapore" 1.3 103.8 8.0)
	(list "Cairo" "Egypt" 30.0 31.2 3.0)
	(list "Sao Paulo" "Brazil" -23.5 -46.6 -3.0)
	(list "Honolulu" "USA" 21.3 -157.8 -10.0)))

(defun config-load ()
	(defq path (cat *env_home* "sunclock.tre"))
	(when (defq stream (file-stream path))
		(defq conf (tree-load stream))
		(when conf
			(defq cid (. conf :find :selected_city))
			(when (and cid (num? cid) (<= 0 cid 5))
				(setq *selected_city_id* cid)))))

(defun config-save ()
	(defq path (cat *env_home* "sunclock.tre"))
	(when (defq stream (file-stream path +file_open_write))
		(tree-save stream (scatter (Emap) :selected_city *selected_city_id*))))

(defun format-hours (h)
	(defq norm_h (% (+ (% h 24.0) 24.0) 24.0)
		ih (n2i (floor norm_h))
		rem_m (* (frac norm_h) 60.0)
		im (n2i (floor (+ rem_m 0.5))))
	(if (>= im 60)
		(setq im 0 ih (% (inc ih) 24)))
	(cat (pad ih 2 "0") ":" (pad im 2 "0")))

(defun format-duration (h)
	(defq total_m (n2i (floor (+ (* h 60.0) 0.5)))
		hh (/ total_m 60)
		mm (% total_m 60))
	(cat (str hh) "h " (pad mm 2 "0") "m"))

(defun format-geo-coord (val is_lat)
	(defq abs_v (abs val)
		deg (n2i (floor abs_v))
		min_val (n2i (floor (+ (* (frac abs_v) 60.0) 0.5)))
		hem (if is_lat
			(if (>= val 0.0) "N" "S")
			(if (>= val 0.0) "E" "W")))
	(cat (str deg) "o" (pad min_val 2 "0") "'" hem))

(defun render-world-map (canvas unix_sec w_i h_i)
	(defq w (n2f w_i) h (n2f h_i))
	(.-> canvas
		(:fill 0xff132338) ; Deep atmospheric ocean blue
		(:set_canvas_flags +canvas_flag_antialias))
	; 1. Graticule Lines (Equator, Prime Meridian, Tropics)
	(defq lines_path (path)
		eq_y (* h 0.5)
		pm_x (* w 0.5)
		tropic_n_y (* (/ (- 90.0 23.44) 180.0) h)
		tropic_s_y (* (/ (+ 90.0 23.44) 180.0) h))
	(push lines_path 0.0 eq_y w eq_y)
	(push lines_path pm_x 0.0 pm_x h)
	(push lines_path 0.0 tropic_n_y w tropic_n_y)
	(push lines_path 0.0 tropic_s_y w tropic_s_y)
	(defq graticule_stroke (path-stroke-polyline lines_path 1.0 +join_miter +cap_butt +cap_butt))
	(.-> canvas
		(:set_color 0x24ffffff)
		(:fpoly 0.0 0.0 +winding_none_zero (list graticule_stroke)))

	; 2. Continents: Landmass Fills & Outlines
	(bind '(fills outlines) (create-continent-paths w h))
	(.-> canvas
		(:set_color 0xff2a4637) ; Rich earth sage
		(:fpoly 0.0 0.0 +winding_none_zero fills)
		(:set_color 0xff3e634e) ; Crisp coastline stroke
		(:fpoly 0.0 0.0 +winding_none_zero outlines))

	; 3. Solar Ephemeris & Subsolar Point
	(bind '(sun_lon_deg sun_lat_deg eot_min season) (solar-ephemeris unix_sec))
	(defq sun_lon_rad (geo-deg-to-rad sun_lon_deg)
		sun_lat_rad (geo-deg-to-rad sun_lat_deg))

	; 4. Night Shadow Polygon & Twilight Boundary
	(defq step_x 4.0
		n_steps (n2i (/ w step_x))
		curve_pts (list)
		shadow_poly (path))
	(each (lambda (i)
		(defq px (* (n2f i) step_x)
			lon_rad (- (* (/ px w) +fp_2pi) +fp_pi)
			lat_rad (terminator-lat lon_rad sun_lon_rad sun_lat_rad)
			py (* (/ (- +fp_hpi lat_rad) +fp_pi) h))
		(push curve_pts px py))
		(range 0 (inc n_steps)))
	(if (>= sun_lat_deg 0.0)
		; Northern summer: North is day, South is night
		(progn
			(push shadow_poly 0.0 h)
			(push shadow_poly w h)
			(each (lambda (i)
				(defq idx (* i 2)
					px (elem-get curve_pts idx)
					py (elem-get curve_pts (inc idx)))
				(push shadow_poly px py))
				(range n_steps -1 -1))
			(push shadow_poly 0.0 h))
		; Northern winter: North is night, South is day
		(progn
			(push shadow_poly 0.0 0.0)
			(push shadow_poly w 0.0)
			(each (lambda (i)
				(defq idx (* i 2)
					px (elem-get curve_pts idx)
					py (elem-get curve_pts (inc idx)))
				(push shadow_poly px py))
				(range n_steps -1 -1))
			(push shadow_poly 0.0 0.0)))
	(defq twilight_path (path))
	(each (lambda (i)
		(defq idx (* i 2))
		(push twilight_path (elem-get curve_pts idx) (elem-get curve_pts (inc idx))))
		(range 0 (inc n_steps)))
	(defq twilight_stroke (path-stroke-polyline twilight_path 1.5 +join_round +cap_round +cap_round))
	(.-> canvas
		(:set_color 0x99030612) ; Translucent night shadow
		(:fpoly 0.0 0.0 +winding_none_zero (list shadow_poly))
		(:set_color 0x88f5a623) ; Twilight amber glow
		(:fpoly 0.0 0.0 +winding_none_zero (list twilight_stroke)))

	; 5. Radiant Sun Marker (Subsolar point)
	(bind '(sun_px sun_py) (geo-to-canvas sun_lon_deg sun_lat_deg w h))
	(defq sun_disc (path-gen-arc sun_px sun_py 0.0 +fp_2pi 5.0 (path))
		sun_halo (path-stroke-polyline (path-gen-arc sun_px sun_py 0.0 +fp_2pi 9.0 (path)) 1.5 +join_miter +cap_round +cap_round)
		sun_rays (path-stroke-polyline (path-gen-arc sun_px sun_py 0.0 +fp_2pi 13.5 (path)) 1.0 +join_miter +cap_round +cap_round))
	(.-> canvas
		(:set_color 0x33ffe066)
		(:fpoly 0.0 0.0 +winding_none_zero (list sun_rays))
		(:set_color 0x88ffd700)
		(:fpoly 0.0 0.0 +winding_none_zero (list sun_halo))
		(:set_color 0xfffff066)
		(:fpoly 0.0 0.0 +winding_none_zero (list sun_disc)))

	; 6. Midnight / Moon Marker (Antipodal point)
	(defq moon_lon_deg (norm-angle-180 (+ sun_lon_deg 180.0))
		moon_lat_deg (neg sun_lat_deg))
	(bind '(moon_px moon_py) (geo-to-canvas moon_lon_deg moon_lat_deg w h))
	(defq moon_disc (path-gen-arc moon_px moon_py 0.0 +fp_2pi 3.5 (path))
		moon_halo (path-stroke-polyline (path-gen-arc moon_px moon_py 0.0 +fp_2pi 6.5 (path)) 1.0 +join_miter +cap_round +cap_round))
	(.-> canvas
		(:set_color 0x4488aacc)
		(:fpoly 0.0 0.0 +winding_none_zero (list moon_halo))
		(:set_color 0xffc8dcfa)
		(:fpoly 0.0 0.0 +winding_none_zero (list moon_disc)))

	; 7. Cities Markers
	(each (lambda (i)
		(defq c (elem-get *cities* i)
			clat (elem-get c 2)
			clon (elem-get c 3)
			is_day (daylight? clat clon sun_lat_deg sun_lon_deg))
		(bind '(cpx cpy) (geo-to-canvas clon clat w h))
		(defq dot (path-gen-arc cpx cpy 0.0 +fp_2pi (if (= i *selected_city_id*) 3.8 2.6) (path))
			col (if is_day 0xffffe066 0xff5c8ae6))
		(.-> canvas
			(:set_color col)
			(:fpoly 0.0 0.0 +winding_none_zero (list dot)))
		(when (= i *selected_city_id*)
			(defq sel_ring (path-stroke-polyline (path-gen-arc cpx cpy 0.0 +fp_2pi 6.5 (path)) 1.4 +join_miter +cap_round +cap_round))
			(.-> canvas
				(:set_color 0xeeffffff)
				(:fpoly 0.0 0.0 +winding_none_zero (list sel_ring)))))
		(range 0 (length *cities*)))
	(. canvas :swap 0))

(ui-window *window* ()
	(ui-title-bar _ "World Sun Clock" (0xea19) +event_close)
	; Controls Bar: Simulation & Time Travel
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-label *utc_time_label* (:text "UTC: 00:00:00" :font +font_small :flow_flags +flow_flag_align_vcenter))
		(ui-flow _ (:flow_flags +flow_right)
			(. (ui-button *btn_prev_d* (:text "-1d" :font +font_btn)) :connect +event_btn_prev_d)
			(. (ui-button *btn_prev_h* (:text "-1h" :font +font_btn)) :connect +event_btn_prev_h)
			(. (ui-button *btn_now* (:text "Now" :font +font_btn)) :connect +event_btn_now)
			(. (ui-button *btn_next_h* (:text "+1h" :font +font_btn)) :connect +event_btn_next_h)
			(. (ui-button *btn_next_d* (:text "+1d" :font +font_btn)) :connect +event_btn_next_d)
			(. (ui-button *btn_play* (:text "Play" :font +font_btn)) :connect +event_btn_play)))
	; Hero Card: Selected City Details
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-label *hero_city* (:text "London, UK" :font +font_title))
		(ui-label *hero_status_badge* (:text "Daylight" :font +font_sub :border 0
			:flow_flags (logior +flow_flag_align_hright +flow_flag_align_vcenter))))
	; Hero Card: Solar Elevation & Times
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-label *hero_times* (:text "Rise 00:00  Noon 00:00  Set 00:00" :font +font_small))
		(ui-label *hero_day_len* (:text "Day: 12h 00m" :font +font_small
			:flow_flags (logior +flow_flag_align_hright +flow_flag_align_vcenter))))
	; World Map Canvas
	(ui-canvas *map_canvas* *canvas_width* *canvas_height* 1)
	; Subsolar Telemetry Bar
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-label *subsolar_label* (:text "Subsolar: 0o00'N 0o00'W" :font +font_tiny))
		(ui-label *season_label* (:text "Season: Equinox" :font +font_tiny
			:flow_flags (logior +flow_flag_align_hright +flow_flag_align_vcenter))))
	; Quick World City Grid (2 rows x 3 columns)
	(ui-grid _ (:grid_width 3 :font +font_btn)
		(. (ui-button *btn_city_0* (:text "London")) :connect +event_btn_city_0)
		(. (ui-button *btn_city_1* (:text "New York")) :connect +event_btn_city_1)
		(. (ui-button *btn_city_2* (:text "San Francisco")) :connect +event_btn_city_2)
		(. (ui-button *btn_city_3* (:text "Tokyo")) :connect +event_btn_city_3)
		(. (ui-button *btn_city_4* (:text "Sydney")) :connect +event_btn_city_4)
		(. (ui-button *btn_city_5* (:text "Dubai")) :connect +event_btn_city_5))
	; Footer Attribution
	(ui-label *footer_label* (:text "Equirectangular Projection - Solar Terminator Model" :font *env_small_terminal_font*
		:flow_flags +flow_flag_align_hcenter)))

(defun get-effective-time ()
	(+ (/ (pii-time) 1000000) *sim_offset_sec*))

(defun update-ui ()
	(defq now_sec (get-effective-time)
		utc_date (date now_sec))
	(bind '(sec min hr mday mo yr wk) utc_date)
	(defq utc_str (cat "UTC: " (day-of-the-week wk) " " (month-of-the-year mo) " " (pad mday 2 "0") " "
		(pad hr 2 "0") ":" (pad min 2 "0") ":" (pad sec 2 "0"))
		sim_note (cond
			((= *sim_offset_sec* 0) "")
			((> *sim_offset_sec* 0) (cat " [+" (str (/ *sim_offset_sec* 3600)) "h]"))
			(:t (cat " [" (str (/ *sim_offset_sec* 3600)) "h]"))))
	(def *utc_time_label* :text (cat utc_str sim_note))
	(def *btn_play* :text (if *animating* "Pause" "Play"))

	; Solar Ephemeris
	(bind '(sun_lon_deg sun_lat_deg eot_min season) (solar-ephemeris now_sec))
	(def *subsolar_label* :text (cat "Sun Zenith: " (format-geo-coord sun_lat_deg :t) " "
		(format-geo-coord sun_lon_deg :nil) " (EoT " (str (n2i (floor (+ eot_min 0.5)))) "m)"))
	(def *season_label* :text (cat "Season: " season))

	; Selected City Details
	(defq city (elem-get *cities* *selected_city_id*)
		cname (elem-get city 0)
		ccountry (elem-get city 1)
		clat (elem-get city 2)
		clon (elem-get city 3)
		coff (elem-get city 4)
		city_sec (+ now_sec (n2i (* coff 3600.0)))
		cdate (date city_sec))
	(bind '(cs cmin chr & & & &) cdate)
	(defq local_time_str (cat (pad chr 2 "0") ":" (pad cmin 2 "0"))
		elev (solar-elevation clat clon sun_lat_deg sun_lon_deg)
		is_day (> elev 0.0)
		elev_int (n2i (floor (+ (abs elev) 0.5)))
		elev_str (if (>= elev 0.0) (cat "+" (str elev_int)) (cat "-" (str elev_int))))
	(def *hero_city* :text (cat cname ", " ccountry " (" local_time_str ")"))
	(def *hero_status_badge* :text (if is_day
			(cat "Daylight (" elev_str "o)")
			(cat "Night (" elev_str "o)"))
		:color (if is_day 0xff00e676 0xff253a5c)
		:ink_color (if is_day +argb_black +argb_white))

	; Solar Times for Selected City
	(bind '(sunrise_utc noon_utc sunset_utc day_len state)
		(solar-times clat clon sun_lat_deg eot_min))
	(cond
		((eql state :polar_day)
			(def *hero_times* :text "Midnight Sun - 24h Daylight")
			(def *hero_day_len* :text "Day: 24h 00m"))
		((eql state :polar_night)
			(def *hero_times* :text "Polar Night - 24h Darkness")
			(def *hero_day_len* :text "Day: 0h 00m"))
		(:t
			(defq sunrise_local (+ sunrise_utc coff)
				noon_local (+ noon_utc coff)
				sunset_local (+ sunset_utc coff))
			(def *hero_times* :text (cat "Rise " (format-hours sunrise_local)
				"  Noon " (format-hours noon_local)
				"  Set " (format-hours sunset_local)))
			(def *hero_day_len* :text (cat "Day: " (format-duration day_len)))))

	; Map Canvas
	(render-world-map *map_canvas* now_sec *canvas_width* *canvas_height*)

	; Quick City Grid Buttons
	(defq btns (list *btn_city_0* *btn_city_1* *btn_city_2* *btn_city_3* *btn_city_4* *btn_city_5*))
	(each (lambda (i)
		(defq c (elem-get *cities* i)
			cn (elem-get c 0)
			la (elem-get c 2)
			lo (elem-get c 3)
			of (elem-get c 4)
			c_sec (+ now_sec (n2i (* of 3600.0)))
			d (date c_sec)
			h (elem-get d 2)
			m (elem-get d 1)
			day? (daylight? la lo sun_lat_deg sun_lon_deg)
			badge (if day? "Day " "Night ")
			btn (elem-get btns i))
		(def btn :text (cat badge cn " " (pad h 2 "0") ":" (pad m 2 "0"))))
		(range 0 6))

	(bind '(x y) (. *window* :get_pos))
	(bind '(w h) (. *window* :pref_size))
	(bind '(x y w h) (view-fit x y w h))
	(. *window* :change_dirty x y w h :t))

(defun main ()
	(config-load)
	(defq select (task-mboxes +select_size) *running* :t
		tick_interval 1000000) ; 1 second normal tick
	(def *window* :tip_mbox (elem-get select +select_tip))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (.-> *window* (:change x y w h :t) :dirty_all))
	(update-ui)
	(mail-timeout (elem-get select +select_timer) tick_interval 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_timer
				(when *animating*
					(setq *sim_offset_sec* (+ *sim_offset_sec* 3600))) ; Advance 1 hour per tick
				(update-ui)
				(defq next_interval (if *animating* 150000 tick_interval))
				(mail-timeout (elem-get select +select_timer) next_interval 0))
			(+select_tip
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			(+select_main
				(defq id (getf *msg* +ev_msg_target_id))
				(cond
					((= id +event_close)
						(setq *running* :nil))
					((= id +event_btn_now)
						(setq *sim_offset_sec* 0 *animating* :nil)
						(update-ui))
					((= id +event_btn_prev_h)
						(setq *sim_offset_sec* (- *sim_offset_sec* 3600))
						(update-ui))
					((= id +event_btn_next_h)
						(setq *sim_offset_sec* (+ *sim_offset_sec* 3600))
						(update-ui))
					((= id +event_btn_prev_d)
						(setq *sim_offset_sec* (- *sim_offset_sec* 86400))
						(update-ui))
					((= id +event_btn_next_d)
						(setq *sim_offset_sec* (+ *sim_offset_sec* 86400))
						(update-ui))
					((= id +event_btn_play)
						(setq *animating* (not *animating*))
						(update-ui)
						(mail-timeout (elem-get select +select_timer) (if *animating* 150000 tick_interval) 0))
					((and (>= id +event_btn_city_0) (<= id +event_btn_city_5))
						(setq *selected_city_id* (- id +event_btn_city_0))
						(update-ui)
						(config-save))
					((. *window* :event *msg*))))))
	(config-save)
	(gui-sub-rpc *window*))
