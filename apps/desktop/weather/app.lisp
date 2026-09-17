;debug options
(case :nil
(0 (import "lib/debug/frames.inc"))
(1 (import "lib/debug/profile.inc"))
(2 (import "lib/debug/debug.inc")))

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/net/app.inc")
(import "lib/net/http.inc")
(import "lib/net/json.inc")
(import "lib/net/url.inc")

(enums +event 0
	(enum close refresh unit_toggle city_search)
	(enum city_0 city_1 city_2 city_3 city_4 city_5))

(enums +select 0
	(enum main tip timer worker))

(defq
	*config* :nil
	*config_version* 1
	*config_file* (cat *env_home* "weather.tre")
	*city* "London"
	*unit_c* :t
	*quick_cities* (list "London" "New York" "Tokyo" "Paris" "SF" "Sydney")
	*btn_city_0* :nil
	*btn_city_1* :nil
	*btn_city_2* :nil
	*btn_city_3* :nil
	*btn_city_4* :nil
	*btn_city_5* :nil
	*weather_data* :nil
	*canvas_size* 120
	+font_big_temp (create-font "fonts/OpenSans-Bold.ctf" 32)
	+font_sub (create-font "fonts/OpenSans-Bold.ctf" 15)
	+font_btn (create-font "fonts/OpenSans-Regular.ctf" 14)
	+font_small (create-font "fonts/OpenSans-Regular.ctf" 13)
	+font_tiny (create-font "fonts/OpenSans-Regular.ctf" 11))

(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:city "London"
		:unit_c :t
		:quick_cities (list "London" "New York" "Tokyo" "Paris" "SF" "Sydney")))

(defun config-load ()
	(defq old_config :nil)
	(if (defq stream (file-stream *config_file*))
		(setq old_config (tree-load stream)))
	(if (or (not old_config) (/= (. old_config :find :version) *config_version*))
		(setq *config* (config-default))
		(setq *config* old_config))
	(setq *city* (. *config* :find :city)
		*quick_cities* (. *config* :find :quick_cities))
	(if (not (str? *city*))
		(setq *city* "London"))
	(defq raw_unit (. *config* :find :unit_c))
	(setq *unit_c* (cond
		((or (eql raw_unit :nil) (eql raw_unit "F") (eql raw_unit 'F) (eql raw_unit :f) (eql raw_unit :F)) :nil)
		(:t :t)))
	(if (not (list?? *quick_cities*))
		(setq *quick_cities* (list "London" "New York" "Tokyo" "Paris" "SF" "Sydney")))
	(setq *quick_cities* (map (lambda (c) (if (str? c) c (str c))) *quick_cities*))
	(while (< (length *quick_cities*) 6)
		(push *quick_cities* "London")))

(defun config-save ()
	(if (not *config*)
		(setq *config* (Emap)))
	(scatter *config*
		:version *config_version*
		:city *city*
		:unit_c (if *unit_c* :t :nil)
		:quick_cities *quick_cities*)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)))

(config-load)

(defun draw-sun (canvas cx cy r)
	(defq disc (path-gen-arc cx cy 0.0 +fp_2pi r (path)))
	(.-> canvas
		(:set_color 0xffffcc00)
		(:fpoly 0.0 0.0 +winding_none_zero (list disc)))
	(defq ray_paths (list))
	(each (lambda (i)
		(defq a (/ (* (n2f i) +fp_2pi) 8.0)
			r1 (+ r 5.0) r2 (+ r 13.0)
			x1 (+ cx (* r1 (cos a))) y1 (+ cy (* r1 (sin a)))
			x2 (+ cx (* r2 (cos a))) y2 (+ cy (* r2 (sin a))))
		(push ray_paths (path-stroke-polyline (path x1 y1 x2 y2) 1.5 +join_miter +cap_round +cap_round)))
		(range 0 8))
	(. canvas :fpoly 0.0 0.0 +winding_none_zero ray_paths))

(defun draw-cloud (canvas cx cy scale col)
	(defq base (path-gen-ellipse cx (+ cy (* scale 8.0)) (* scale 28.0) (* scale 14.0) (path))
		puff1 (path-gen-ellipse (- cx (* scale 10.0)) (- cy (* scale 2.0)) (* scale 16.0) (* scale 15.0) (path))
		puff2 (path-gen-ellipse (+ cx (* scale 8.0)) (+ cy (* scale 2.0)) (* scale 14.0) (* scale 13.0) (path)))
	(.-> canvas
		(:set_color col)
		(:fpoly 0.0 0.0 +winding_none_zero (list base puff1 puff2))))

(defun draw-rain (canvas cx cy)
	(draw-cloud canvas cx (- cy 8.0) 1.0 0xff90a0b0)
	(defq rain_drops (list)
		drops '((-16.0 16.0) (-6.0 18.0) (4.0 16.0) (14.0 18.0)))
	(each (lambda ((dx dy))
		(defq x1 (+ cx dx) y1 (+ cy dy)
			x2 (- x1 3.0) y2 (+ y1 11.0))
		(push rain_drops (path-stroke-polyline (path x1 y1 x2 y2) 1.5 +join_miter +cap_round +cap_round)))
		drops)
	(.-> canvas
		(:set_color 0xff50b0ff)
		(:fpoly 0.0 0.0 +winding_none_zero rain_drops)))

(defun draw-snow (canvas cx cy)
	(draw-cloud canvas cx (- cy 8.0) 1.0 0xffd0dbe5)
	(defq snow_flakes (list)
		flakes '((-14.0 18.0) (-2.0 20.0) (10.0 18.0)))
	(each (lambda ((dx dy))
		(defq fx (+ cx dx) fy (+ cy dy))
		(push snow_flakes (path-stroke-polyline (path (- fx 3.0) fy (+ fx 3.0) fy) 1.2 +join_miter +cap_round +cap_round))
		(push snow_flakes (path-stroke-polyline (path fx (- fy 3.0) fx (+ fy 3.0)) 1.2 +join_miter +cap_round +cap_round)))
		flakes)
	(.-> canvas
		(:set_color +argb_white)
		(:fpoly 0.0 0.0 +winding_none_zero snow_flakes)))

(defun draw-thunder (canvas cx cy)
	(draw-cloud canvas cx (- cy 10.0) 1.0 0xff505860)
	(defq bolt (path
		cx (+ cy 6.0)
		(- cx 8.0) (+ cy 20.0)
		(+ cx 2.0) (+ cy 20.0)
		(- cx 4.0) (+ cy 36.0)
		(+ cx 8.0) (+ cy 18.0)
		cx (+ cy 18.0)))
	(.-> canvas
		(:set_color 0xffffdd00)
		(:fpoly 0.0 0.0 +winding_none_zero (list bolt))))

(defun render-weather-icon (canvas code)
	(.-> canvas (:fill 0) (:set_canvas_flags +canvas_flag_antialias))
	(defq cx (* (n2f *canvas_size*) 0.5) cy (* (n2f *canvas_size*) 0.5))
	(cond
		; Clear / Sunny
		((= code 113)
			(draw-sun canvas cx cy 22.0))
		; Partly cloudy
		((= code 116)
			(draw-sun canvas (+ cx 16.0) (- cy 12.0) 14.0)
			(draw-cloud canvas (- cx 4.0) (+ cy 6.0) 0.95 0xffd8e0e8))
		; Cloudy / Overcast / Fog
		((or (= code 119) (= code 122) (= code 143) (= code 248) (= code 260))
			(draw-cloud canvas (- cx 6.0) (- cy 6.0) 0.9 0xff707884)
			(draw-cloud canvas (+ cx 4.0) (+ cy 4.0) 0.95 0xffd0d8e2))
		; Thunderstorms
		((or (= code 200) (= code 386) (= code 389) (= code 392) (= code 395))
			(draw-thunder canvas cx cy))
		; Snow / Sleet / Ice
		((or (= code 179) (= code 182) (= code 227) (= code 230)
			(= code 323) (= code 326) (= code 329) (= code 332)
			(= code 335) (= code 338) (= code 350) (= code 368) (= code 371))
			(draw-snow canvas cx cy))
		; Default: Rain / Drizzle / Showers
		(:t (draw-rain canvas cx cy)))
	(. canvas :swap 0))

(ui-window *window* ()
	(ui-title-bar _ "Weather" (0xea19) +event_close)
	; Row 1: Search & Controls
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-textfield *search_input* (:color +argb_white :clear_text *city* :hint_text "Search city..." :font +font_small)) :connect +event_refresh)
		(. (ui-button *btn_refresh* (:text "Fetch" :font +font_small)) :connect +event_refresh)
		(. (ui-button *btn_unit* (:text (if *unit_c* "F" "C") :font +font_small)) :connect +event_unit_toggle))
	; Row 2: Quick City Buttons (2 rows of 3 columns)
	(ui-grid _ (:grid_width 3 :font +font_btn)
		(. (ui-button *btn_city_0* (:text (elem-get *quick_cities* 0))) :connect +event_city_0)
		(. (ui-button *btn_city_1* (:text (elem-get *quick_cities* 1))) :connect +event_city_1)
		(. (ui-button *btn_city_2* (:text (elem-get *quick_cities* 2))) :connect +event_city_2)
		(. (ui-button *btn_city_3* (:text (elem-get *quick_cities* 3))) :connect +event_city_3)
		(. (ui-button *btn_city_4* (:text (elem-get *quick_cities* 4))) :connect +event_city_4)
		(. (ui-button *btn_city_5* (:text (elem-get *quick_cities* 5))) :connect +event_city_5))
	; Row 3: Main Weather Hero
	(ui-flow _ (:flow_flags +flow_down_fill)
		(ui-canvas *icon_canvas* *canvas_size* *canvas_size* 1)
		(ui-label *temp_label* (:text "00 C" :font +font_big_temp
			:flow_flags (logior +flow_flag_align_hcenter +flow_flag_align_vcenter)))
		(ui-label *condition_label* (:text "Patchy rain nearby" :font +font_sub
			:flow_flags (logior +flow_flag_align_hcenter +flow_flag_align_vcenter)))
		(ui-label *feels_label* (:text "Feels like 00 C" :font +font_small
			:flow_flags (logior +flow_flag_align_hcenter +flow_flag_align_vcenter)))
		(ui-label *location_label* (:text "Brompton, United Kingdom" :font +font_small
			:flow_flags (logior +flow_flag_align_hcenter +flow_flag_align_vcenter))))
	; Row 4: Metrics 2x2 Grid
	(ui-grid _ (:grid_width 2 :font +font_small)
		(ui-label *humidity_label* (:text "Humidity: 100%"))
		(ui-label *wind_label* (:text "Wind: 100 km/h WSW"))
		(ui-label *pressure_label* (:text "Pressure: 1013 hPa"))
		(ui-label *vis_label* (:text "Visibility: 10 km")))
	; Row 5: 3-Day Forecast
	(ui-title _ (:text "3-Day Forecast" :color *env_title_col* :font +font_sub))
	(ui-grid _ (:grid_width 3)
		(ui-flow _ (:flow_flags +flow_down_fill)
			(ui-label *fc_d1_date* (:text "00-00" :font +font_sub :flow_flags +flow_flag_align_hcenter))
			(ui-label *fc_d1_desc* (:text "Patchy rain nearby" :font +font_tiny :flow_flags +flow_flag_align_hcenter))
			(ui-label *fc_d1_temp* (:text "00 / 00 C" :font +font_small :flow_flags +flow_flag_align_hcenter)))
		(ui-flow _ (:flow_flags +flow_down_fill)
			(ui-label *fc_d2_date* (:text "00-00" :font +font_sub :flow_flags +flow_flag_align_hcenter))
			(ui-label *fc_d2_desc* (:text "Patchy rain nearby" :font +font_tiny :flow_flags +flow_flag_align_hcenter))
			(ui-label *fc_d2_temp* (:text "00 / 00 C" :font +font_small :flow_flags +flow_flag_align_hcenter)))
		(ui-flow _ (:flow_flags +flow_down_fill)
			(ui-label *fc_d3_date* (:text "00-00" :font +font_sub :flow_flags +flow_flag_align_hcenter))
			(ui-label *fc_d3_desc* (:text "Patchy rain nearby" :font +font_tiny :flow_flags +flow_flag_align_hcenter))
			(ui-label *fc_d3_temp* (:text "00 / 00 C" :font +font_small :flow_flags +flow_flag_align_hcenter))))
	; Row 6: Status footer
	(ui-label *status_label* (:text "Ready" :font *env_small_terminal_font* :flow_flags +flow_flag_align_hcenter)))

(defun update-ui-labels ()
	(when *weather_data*
		(bind '(area country desc code
			temp_c temp_f feels_c feels_f
			humidity wind_kmph wind_dir pressure vis_km
			forecast) *weather_data*)
		(defq temp_str (if *unit_c* (cat temp_c " C") (cat temp_f " F"))
			feels_str (if *unit_c* (cat feels_c " C") (cat feels_f " F")))
		(def *temp_label* :text temp_str)
		(def *condition_label* :text desc)
		(def *feels_label* :text (cat "Feels like " feels_str))
		(def *location_label* :text (cat area (if (> (length country) 0) (cat ", " country) "")))
		(def *humidity_label* :text (cat "Humidity: " humidity "%"))
		(def *wind_label* :text (cat "Wind: " wind_kmph " km/h " wind_dir))
		(def *pressure_label* :text (cat "Pressure: " pressure " hPa"))
		(def *vis_label* :text (cat "Visibility: " vis_km " km"))
		; Forecast cards
		(when (>= (length forecast) 3)
			(defq d1 (elem-get forecast 0)
				d2 (elem-get forecast 1)
				d3 (elem-get forecast 2))
			(bind '(dt1 mx1 mn1 mxf1 mnf1 desc1 &) d1)
			(bind '(dt2 mx2 mn2 mxf2 mnf2 desc2 &) d2)
			(bind '(dt3 mx3 mn3 mxf3 mnf3 desc3 &) d3)
			(def *fc_d1_date* :text (if (>= (length dt1) 5) (slice dt1 5 -1) dt1))
			(def *fc_d1_desc* :text desc1)
			(def *fc_d1_temp* :text (if *unit_c* (cat mx1 " / " mn1 " C") (cat mxf1 " / " mnf1 " F")))
			(def *fc_d2_date* :text (if (>= (length dt2) 5) (slice dt2 5 -1) dt2))
			(def *fc_d2_desc* :text desc2)
			(def *fc_d2_temp* :text (if *unit_c* (cat mx2 " / " mn2 " C") (cat mxf2 " / " mnf2 " F")))
			(def *fc_d3_date* :text (if (>= (length dt3) 5) (slice dt3 5 -1) dt3))
			(def *fc_d3_desc* :text desc3)
			(def *fc_d3_temp* :text (if *unit_c* (cat mx3 " / " mn3 " C") (cat mxf3 " / " mnf3 " F"))))
		(render-weather-icon *icon_canvas* code)
		(def *status_label* :text (cat "Updated for " area))
		(bind '(x y) (. *window* :get_pos))
		(bind '(w h) (. *window* :pref_size))
		(bind '(x y w h) (view-fit x y w h))
		(. *window* :change_dirty x y w h :t)))

(defun trigger-fetch (worker_mbox city)
	(def *status_label* :text (cat "Fetching weather for " city "..."))
	(.-> *status_label* :layout :dirty)
	(defq task_code (str `(progn
		(import "service/net/app.inc")
		(import "lib/net/http.inc")
		(import "lib/net/json.inc")
		(import "lib/net/url.inc")
		(ensure-net-service)
		(defq result :nil)
		(catch
			(progn
				(defq url (cat "http://wttr.in/" (url-encode ,city :t) "?format=j1")
					resp (http-get url))
				(when resp
					(defq body (http-body-str resp))
					(when (and body (starts-with "{" (trim body)))
						(defq json (json-parse body))
						(when json
							(defq cur (first (pfind json :current_condition))
								area (first (pfind json :nearest_area))
								area_name (trim (or (pfind (first (pfind area :areaName)) :value) ,city))
								country (trim (or (pfind (first (pfind area :country)) :value) ""))
								desc (trim (or (pfind (first (pfind cur :weatherDesc)) :value) "Unknown"))
								code (str-to-num (or (pfind cur :weatherCode) "113"))
								temp_c (or (pfind cur :temp_C) "0")
								temp_f (or (pfind cur :temp_F) "32")
								feels_c (or (pfind cur :FeelsLikeC) temp_c)
								feels_f (or (pfind cur :FeelsLikeF) temp_f)
								humidity (or (pfind cur :humidity) "0")
								wind_kmph (or (pfind cur :windspeedKmph) "0")
								wind_dir (trim (or (pfind cur :winddir16Point) ""))
								pressure (or (pfind cur :pressure) "1013")
								vis_km (or (pfind cur :visibility) "10")
								days (pfind json :weather)
								forecast_list (list))
							(each (lambda (day)
								(defq d_date (or (pfind day :date) "")
									max_c (or (pfind day :maxtempC) "0")
									min_c (or (pfind day :mintempC) "0")
									max_f (or (pfind day :maxtempF) "32")
									min_f (or (pfind day :mintempF) "32")
									mid_hour (elem-get (pfind day :hourly) 4)
									day_desc (trim (if mid_hour (or (pfind (first (pfind mid_hour :weatherDesc)) :value) "") ""))
									day_code (if mid_hour (str-to-num (or (pfind mid_hour :weatherCode) "113")) 113))
								(push forecast_list (list d_date max_c min_c max_f min_f day_desc day_code)))
								days)
							(setq result (list area_name country desc code
								temp_c temp_f feels_c feels_f
								humidity wind_kmph wind_dir pressure vis_km
								forecast_list))))))
			(progn (setq result :nil) :t))
		(mail-send (hex-decode ,(hex-encode worker_mbox)) (str result)))))
	(open-child task_code +kn_call_run))

(defun main ()
	(config-load)
	(defq select (task-mboxes +select_size) *running* :t
		refresh_interval (* 15 60 1000000)) ; 15 minutes
	(def *window* :tip_mbox (elem-get select +select_tip))
	(. *search_input* :set_text *city*)
	(def *btn_unit* :text (if *unit_c* "F" "C"))
	(defq city_btns (list *btn_city_0* *btn_city_1* *btn_city_2* *btn_city_3* *btn_city_4* *btn_city_5*))
	(each (lambda (i)
		(def (elem-get city_btns i) :text (elem-get *quick_cities* i)))
		(range 0 6))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (.-> *window* (:change x y w h :t) :dirty_all))
	(render-weather-icon *icon_canvas* 116)
	(trigger-fetch (elem-get select +select_worker) *city*)
	(mail-timeout (elem-get select +select_timer) refresh_interval 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_worker
				; Worker message received
				(if (and *msg* (defq parsed_data (first (read (string-stream *msg*)))))
					(progn
						(setq *weather_data* parsed_data)
						(update-ui-labels))
					(progn
						(def *status_label* :text (cat "Location not found: " *city*))
						(.-> *status_label* :layout :dirty))))
			(+select_timer
				; Re-arm 15m timer and trigger refresh
				(mail-timeout (elem-get select +select_timer) refresh_interval 0)
				(trigger-fetch (elem-get select +select_worker) *city*))
			(+select_tip
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			(+select_main
				(defq id (getf *msg* +ev_msg_target_id))
				(cond
					((= id +event_close)
						(setq *running* :nil))
					((= id +event_refresh)
						(setq *city* (trim (. *search_input* :get_text)))
						(trigger-fetch (elem-get select +select_worker) *city*)
						(config-save))
					((= id +event_unit_toggle)
						(setq *unit_c* (not *unit_c*))
						(def *btn_unit* :text (if *unit_c* "F" "C"))
						(.-> *btn_unit* :layout :dirty)
						(update-ui-labels)
						(config-save))
					((and (>= id +event_city_0) (<= id +event_city_5))
						(defq city_idx (- id +event_city_0)
							chosen_city (elem-get *quick_cities* city_idx))
						(when (and chosen_city (> (length chosen_city) 0))
							(setq *city* chosen_city)
							(.-> *search_input* (:set_text *city*) :layout :dirty)
							(trigger-fetch (elem-get select +select_worker) *city*)
							(config-save)))
					((. *window* :event *msg*))))))
	(config-save)
	(gui-sub-rpc *window*))

