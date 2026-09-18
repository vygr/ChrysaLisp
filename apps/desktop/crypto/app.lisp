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

(enums +event 0
	(enum close refresh select_coin)
	(enum coin_0 coin_1 coin_2 coin_3 coin_4 coin_5))

(enums +select 0
	(enum main tip timer worker trash))

(defq *config* :nil *config_version* 1
	*config_file* (cat *env_home* "crypto.tre")
	*selected_symbol* "BTC" *coins_data* (list)
	*canvas_width* 360 *canvas_height* 110
	*btn_coin_0* :nil *btn_coin_1* :nil *btn_coin_2* :nil
	*btn_coin_3* :nil *btn_coin_4* :nil *btn_coin_5* :nil)

(defun config-default ()
	(scatter (Emap)
		:version *config_version*
		:selected_symbol "BTC"))

(defun config-load ()
	(defq old_config :nil)
	(if (defq stream (file-stream *config_file*))
		(setq old_config (tree-load stream)))
	(if (or (not old_config) (/= (. old_config :find :version) *config_version*))
		(setq *config* (config-default))
		(setq *config* old_config))
	(setq *selected_symbol* (. *config* :find :selected_symbol)))

(defun config-save ()
	(ifn *config* (setq *config* (Emap)))
	(scatter *config*
		:version *config_version*
		:selected_symbol *selected_symbol*)
	(when (defq stream (file-stream *config_file* +file_open_write))
		(tree-save stream *config*)))

(defun format-price (price_str)
	(ifn (str? price_str) "$0.00"
		(ifn (defq dot (find "." price_str)) (cat "$" price_str ".00")
			(defq int_part (slice price_str 0 dot)
				frac_part (slice price_str (+ dot 1) -1))
			(if (eql int_part "0")
				(cat "$0." (slice (cat frac_part "0000") 0 4))
				(cat "$" int_part "." (slice (cat frac_part "00") 0 2))))))

(defun format-change (chg_str)
	(ifn (str? chg_str) "+0.00%"
		(if (starts-with "-" chg_str) (cat chg_str "%")
			(cat "+" chg_str "%"))))

(defun render-sparkline (canvas sparkline_pts change_str)
	(.-> canvas (:fill +argb_black) (:set_canvas_flags +canvas_flag_antialias))
	(defq w (n2f *canvas_width*) h (n2f *canvas_height*)
		margin_x 18.0 margin_y 14.0
		num_pts (if sparkline_pts (length sparkline_pts) 0))
	(if (< num_pts 2)
		(progn
			(defq base_p (path margin_x (* h 0.5) (- w margin_x) (* h 0.5))
				base_stroke (path-stroke-polyline base_p 1.5 +join_miter +cap_round +cap_round))
			(.-> canvas (:set_color 0x33ffffff) (:fpoly 0.0 0.0 +winding_none_zero (list base_stroke)))
			(. canvas :swap 0))
		(defq flt_pts (list))
		(each (# (when (and %0 (str? %0)) (push flt_pts (str-to-num %0)))) sparkline_pts)
		(if (< (length flt_pts) 2) (. canvas :swap 0)
			(defq min_val (first flt_pts) max_val (first flt_pts))
			(each (# (setq min_val (min min_val %0)
					max_val (max max_val %0)))
				flt_pts)
			(defq val_range (- max_val min_val))
			(if (<= val_range 0.000001) (setq val_range 1.0))
			(defq chart_w (- w (* margin_x 2.0))
				chart_h (- h (* margin_y 2.0))
				n_pts (length flt_pts)
				step_x (/ chart_w (n2f (- n_pts 1)))
				raw_path (path))
			(each (#
				(defq val (elem-get flt_pts %0)
					norm_y (/ (- val min_val) val_range)
					px (+ margin_x (* (n2f %0) step_x))
					py (- (- h margin_y) (* norm_y chart_h)))
				(push raw_path px py))
				(range 0 n_pts))
			; Smooth curve through points
			(defq smooth_line (path-smooth raw_path)
				is_positive (not (starts-with "-" change_str))
				line_col (if is_positive 0xff00e676 0xffff3344)
				bg_col (if is_positive 0x3000e676 0x30ff3344)
				stroke (path-stroke-polyline smooth_line 2.2 +join_round +cap_round +cap_round))
			; Subtle grid lines (top, center, bottom)
			(.-> canvas
				(:set_color 0x22ffffff)
				(:fpoly 0.0 0.0 +winding_none_zero
					(map (#
						(defq gy (+ margin_y (* %0 chart_h)))
						(path-stroke-polyline (path margin_x gy (- w margin_x) gy) 1.0 +join_miter +cap_square +cap_square))
						'(0.0 0.5 1.0))))
					; Area fill under the smoothed line
					(defq first_x (elem-get smooth_line 0)
						last_x (elem-get smooth_line -3)
						bottom_y (- h margin_y)
						area_poly (path first_x bottom_y))
					(each (# (push area_poly %0)) smooth_line)
					(push area_poly last_x bottom_y)
					(.-> canvas
						(:set_color bg_col)
						(:fpoly 0.0 0.0 +winding_none_zero (list area_poly)))
					; Curve stroke
					(.-> canvas
						(:set_color line_col)
						(:fpoly 0.0 0.0 +winding_none_zero (list stroke)))
					; Glowing terminal marker on the latest price (right edge)
					(defq last_px (elem-get smooth_line -3)
						last_py (elem-get smooth_line -2)
						dot (path-gen-arc last_px last_py 0.0 +fp_2pi 3.5 (path))
						dot_ring (path-stroke-polyline (path-gen-arc last_px last_py 0.0 +fp_2pi 6.0 (path)) 1.2 +join_miter +cap_round +cap_round))
					(.-> canvas
						(:set_color line_col)
						(:fpoly 0.0 0.0 +winding_none_zero (list dot)))
					(.-> canvas
						(:set_color 0x99ffffff)
						(:fpoly 0.0 0.0 +winding_none_zero (list dot_ring)))
					(. canvas :swap 0))))

(ui-window *window* ()
	(ui-title-bar _ "Crypto Ticker" (0xea19) +event_close)
	; Controls Bar: Status & Refresh
	(ui-flow _ (:flow_flags +flow_right_fill)
		(. (ui-button *btn_refresh* (:text "Refresh")) :connect +event_refresh)
		(ui-label *status_label* (:text "Connecting..." :flow_flags +flow_flag_align_vcenter :color +argb_grey15)))
	; Hero Card: Coin Header
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-label *hero_symbol* (:text "#1  BTC" :font *env_sub_title_font*))
		(ui-label *hero_name* (:text "Bitcoin" :font *env_bold_font* :flow_flags +flow_flag_align_vcenter))
		(ui-label *hero_change* (:text "+0.00%" :font *env_bold_font* :border 0
			:flow_flags (logior +flow_flag_align_hright +flow_flag_align_vcenter))))
	; Hero Card: Big Price & 24h Range
	(ui-flow _ (:flow_flags +flow_right_fill)
		(ui-label *hero_price* (:text "$0.00" :font *env_display_font*))
		(ui-label *range_label* (:text "24h: $0.00 - $0.00" :font *env_small_font*
			:flow_flags (logior +flow_flag_align_hright +flow_flag_align_vcenter))))
	; Vector Sparkline Chart Canvas
	(ui-canvas *chart_canvas* *canvas_width* *canvas_height* 1)
	; Quick Asset Selection Grid (2 rows x 3 columns)
	(ui-grid _ (:grid_width 3 :font *env_small_font* :color *env_toolbar2_col*)
		(. (ui-button *btn_coin_0* (:text "BTC $0.00")) :connect +event_coin_0)
		(. (ui-button *btn_coin_1* (:text "ETH $0.00")) :connect +event_coin_1)
		(. (ui-button *btn_coin_2* (:text "SOL $0.00")) :connect +event_coin_2)
		(. (ui-button *btn_coin_3* (:text "BNB $0.00")) :connect +event_coin_3)
		(. (ui-button *btn_coin_4* (:text "XRP $0.00")) :connect +event_coin_4)
		(. (ui-button *btn_coin_5* (:text "DOGE $0.00")) :connect +event_coin_5))
	; Footer Attribution
	(ui-label *footer_label* (:text "Live data from Coinranking (24h trend)" :font *env_small_font*
		:flow_flags +flow_flag_align_hcenter)))

(defun update-ui ()
	(when (> (length *coins_data*) 0)
		(defq selected_coin (some (# (if (eql (first %0) *selected_symbol*) %0)) *coins_data*))
		(if (not selected_coin)
			(setq selected_coin (first *coins_data*)
				*selected_symbol* (first selected_coin)))
		(bind '(coin_sym name price change rank spark) selected_coin)
		(def *hero_symbol* :text (cat "#" (str rank) "  " coin_sym))
		(def *hero_name* :text (if (eql coin_sym name) "" name))
		(defq chg_str (format-change change)
			is_pos (not (starts-with "-" change)))
		(def *hero_change* :text chg_str
			:color (if is_pos 0xff00e676 0xffff3344)
			:ink_color (if is_pos +argb_black +argb_white))
		(def *hero_price* :text (format-price price))
		(defq flts (list))
		(each (# (when (and %0 (str? %0)) (push flts (str-to-num %0)))) spark)
		(if (< (length flts) 2) (def *range_label* :text "")
			(defq mn (first flts) mx (first flts))
			(each (# (setq mn (min mn %0)
					mx (max mx %0)))
				flts)
			(def *range_label* :text (cat "24h: " (format-price (str mn)) " - " (format-price (str mx)))))
		(render-sparkline *chart_canvas* spark change)
		(defq btns (list
			*btn_coin_0* *btn_coin_1* *btn_coin_2*
			*btn_coin_3* *btn_coin_4* *btn_coin_5*))
		(each (#
			(when (< %0 (length *coins_data*))
				(defq c (elem-get *coins_data* %0)
					csym (elem-get c 0)
					cprice (elem-get c 2))
				(def (elem-get btns %0) :text (cat csym " " (format-price cprice)))))
			(range 0 (min 6 (length *coins_data*))))
		(def *status_label* :text (cat "Tracking " (str (length *coins_data*)) " assets"))
		(bind '(x y) (. *window* :get_pos))
		(bind '(w h) (. *window* :pref_size))
		(bind '(x y w h) (view-fit x y w h))
		(. *window* :change_dirty x y w h :t)))

(defun trigger-fetch (worker_mbox trash_mbox)
	(def *status_label* :text "Updating prices...")
	(.-> *status_label* :layout :dirty)
	(defq task_code (str `(progn
		(import "service/net/app.inc")
		(import "lib/net/http.inc")
		(import "lib/net/json.inc")
		(ensure-net-service)
		(defq result :nil)
		(catch
			(progn
				(defq api_url "http://api.coinranking.com/v2/coins?limit=6")
				(when (defq resp (http-get api_url))
					(defq body (http-body-str resp))
					(when (and body (starts-with "{" (trim body)))
						(when (defq json (json-parse body))
							(defq coin_list (map (#
								(list (or (pfind %0 :symbol) "")
									(or (pfind %0 :name) "")
									(or (pfind %0 :price) "0")
									(or (pfind %0 :change) "0")
									(or (pfind %0 :rank) 0)
									(or (pfind %0 :sparkline) (list))))
								(pfind (pfind json :data) :coins)))
							(setq result coin_list)))))
			(progn (setq result :nil) :t))
		(mail-send (hex-decode ,(hex-encode worker_mbox)) (str result)))))
	(open-task task_code (slice (task-mbox) +long_size -1) +kn_call_run 0 trash_mbox))

(defun main ()
	(config-load)
	(defq select (task-mboxes +select_size) *running* :t
		refresh_interval (* 60 1000000)) ; 60 seconds
	(def *window* :tip_mbox (elem-get select +select_tip))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (.-> *window* (:change x y w h :t) :dirty_all))
	(render-sparkline *chart_canvas* :nil "0")
	(trigger-fetch (elem-get select +select_worker) (elem-get select +select_trash))
	(mail-timeout (elem-get select +select_timer) refresh_interval 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_worker
				(if (and *msg*
						(defq parsed_data (first (read (string-stream *msg*)))))
					(progn
						(setq *coins_data* parsed_data)
						(update-ui))
					(def *status_label* :text "Failed to update prices")
					(.-> *status_label* :layout :dirty)))
			(+select_timer
				(mail-timeout (elem-get select +select_timer) refresh_interval 0)
				(trigger-fetch (elem-get select +select_worker) (elem-get select +select_trash)))
			(+select_tip
				(if (defq view (. *window* :find_id (getf *msg* +mail_timeout_id)))
					(. view :show_tip)))
			(+select_main
				(defq id (getf *msg* +ev_msg_target_id))
				(cond
					((= id +event_close)
						(setq *running* :nil))
					((= id +event_refresh)
						(trigger-fetch (elem-get select +select_worker) (elem-get select +select_trash)))
					((<= +event_coin_0 id (const (inc +event_coin_5)))
						(defq coin_idx (- id +event_coin_0))
						(when (< coin_idx (length *coins_data*))
							(defq chosen (elem-get *coins_data* coin_idx))
							(setq *selected_symbol* (first chosen))
							(update-ui)
							(config-save)))
					((. *window* :event *msg*))))))
	(config-save)
	(gui-sub-rpc *window*))

