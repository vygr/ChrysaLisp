;debug options
(case :nil
	(0 (import "lib/debug/frames.inc")))

(import "usr/env.inc")
(import "gui/lisp.inc")
(import "service/lock/app.inc")

(enums +event +event_user
	(enum refresh select_coin)
	(enum coin_0 coin_1 coin_2 coin_3 coin_4 coin_5))

(enums +select 0
	(enum main tip timer worker trash))

(defq *config* :nil +config_version 1
	+config_file (cat (get :home *env*) "crypto.tre")
	*selected_symbol* "BTC" *coins_data* (list) *site* :nil
	*canvas_width* 360 *canvas_height* 110
	*btn_coin_0* :nil *btn_coin_1* :nil *btn_coin_2* :nil
	*btn_coin_3* :nil *btn_coin_4* :nil *btn_coin_5* :nil)

(defun config-default ()
	(scatter (Emap) :version +config_version :selected_symbol "BTC"))

(defun config-load ()
	(setq *config* (with-read-lock +config_file
			(tree-load (file-stream +config_file))))
	(if (or (not *config*) (nql (. *config* :find :version) +config_version))
		(setq *config* (config-default)))
	(setq *selected_symbol* (. *config* :find :selected_symbol)))

(defun config-save ()
	(scatter *config* :selected_symbol *selected_symbol*)
	(with-write-lock +config_file
		(tree-save (file-stream +config_file +file_open_write) *config*)))

(defun format-places (val places)
	; (format-places val places) -> str
	;a fixed to 2 or 4 places, rounded: a fixed is a little under what it
	;was written as, 0.15 is 0.14999, and cut it would say 0.14
	(defq s (str (+ (abs val) (if (= places 2) 0.005 0.00005))))
	(cat (if (< val 0.0) "-" "") (slice s 0 (+ (find "." s) 1 places))))

(defun format-price (price)
	(cat "$" (format-places price (if (< price 1.0) 4 2))))

(defun format-change (change)
	(cat (if (< change 0.0) "" "+") (format-places change 2) "%"))

(defun render-sparkline (canvas sparkline_pts change)
	(.-> canvas (:fill +argb_black) (:set_canvas_flags +canvas_flag_antialias))
	(defq w (n2q *canvas_width*) h (n2q *canvas_height*)
		margin_x 18.0 margin_y 14.0
		num_pts (if sparkline_pts (length sparkline_pts) 0))
	(if (< num_pts 2)
		(progn
			(defq base_p (path margin_x (* h 0.5) (- w margin_x) (* h 0.5))
				base_stroke (path-stroke-polyline base_p 1.5 +join_miter +cap_round +cap_round))
			(.-> canvas (:set_color 0x33ffffff) (:fpoly 0.0 0.0 +winding_none_zero (list base_stroke)))
			(. canvas :swap +swap_write))
		(defq flt_pts sparkline_pts)
		(if (< (length flt_pts) 2) (. canvas :swap +swap_write)
			(defq min_val (first flt_pts) max_val (first flt_pts))
			(each (# (setq min_val (min min_val %0)
						max_val (max max_val %0)))
				flt_pts)
			(defq val_range (- max_val min_val))
			(if (<= val_range 0.0) (setq val_range 1.0))
			(defq chart_w (- w (* margin_x 2.0))
				chart_h (- h (* margin_y 2.0))
				n_pts (length flt_pts)
				step_x (/ chart_w (n2q (- n_pts 1)))
				raw_path (path))
			(each! (# (defq val %0
						norm_y (/ (- val min_val) val_range)
						px (+ margin_x (* (n2q (!)) step_x))
						py (- (- h margin_y) (* norm_y chart_h)))
					(push raw_path px py)) (list flt_pts) 0 n_pts)
			; Smooth curve through points
			(defq smooth_line (path-smooth raw_path)
				is_positive (>= change 0.0)
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
			(. canvas :swap +swap_write))))

(ui-window *window* (:resizable :nil)
	(ui-title-bar _ "Crypto Ticker" (+sym_close) +event_close)
	; Controls Bar: Status & Refresh
	(ui-flow *ctrl_bar* (:flow_flags +flow_right_fill)
		(. (ui-button *btn_refresh* (:text "Refresh")) :connect +event_refresh)
		(ui-label *status_label* (:text "Connecting..." :flow_flags +flow_flag_align_vcenter :paper +argb_grey15)))
	; Hero Card: Coin Header
	(ui-flow *hero_head* (:flow_flags +flow_right_fill)
		(ui-label *hero_symbol* (:text "#1  BTC" :font :font_sub_title))
		(ui-label *hero_name* (:text "Bitcoin" :font :font_bold :flow_flags +flow_flag_align_vcenter))
		(ui-label *hero_change* (:text "+0.00%" :font :font_bold :border 0
				:flow_flags (logior +flow_flag_align_hright +flow_flag_align_vcenter))))
	; Hero Card: Big Price & 24h Range
	(ui-flow *hero_body* (:flow_flags +flow_right_fill)
		(ui-label *hero_price* (:text "$0.00" :font :font_display))
		(ui-label *range_label* (:text "24h: $0.00 - $0.00" :font :font_small
				:flow_flags (logior +flow_flag_align_hright +flow_flag_align_vcenter))))
	; Vector Sparkline Chart Canvas
	(ui-canvas *chart_canvas* *canvas_width* *canvas_height* 1)
	; Quick Asset Selection Grid (2 rows x 3 columns)
	(ui-grid _ (:grid_width 3 :font :font_small :paper :paper_toolbar2)
		(. (ui-button *btn_coin_0* (:text "BTC $0.00")) :connect +event_coin_0)
		(. (ui-button *btn_coin_1* (:text "ETH $0.00")) :connect +event_coin_1)
		(. (ui-button *btn_coin_2* (:text "SOL $0.00")) :connect +event_coin_2)
		(. (ui-button *btn_coin_3* (:text "BNB $0.00")) :connect +event_coin_3)
		(. (ui-button *btn_coin_4* (:text "XRP $0.00")) :connect +event_coin_4)
		(. (ui-button *btn_coin_5* (:text "DOGE $0.00")) :connect +event_coin_5))
	; Footer Attribution
	(ui-label *footer_label* (:text "Live data (24h trend)" :font :font_small
			:flow_flags +flow_flag_align_hcenter)))

(defun update-ui ()
	(when (> (length *coins_data*) 0)
		(defq selected_coin (some (# (if (eql (first %0) *selected_symbol*) %0)) *coins_data*))
		(unless selected_coin
			(setq selected_coin (first *coins_data*)
				*selected_symbol* (first selected_coin)))
		(bind '(coin_sym name price change rank spark) selected_coin)
		(. *hero_symbol* :set_text (cat "#" (str rank) "  " coin_sym))
		(. *hero_name* :set_text (if (eql coin_sym name) "" name))
		(defq chg_str (format-change change)
			is_pos (>= change 0.0))
		(def (. *hero_change* :dirty) :paper (if is_pos 0xff00e676 0xffff3344)
			:ink (if is_pos +argb_black +argb_white))
		(. *hero_change* :set_text chg_str)
		(. *hero_price* :set_text (format-price price))
		(if (< (length spark) 2) (. *range_label* :set_text "")
			(defq mn (first spark) mx (first spark))
			(each (# (setq mn (min mn %0)
						mx (max mx %0)))
				spark)
			(. *range_label* :set_text (cat "24h: " (format-price mn) " - " (format-price mx))))
		(. *footer_label* :set_text (cat "Live data from " *site* " (24h trend)"))
		;the two rows of the card laid out again for what they now say, the
		;window as it is
		(each (# (.-> %0 (:constrain :t) :dirty_all)) (list *hero_head* *hero_body*))
		(render-sparkline *chart_canvas* spark change)
		(defq btns (list
				*btn_coin_0* *btn_coin_1* *btn_coin_2*
				*btn_coin_3* *btn_coin_4* *btn_coin_5*))
		(each! (# (when (< (!) (length *coins_data*))
					(defq c %0
						csym (elem-get c 0)
						cprice (elem-get c 2))
					(. %1 :set_text (cat csym " " (format-price cprice))))) (list *coins_data* btns) 0 (min 6 (length *coins_data*)))
		(. *status_label* :set_text (cat "Tracking " (str (length *coins_data*)) " assets"))))

(defun trigger-fetch (worker_mbox trash_mbox)
	(. *status_label* :set_text "Updating prices...")
	;the sites are asked by a task of their own, apps/desktop/crypto/fetch.inc,
	;which says which answered and what it gave, or :nil
	(defq task_code (str `(progn
				(import "apps/desktop/crypto/fetch.inc")
				(mail-send (hex-decode ,(hex-encode worker_mbox)) (str (crypto-fetch))))))
	(open-task task_code (task-nodeid) +kn_call_run 0 trash_mbox))

(defun main ()
	(config-load)
	(defq select (task-mboxes +select_size) *running* :t
		refresh_interval (* 60 1000000)) ; 60 seconds
	(def *window* :tip_mbox (elem-get select +select_tip))
	(ui-tool-tips *ctrl_bar* '("refresh market data" ""))
	(. *window* :open)
	(render-sparkline *chart_canvas* :nil 0.0)
	(trigger-fetch (elem-get select +select_worker) (elem-get select +select_trash))
	(mail-timeout (elem-get select +select_timer) refresh_interval 0)
	(while *running*
		(defq *msg* (mail-read (elem-get select (defq idx (mail-select select)))))
		(case idx
			(+select_worker
				(if (and *msg*
						(defq parsed_data (first (read (string-stream *msg*)))))
					(progn
						(setq *site* (first parsed_data) *coins_data* (second parsed_data))
						(update-ui))
					(. *status_label* :set_text "No site gave prices")))
			(+select_timer
				(mail-timeout (elem-get select +select_timer) refresh_interval 0)
				(trigger-fetch (elem-get select +select_worker) (elem-get select +select_trash)))
			(+select_tip
				(. *window* :tip *msg*))
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
	(. *window* :close))
