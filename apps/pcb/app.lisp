;import settings
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

;math tools
(import 'apps/math.inc)

(structure 'event 0
	(byte 'win_close)
	(byte 'win_next)
	(byte 'win_prev)
	(byte 'win_scale_up)
	(byte 'win_scale_down))

(defq pcbs '(apps/pcb/test1.pcb apps/pcb/test2.pcb) index 0 id t canvas_scale 3
	max_zoom 15 min_zoom 5 scale (div (add min_zoom max_zoom) 2) stack (array) eps 0.025)

(ui-tree window (create-window window_flag_close) nil
	(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_down flow_flag_fillw flow_flag_lasth)
			'color argb_green)
		(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_right flow_flag_fillh)
				'font (create-font "fonts/Entypo.otf" 32))
			(button-connect-click (ui-element _ (create-button) ('text "")) event_win_prev)
			(button-connect-click (ui-element _ (create-button) ('text "")) event_win_next)
			(button-connect-click (ui-element _ (create-button) ('text "")) event_win_scale_down)
			(button-connect-click (ui-element _ (create-button) ('text "")) event_win_scale_up))
		(ui-element pcb_scroll (create-scroll (bit-or scroll_flag_vertical scroll_flag_horizontal))
			('min_width 512 'min_height 256))))

(defun trans (_)
	(add (bit-and 0xffffff _) 0xa0000000))

(defun pcb-load (path scale)
	(bind '(pcb _) (read (file-stream path) (ascii " ")))
	(bind '(pcb_width pcb_height pcb_depth) (elem 0 pcb))
	(defq canvas (create-canvas (mul pcb_width scale) (mul pcb_height scale) canvas_scale)
		scale (mul scale canvas_scale)
		colors (map trans (list argb_red argb_green argb_blue argb_yellow argb_cyan argb_magenta)))
	(canvas-fill (canvas-set-flags canvas 1) argb_black)
	(each (lambda ((id track_radius via_radius track_gap pads paths))
		(setq track_radius (mul scale track_radius) via_radius (mul scale via_radius)
			track_gap (mul scale track_gap))
		(when (ne track_radius 0)
			(defq vias (list))
			;draw path layers
			(each (lambda (path)
				(defq s 0 e 0 path_2d (apply points (reduce (lambda (l (x y _))
						(push l (mul scale x) (mul scale y))) path (list))))
				(while (le (setq e (inc e)) (length path))
					(when (or (eq e (length path)) (ne (defq z (elem 2 (elem s path))) (elem 2 (elem e path))))
						(when (gt (sub e s) 1)
							(canvas-set-color canvas (elem (mod (bit-shr z fp_shift) (length colors)) colors))
							(canvas-fpoly canvas 0.0 0.0 0
								(points-stroke-polylines stack track_radius eps join-round cap-round cap-round
									(list (slice (mul s 2) (mul e 2) path_2d)) (list))))
						(when (ne s 0)
							(push vias (list (elem (mul s 2) path_2d) (elem (inc (mul s 2)) path_2d))))
						(setq s e)))
				) paths)
			;draw vias
			(each (lambda ((x y))
				(canvas-set-color canvas (const (trans argb_white)))
				(canvas-fpoly canvas x y 0
					(list (points-gen-arc stack 0 0 0 fp_2pi via_radius eps (points))))
				(canvas-set-color canvas (const (trans argb_black)))
				(canvas-fpoly canvas x y 0
					(list (points-gen-arc stack 0 0 0 fp_2pi (div via_radius 2) eps (points))))
				) vias))
		;draw pads
		(each (lambda ((pad_radius pad_gap (pad_x pad_y pad_z) pad_shape))
			(setq pad_radius (mul scale pad_radius) pad_gap (mul scale pad_gap)
				pad_x (mul scale pad_x) pad_y (mul scale pad_y)
				pad_shape (apply points (reduce (lambda (l (x y))
					(push l (mul scale x) (mul scale y))) pad_shape (list))))
			(canvas-set-color canvas (const (trans argb_white)))
			(cond
				((eq (length pad_shape) 0)
					;circular pad
					(canvas-fpoly canvas pad_x pad_y 0
						(list (points-gen-arc stack 0 0 0 fp_2pi pad_radius eps (points)))))
				((eq (length pad_shape) 4)
					;oval pad
					(canvas-fpoly canvas pad_x pad_y 0
						(points-stroke-polylines stack pad_radius eps join-bevel cap-round cap-round
							(list pad_shape) (list))))
				(t
					;polygon pad
					(canvas-fpoly canvas pad_x pad_y 0 (list pad_shape))))
			) pads)
		) (slice 1 -1 pcb))
	canvas)

(defun win-refresh (_)
	(view-layout (view-add-child pcb_scroll (pcb-load (elem (setq index _) pcbs) scale)))
	(view-dirty-all (view-layout (window-set-title window (elem _ pcbs)))))

(window-connect-close window event_win_close)
(bind '(w h) (view-pref-size (win-refresh index)))
(gui-add (view-change window 64 256 w h))

(while id
	(cond
		((eq (setq id (get-long (defq msg (mail-mymail)) ev_msg_target_id)) event_win_close)
			(setq id nil))
		((eq id event_win_next)
			(win-refresh (mod (inc index) (length pcbs))))
		((eq id event_win_prev)
			(win-refresh (mod (add (dec index) (length pcbs)) (length pcbs))))
		((eq id event_win_scale_down)
			(setq scale (if (eq scale min_zoom) min_zoom (dec scale)))
			(win-refresh index))
		((eq id event_win_scale_up)
			(setq scale (if (eq scale max_zoom) max_zoom (inc scale)))
			(win-refresh index))
		(t (view-event window msg))))
