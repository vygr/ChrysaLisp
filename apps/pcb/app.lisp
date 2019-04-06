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

(defun circle (r)
	(defq k (sym-cat (str r)))
	(if (defq i (find k cache_key)) (elem i cache_poly)
		(progn
			(push cache_key k)
			(push cache_poly (list (points-gen-arc stack 0 0 0 fp_2pi r eps (points))))
			(elem -2 cache_poly))))

(defun oval (r s)
	(defq k (sym-cat (str r) ":" (str s)))
	(if (defq i (find k cache_key)) (elem i cache_poly)
		(progn
			(push cache_key k)
			(push cache_poly (points-stroke-polylines stack r eps
				join-bevel cap-round cap-round (list s) (list)))
			(elem -2 cache_poly))))

(defun batch (path)
	(defq s 0 e 0 b (list))
	(while (le (setq e (inc e)) (length path))
		(when (or (eq e (length path)) (ne (elem 2 (elem s path)) (elem 2 (elem e path))))
			(push b (slice s e path))
			(setq s e))) b)

(defun to_2d (_)
	(reduce (lambda (p _)
		(push p (mul scale (elem 0 _)) (mul scale (elem 1 _)))) _ (points)))

(defun batch_to_2d (_)
	(map to_2d _))

(defun pcb-load (_)
	(bind '(pcb _) (read (file-stream _) (ascii " ")))
	(bind '(pcb_width pcb_height pcb_depth) (elem 0 pcb))
	(defq canvas (create-canvas (mul pcb_width scale) (mul pcb_height scale) canvas_scale)
		scale (mul scale canvas_scale) cache_key (list) cache_poly (list)
		colors (map trans (list argb_red argb_green argb_blue argb_yellow argb_cyan argb_magenta)))
	(canvas-fill (canvas-set-flags canvas 1) argb_black)
	(each (lambda ((id track_radius via_radius track_gap pads paths))
		(setq track_radius (mul scale track_radius) via_radius (mul scale via_radius)
			track_gap (mul scale track_gap))
		(when (ne track_radius 0)
			;draw path layers
			(defq paths (map batch paths) paths_2d (map batch_to_2d paths))
			(each (lambda (path path_2d)
				(each (lambda (seg seg_2d)
					(when (gt (length seg) 1)
						(canvas-set-color canvas (elem (mod (bit-shr (elem 2 (elem 0 seg)) fp_shift) (length colors)) colors))
						(canvas-fpoly canvas 0.0 0.0 0
							(points-stroke-polylines stack track_radius eps join-round cap-round cap-round
								(list seg_2d) (list))))
					) path path_2d)
				) paths paths_2d)
			;draw vias
			(each (lambda (path_2d)
				(each! 1 nil nil (lambda (seg_2d)
					(bind '(x y) (slice 0 2 seg_2d))
					(canvas-set-color canvas (const (trans argb_white)))
					(canvas-fpoly canvas x y 0 (circle via_radius))
					(canvas-set-color canvas (const (trans argb_black)))
					(canvas-fpoly canvas x y 0 (circle (div via_radius 2)))
					) (list path_2d))
				) paths_2d)
		)
		;draw pads
		(each (lambda ((pad_radius pad_gap (pad_x pad_y pad_z) pad_shape))
			(setq pad_radius (mul scale pad_radius) pad_gap (mul scale pad_gap)
				pad_x (mul scale pad_x) pad_y (mul scale pad_y)
				pad_shape (to_2d pad_shape))
			(canvas-set-color canvas (const (trans argb_white)))
			(cond
				((eq (length pad_shape) 0)
					;circular pad
					(canvas-fpoly canvas pad_x pad_y 0 (circle pad_radius)))
				((eq (length pad_shape) 4)
					;oval pad
					(canvas-fpoly canvas pad_x pad_y 0 (oval pad_radius pad_shape)))
				(t
					;polygon pad
					(canvas-fpoly canvas pad_x pad_y 0 (list pad_shape))))
			) pads)
		) (slice 1 -1 pcb))
	canvas)

(defun win-refresh (_)
	(view-layout (view-add-child pcb_scroll (pcb-load (elem (setq index _) pcbs))))
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
