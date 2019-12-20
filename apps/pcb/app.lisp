;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/math.inc)

(structure 'event 0
	(byte 'win_close 'win_prev 'win_next)
	(byte 'win_scale_down 'win_scale_up)
	(byte 'win_mode_normal 'win_mode_gerber)
	(byte 'win_show_all 'win_show_1 'win_show_2 'win_show_3 'win_show_4))

(defq pcbs '("apps/pcb/test1.pcb" "apps/pcb/test2.pcb" "apps/pcb/test3.pcb")
	index 1 id t canvas_scale 1 mode 0 show -1 max_zoom 15 min_zoom 5
	zoom (/ (+ min_zoom max_zoom) 2) eps 0.25)

(ui-tree window (create-window window_flag_close) nil
	(ui-element _ (create-flow) ('flow_flags (logior flow_flag_down flow_flag_fillw flow_flag_lasth)
			'color slider_col)
		(ui-element _ (create-flow) ('flow_flags (logior flow_flag_right flow_flag_fillh))
			(each (lambda (l)
				(component-connect (ui-element __ (create-button)
					('text l 'color toolbar_col 'font (create-font "fonts/Entypo.otf" 32))) (+ event_win_prev _)))
						'("" "" "" "" "" ""))
			(each (lambda (l)
				(component-connect (ui-element __ (create-button)
					('text l 'color toolbar2_col 'font (create-font "fonts/OpenSans-Regular.ttf" 24))) (+ event_win_show_all _)))
						'("0" "1" "2" "3" "4")))
		(ui-element pcb_scroll (create-scroll (logior scroll_flag_vertical scroll_flag_horizontal))
			('min_width 512 'min_height 256))))

(defun-bind trans (_)
	(+ (logand 0xffffff _) 0xa0000000))

(defun-bind circle (r)
	(if (defq i (find (defq k (sym (str r))) cache_key))
		(elem i cache_poly)
		(progn
			(push cache_key k)
			(elem -2 (push cache_poly (list (points-gen-arc 0 0 0 fp_2pi r eps (points))))))))

(defun-bind oval (r s)
	(if (defq i (find (defq k (sym (str r ":" s))) cache_key))
		(elem i cache_poly)
		(progn
			(push cache_key k)
			(elem -2 (push cache_poly (points-stroke-polylines r eps
				join_bevel cap_round cap_round (list s) (list)))))))

(defun-bind batch (path)
	(defq s 0 e 0 b (list))
	(while (<= (setq e (inc e)) (length path))
		(when (or (= e (length path)) (/= (elem 2 (elem s path)) (elem 2 (elem e path))))
			(push b (slice s e path))
			(setq s e))) b)

(defun-bind to-2d (_)
	(reduce (lambda (p _)
		(push p (* zoom (elem 0 _)) (* zoom (elem 1 _)))) _ (points)))

(defun-bind batch-to-2d (_)
	(map to-2d _))

(defun-bind pcb-load (_)
	(bind '(pcb _) (read (string-stream (cat "(" (load _) ")")) (const (ascii-code " "))))
	(bind '(pcb_width pcb_height pcb_depth) (elem 0 pcb))
	(defq canvas (create-canvas (* (+ pcb_width 4) zoom) (* (+ pcb_height 4) zoom) canvas_scale)
		zoom (* zoom canvas_scale) pcb_border (* 2.0 zoom) cache_key (list) cache_poly (list))
	(canvas-fill (canvas-set-flags canvas 1) (const argb_black))
	(if (= mode 1)
		(pcb-draw-gerber)
		(pcb-draw-normal))
	(canvas-swap canvas))

(defun-bind pcb-draw-normal ()
	(defq colors (map trans (list argb_red argb_green argb_blue argb_yellow argb_cyan argb_magenta)))
	(each! 1 -2 (lambda ((id track_radius via_radius track_gap pads paths))
		(setq track_radius (* zoom track_radius) via_radius (* zoom via_radius)
			track_gap (* zoom track_gap))
		(when (/= track_radius 0)
			;draw layers
			(defq batched_paths (map batch paths) batched_paths_2d (map batch-to-2d batched_paths)
				layers (list (list) (list) (list) (list) (list) (list)))
			(each (lambda (path path_2d)
				(each (lambda (seg seg_2d)
					(when (or (= show (defq z (% (>> (elem 2 (elem 0 seg)) fp_shift) pcb_depth)))
								(= show -1))
						(points-stroke-polylines track_radius eps join_round cap_round cap_round
							(list seg_2d) (elem z layers)))
					) path path_2d)
				) batched_paths batched_paths_2d)
			(each! 0 pcb_depth (lambda (layer color)
				(canvas-set-color canvas color)
				(canvas-fpoly canvas pcb_border pcb_border 1 layer)
				) (list layers colors))
			;draw vias
			(each (lambda (path_2d)
				(each! 1 -1 (lambda (seg_2d)
					(bind '(x y) (slice 0 2 seg_2d))
					(setq x (+ x pcb_border) y (+ y pcb_border))
					(canvas-set-color canvas (const (trans argb_white)))
					(canvas-fpoly canvas x y 0 (circle via_radius))
					(canvas-set-color canvas (const (trans argb_black)))
					(canvas-fpoly canvas x y 0 (circle (/ via_radius 2)))
					) (list path_2d))
				) batched_paths_2d))
		;draw pads
		(each (lambda ((pad_radius pad_gap (pad_x pad_y pad_z) pad_shape))
			(when (or (= show -1) (= show (>> pad_z fp_shift)))
				(setq pad_radius (* zoom pad_radius) pad_gap (* zoom pad_gap)
					pad_x (+ (* zoom pad_x) pcb_border) pad_y (+ (* zoom pad_y) pcb_border)
					pad_shape (to-2d pad_shape))
				(canvas-set-color canvas (const (trans argb_white)))
				(cond
					((= (length pad_shape) 0)
						;circular pad
						(canvas-fpoly canvas pad_x pad_y 0 (circle pad_radius)))
					((= (length pad_shape) 4)
						;oval pad
						(canvas-fpoly canvas pad_x pad_y 0 (oval pad_radius pad_shape)))
					(t
						;polygon pad
						(canvas-fpoly canvas pad_x pad_y 0 (list pad_shape)))))
			) pads)
		) (list pcb)))

(defun-bind pcb-draw-gerber ()
	;first draw in white with gaps
	(canvas-set-color canvas (const argb_white))
	(pcb-draw-layer t)
	;second draw in black without gaps
	(canvas-set-color canvas (const argb_black))
	(pcb-draw-layer nil))

(defun-bind pcb-draw-layer (with_gaps)
	(each! 1 -2 (lambda ((id track_radius via_radius track_gap pads paths))
		(defq track_radius (* zoom track_radius) via_radius (* zoom via_radius)
			track_gap (* zoom track_gap))
		(when (/= track_radius 0)
			;draw layers
			(defq batched_paths (map batch paths) batched_paths_2d (map batch-to-2d batched_paths) layer (list))
			(each (lambda (path path_2d)
				(each (lambda (seg seg_2d)
					(when (= show (defq z (% (>> (elem 2 (elem 0 seg)) fp_shift) pcb_depth)))
						(points-stroke-polylines (+ track_radius (if with_gaps track_gap 0)) eps join_round cap_round cap_round
							(list seg_2d) layer))
					) path path_2d)
				) batched_paths batched_paths_2d)
			(canvas-fpoly canvas pcb_border pcb_border 1 layer)
			;draw vias
			(each (lambda (path_2d)
				(each! 1 -1 (lambda (seg_2d)
					(bind '(x y) (slice 0 2 seg_2d))
					(setq x (+ x pcb_border) y (+ y pcb_border))
					(canvas-fpoly canvas x y 0 (circle (+ via_radius (if with_gaps track_gap 0))))
					) (list path_2d))
				) batched_paths_2d))
		;draw pads
		(each (lambda ((pad_radius pad_gap (pad_x pad_y pad_z) pad_shape))
			(when (= show (>> pad_z fp_shift))
				(setq pad_radius (* zoom pad_radius) pad_gap (* zoom pad_gap)
					pad_x (+ (* zoom pad_x) pcb_border) pad_y (+ (* zoom pad_y) pcb_border)
					pad_shape (to-2d pad_shape))
				(cond
					((= (length pad_shape) 0)
						;circular pad
						(canvas-fpoly canvas pad_x pad_y 0 (circle (+ pad_radius (if with_gaps pad_gap 0)))))
					((= (length pad_shape) 4)
						;oval pad
						(canvas-fpoly canvas pad_x pad_y 0 (oval (+ pad_radius (if with_gaps pad_gap 0)) pad_shape)))
					(t
						;polygon pad
						(if with_gaps
							(canvas-fpoly canvas pad_x pad_y 0
								(points-stroke-polygons pad_gap eps join_round (list pad_shape) (list)))
							(canvas-fpoly canvas pad_x pad_y 0
								(list pad_shape))))))
			) pads)
		) (list pcb)))

(defun-bind win-refresh (_)
	(view-layout (view-add-child pcb_scroll (pcb-load (elem (setq index _) pcbs))))
	(view-dirty-all (view-layout (window-set-title window (elem _ pcbs)))))

(gui-add (apply view-change (cat (list window 64 256)
	(view-pref-size (window-connect-close (win-refresh index) event_win_close)))))

(while id
	(cond
		((= (setq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) event_win_close)
			(setq id nil))
		((= id event_win_next)
			(win-refresh (% (inc index) (length pcbs))))
		((= id event_win_prev)
			(win-refresh (% (+ (dec index) (length pcbs)) (length pcbs))))
		((= id event_win_scale_down)
			(setq zoom (max min_zoom (dec zoom)))
			(win-refresh index))
		((= id event_win_scale_up)
			(setq zoom (min max_zoom (inc zoom)))
			(win-refresh index))
		((<= event_win_show_all id event_win_show_4)
			(setq show (- id event_win_show_all 1))
			(win-refresh index))
		((<= event_win_mode_normal id event_win_mode_gerber)
			(setq mode (- id event_win_mode_normal))
			(win-refresh index))
		(t (view-event window msg))))

(view-hide window)
