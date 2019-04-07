;import settings
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)

;math tools
(import 'apps/math.inc)

(structure 'event 0
	(byte 'win_close)
	(byte 'win_next)
	(byte 'win_prev)
	(byte 'win_scale_down)
	(byte 'win_scale_up)
	(byte 'win_mode_normal)
	(byte 'win_mode_gerber)
	(byte 'win_show_all)
	(byte 'win_show_1)
	(byte 'win_show_2)
	(byte 'win_show_3)
	(byte 'win_show_4)
	(byte 'win_show_5)
	(byte 'win_show_6))

(defq pcbs '(apps/pcb/test1.pcb apps/pcb/test2.pcb) index 0 id t canvas_scale 3
	mode 0 show -1 max_zoom 15 min_zoom 5 zoom (div (add min_zoom max_zoom) 2)
	stack (array) eps 0.025)

(ui-tree window (create-window window_flag_close) nil
	(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_down flow_flag_fillw flow_flag_lasth)
			'color argb_green)
		(ui-element _ (create-flow) ('flow_flags (bit-or flow_flag_right flow_flag_fillh))
			(each (lambda (l)
				(button-connect-click (ui-element __ (create-button)
					('text l 'color argb_green 'font (create-font "fonts/Entypo.otf" 32))) (add event_win_next _)))
						'("" "" "" "" "" ""))
			(each (lambda (l)
				(button-connect-click (ui-element __ (create-button)
					('text l 'color argb_cyan 'font (create-font "fonts/OpenSans-Regular.ttf" 24))) (add event_win_show_all _)))
						'("0" "1" "2" "3" "4" "5" "6")))
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

(defun to-2d (_)
	(reduce (lambda (p _)
		(push p (mul zoom (elem 0 _)) (mul zoom (elem 1 _)))) _ (points)))

(defun batch-to-2d (_)
	(map to-2d _))

(defun pcb-load (_)
	(bind '(pcb _) (read (file-stream _) (ascii " ")))
	(bind '(pcb_width pcb_height pcb_depth) (elem 0 pcb))
	(defq canvas (create-canvas (mul pcb_width zoom) (mul pcb_height zoom) canvas_scale)
		zoom (mul zoom canvas_scale) cache_key (list) cache_poly (list))
	(canvas-fill (canvas-set-flags canvas 1) (const argb_black))
	(if (eq mode 1)
		(pcb-draw-gerber)
		(pcb-draw-normal))
	canvas)

(defun pcb-draw-normal ()
	(defq colors (map trans (list argb_red argb_green argb_blue argb_yellow argb_cyan argb_magenta)))
	(each (lambda ((id track_radius via_radius track_gap pads paths))
		(setq track_radius (mul zoom track_radius) via_radius (mul zoom via_radius)
			track_gap (mul zoom track_gap))
		(when (ne track_radius 0)
			;draw layers
			(defq batched_paths (map batch paths) batched_paths_2d (map batch-to-2d batched_paths)
				layers (list (list) (list) (list) (list) (list) (list)))
			(each (lambda (path path_2d)
				(each (lambda (seg seg_2d)
					(when (and (gt (length seg) 1)
							(or (eq show (defq z (mod (bit-shr (elem 2 (elem 0 seg)) fp_shift) pcb_depth)))
								(eq show -1)))
						(points-stroke-polylines stack track_radius eps join-round cap-round cap-round
							(list seg_2d) (elem z layers)))
					) path path_2d)
				) batched_paths batched_paths_2d)
			(each! 0 pcb_depth nil (lambda (layer color)
				(canvas-set-color canvas color)
				(canvas-fpoly canvas 0.0 0.0 1 layer)
				) (list layers colors))
			;draw vias
			(each (lambda (path_2d)
				(each! 1 nil nil (lambda (seg_2d)
					(bind '(x y) (slice 0 2 seg_2d))
					(canvas-set-color canvas (const (trans argb_white)))
					(canvas-fpoly canvas x y 0 (circle via_radius))
					(canvas-set-color canvas (const (trans argb_black)))
					(canvas-fpoly canvas x y 0 (circle (div via_radius 2)))
					) (list path_2d))
				) batched_paths_2d))
		;draw pads
		(each (lambda ((pad_radius pad_gap (pad_x pad_y pad_z) pad_shape))
			(when (or (eq show (bit-shr pad_z fp_shift)) (eq show -1))
				(setq pad_radius (mul zoom pad_radius) pad_gap (mul zoom pad_gap)
					pad_x (mul zoom pad_x) pad_y (mul zoom pad_y)
					pad_shape (to-2d pad_shape))
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
						(canvas-fpoly canvas pad_x pad_y 0 (list pad_shape)))))
			) pads)
		) (slice 1 -1 pcb)))

(defun pcb-draw-gerber ()
	(each (lambda ((id track_radius via_radius track_gap pads paths))
		(defq track_radius (mul zoom track_radius) via_radius (mul zoom via_radius)
			track_gap (mul zoom track_gap))
		;first draw in white including track and pad gaps
		(canvas-set-color canvas (const argb_white))
		(when (ne track_radius 0)
			;draw layers
			(defq batched_paths (map batch paths) batched_paths_2d (map batch-to-2d batched_paths) layer (list))
			(each (lambda (path path_2d)
				(each (lambda (seg seg_2d)
					(when (and (gt (length seg) 1)
							(eq show (defq z (mod (bit-shr (elem 2 (elem 0 seg)) fp_shift) pcb_depth))))
						(points-stroke-polylines stack (add track_radius track_gap) eps join-round cap-round cap-round
							(list seg_2d) layer))
					) path path_2d)
				) batched_paths batched_paths_2d)
			(canvas-fpoly canvas 0.0 0.0 1 layer)
			;draw vias
			(each (lambda (path_2d)
				(each! 1 nil nil (lambda (seg_2d)
					(bind '(x y) (slice 0 2 seg_2d))
					(canvas-fpoly canvas x y 0 (circle (add via_radius track_gap)))
					) (list path_2d))
				) batched_paths_2d))
		;draw pads
		(each (lambda ((pad_radius pad_gap (pad_x pad_y pad_z) pad_shape))
			(when (eq show (bit-shr pad_z fp_shift))
				(setq pad_radius (mul zoom pad_radius) pad_gap (mul zoom pad_gap)
					pad_x (mul zoom pad_x) pad_y (mul zoom pad_y)
					pad_shape (to-2d pad_shape))
				(cond
					((eq (length pad_shape) 0)
						;circular pad
						(canvas-fpoly canvas pad_x pad_y 0 (circle (add pad_radius pad_gap))))
					((eq (length pad_shape) 4)
						;oval pad
						(canvas-fpoly canvas pad_x pad_y 0 (oval (add pad_radius pad_gap) pad_shape)))
					(t
						;polygon pad
						(canvas-fpoly canvas pad_x pad_y 0
							(points-stroke-polygons stack pad_gap eps join-round (list pad_shape) (list))))))
			) pads)
		;second draw in block
		(canvas-set-color canvas (const argb_black))
		(when (ne track_radius 0)
			;draw layers
			(defq batched_paths (map batch paths) batched_paths_2d (map batch-to-2d batched_paths) layer (list))
			(each (lambda (path path_2d)
				(each (lambda (seg seg_2d)
					(when (and (gt (length seg) 1)
							(eq show (defq z (mod (bit-shr (elem 2 (elem 0 seg)) fp_shift) pcb_depth))))
						(points-stroke-polylines stack track_radius eps join-round cap-round cap-round
							(list seg_2d) layer))
					) path path_2d)
				) batched_paths batched_paths_2d)
			(canvas-fpoly canvas 0.0 0.0 1 layer)
			;draw vias
			(each (lambda (path_2d)
				(each! 1 nil nil (lambda (seg_2d)
					(bind '(x y) (slice 0 2 seg_2d))
					(canvas-fpoly canvas x y 0 (circle via_radius))
					) (list path_2d))
				) batched_paths_2d))
		;draw pads
		(each (lambda ((pad_radius pad_gap (pad_x pad_y pad_z) pad_shape))
			(when (eq show (bit-shr pad_z fp_shift))
				(setq pad_radius (mul zoom pad_radius) pad_gap (mul zoom pad_gap)
					pad_x (mul zoom pad_x) pad_y (mul zoom pad_y)
					pad_shape (to-2d pad_shape))
				(cond
					((eq (length pad_shape) 0)
						;circular pad
						(canvas-fpoly canvas pad_x pad_y 0 (circle pad_radius)))
					((eq (length pad_shape) 4)
						;oval pad
						(canvas-fpoly canvas pad_x pad_y 0 (oval pad_radius pad_shape)))
					(t
						;polygon pad
						(canvas-fpoly canvas pad_x pad_y 0 (list pad_shape)))))
			) pads)
		) (slice 1 -1 pcb)))

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
			(setq zoom (if (eq zoom min_zoom) min_zoom (dec zoom)))
			(win-refresh index))
		((eq id event_win_scale_up)
			(setq zoom (if (eq zoom max_zoom) max_zoom (inc zoom)))
			(win-refresh index))
		((le event_win_show_all id event_win_show_6)
			(setq show (sub id event_win_show_all 1))
			(win-refresh index))
		((le event_win_mode_normal id event_win_mode_gerber)
			(setq mode (sub id event_win_mode_normal))
			(win-refresh index))
		(t (view-event window msg))))
