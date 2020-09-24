;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)
(import 'lib/math/math.inc)

(structure '+event 0
	(byte 'close+)
	(byte 'prev+ 'next+ 'scale_down+ 'scale_up+ 'mode_normal+ 'mode_gerber+)
	(byte 'show_all+ 'show_1+ 'show_2+ 'show_3+ 'show_4+))

(defun-bind all-pcbs (p)
	(defq out (list))
	(each! 0 -1 (lambda (f m) (and (eql m "8") (ends-with ".pcb" f) (push out (cat p f))))
		(unzip (split (pii-dirlist p) ",") (list (list) (list))))
	(sort cmp out))

(defq pcbs (all-pcbs "apps/pcb/") index 1 canvas_scale 1 mode 0 show -1
	max_zoom 15.0 min_zoom 5.0 zoom (/ (+ min_zoom max_zoom) 2.0) eps 0.25)

(ui-window window ()
	(ui-title-bar window_title "" (0xea19) +event_close+)
	(ui-tool-bar _ ()
		(ui-buttons (0xe91d 0xe91e 0xea00 0xea01 0xe9ac 0xe9ad) +event_prev+)
		(ui-buttons ("0" "1" "2" "3" "4") +event_show_all+
			(:color (const *env_toolbar2_col*) :font (const (create-font "fonts/OpenSans-Regular.ctf" 24)))))
	(ui-scroll pcb_scroll (logior scroll_flag_vertical scroll_flag_horizontal) (:min_width 512 :min_height 256)))

(defun-bind trans (_)
	(+ (logand 0xffffff _) 0xa0000000))

(defun-bind circle (r)
	;cached circle generation
	(defq i (% (logior r) 7) k (elem i '(()()()()()()())) p (elem i '(()()()()()()())))
	(cond ((defq i (some (lambda (i) (if (= i r) _)) k)) (elem i p))
		(t (push k r) (elem -2 (push p (list
			(path-gen-arc 0.0 0.0 0.0 (const +fp_2pi+) r 0.25 (path))))))))

(defun-bind oval (r s)
	;cached oval generation
	(defq k '() p '())
	(cond ((defq i (find-rev (defq _ (sym (str r s))) k)) (elem i p))
		(t (push k _) (elem -2 (push p
			(path-stroke-polylines (list) r eps join_bevel cap_round cap_round (list s)))))))

(defun-bind batch (p)
	(defq s 0 e 0 b (list))
	(while (<= (setq e (inc e)) (length p))
		(when (or (= e (length p)) (/= (elem 2 (elem s p)) (elem 2 (elem e p))))
			(push b (slice s e p))
			(setq s e))) b)

(defun-bind to-2d (_)
	(reduce (lambda (p _)
		(push p (* zoom (elem 0 _)) (* zoom (elem 1 _)))) _ (path)))

(defun-bind batch-to-2d (_)
	(map to-2d _))

(defun-bind pcb-load (_)
	(bind '(pcb _) (read (string-stream (cat "(" (load _) ")")) (ascii-code " ")))
	(bind '(pcb_width pcb_height pcb_depth) (elem 0 pcb))
	(defq canvas (create-canvas (* (+ pcb_width 4) (f2i zoom)) (* (+ pcb_height 4) (f2i zoom)) canvas_scale)
		zoom (* zoom (i2f canvas_scale)) pcb_border (* zoom 2.0))
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
		(when (/= track_radius 0.0)
			;draw layers
			(defq batched_paths (map batch paths) batched_paths_2d (map batch-to-2d batched_paths)
				layers (list (list) (list) (list) (list) (list) (list)))
			(each (lambda (p path_2d)
				(each (lambda (seg seg_2d)
					(when (or (= show (defq z (% (>> (elem 2 (elem 0 seg)) +fp_shift+) pcb_depth))) (= show -1))
						(path-stroke-polylines (elem z layers) track_radius eps join_round cap_round cap_round (list seg_2d)))
					) p path_2d)
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
					(canvas-fpoly canvas x y 0 (circle (/ via_radius 2.0)))
					) (list path_2d))
				) batched_paths_2d))
		;draw pads
		(each (lambda ((pad_radius pad_gap (pad_x pad_y pad_z) pad_shape))
			(when (or (= show -1) (= show (>> pad_z +fp_shift+)))
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
					(t	;polygon pad
						(canvas-fpoly canvas pad_x pad_y 0 (list pad_shape)))))) pads)
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
		(setq track_radius (* zoom track_radius) via_radius (* zoom via_radius)
			track_gap (* zoom track_gap))
		(when (/= track_radius 0.0)
			;draw layers
			(defq batched_paths (map batch paths) batched_paths_2d (map batch-to-2d batched_paths) layer (list))
			(each (lambda (p path_2d)
				(each (lambda (seg seg_2d)
					(when (= show (defq z (% (>> (elem 2 (elem 0 seg)) +fp_shift+) pcb_depth)))
						(path-stroke-polylines layer (+ track_radius (if with_gaps track_gap 0.0))
							eps join_round cap_round cap_round (list seg_2d)))
					) p path_2d)
				) batched_paths batched_paths_2d)
			(canvas-fpoly canvas pcb_border pcb_border 1 layer)
			;draw vias
			(each (lambda (path_2d)
				(each! 1 -1 (lambda (seg_2d)
					(bind '(x y) (slice 0 2 seg_2d))
					(setq x (+ x pcb_border) y (+ y pcb_border))
					(canvas-fpoly canvas x y 0 (circle (+ via_radius (if with_gaps track_gap 0.0))))
					) (list path_2d))
				) batched_paths_2d))
		;draw pads
		(each (lambda ((pad_radius pad_gap (pad_x pad_y pad_z) pad_shape))
			(when (= show (>> pad_z +fp_shift+))
				(setq pad_radius (* zoom pad_radius) pad_gap (* zoom pad_gap)
					pad_x (+ (* zoom pad_x) pcb_border) pad_y (+ (* zoom pad_y) pcb_border)
					pad_shape (to-2d pad_shape))
				(cond
					((= (length pad_shape) 0)
						;circular pad
						(canvas-fpoly canvas pad_x pad_y 0 (circle (+ pad_radius (if with_gaps pad_gap 0.0)))))
					((= (length pad_shape) 4)
						;oval pad
						(canvas-fpoly canvas pad_x pad_y 0 (oval (+ pad_radius (if with_gaps pad_gap 0.0)) pad_shape)))
					(t	;polygon pad
						(if with_gaps
							(canvas-fpoly canvas pad_x pad_y 0
								(path-stroke-polygons (list) pad_gap eps join_round (list pad_shape)))
							(canvas-fpoly canvas pad_x pad_y 0
								(list pad_shape))))))
			) pads)
		) (list pcb)))

(defun-bind win-refresh (_)
	(view-layout (view-add-child pcb_scroll (pcb-load (elem (setq index _) pcbs))))
	(def window_title :text (elem _ pcbs))
	(view-layout window_title)
	(view-dirty-all (view-layout window)))

(defun-bind main ()
	(bind '(x y w h) (apply view-locate (view-pref-size (win-refresh index))))
	(gui-add (view-change window x y w h))
	(while (cond
		((= (defq id (get-long (defq msg (mail-read (task-mailbox))) ev_msg_target_id)) +event_close+)
			nil)
		((<= +event_prev+ id +event_next+)
			(win-refresh (% (+ index (dec (* 2 (- id +event_prev+))) (length pcbs)) (length pcbs))))
		((<= +event_scale_down+ id +event_scale_up+)
			(setq zoom (max (min (+ zoom (i2f (dec (* 2 (- id +event_scale_down+))))) max_zoom) min_zoom))
			(win-refresh index))
		((<= +event_show_all+ id +event_show_4+)
			(setq show (- id +event_show_all+ 1))
			(win-refresh index))
		((<= +event_mode_normal+ id +event_mode_gerber+)
			(setq mode (- id +event_mode_normal+))
			(win-refresh index))
		(t (view-event window msg))))
	(view-hide window))
