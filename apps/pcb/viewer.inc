;;;;;;;;
; viewer
;;;;;;;;

;module
(env-push)

(defun trans (_) (+ (logand 0xffffff _) 0xa0000000))

(defq +eps 0.25
	+colors (static '(map (const trans) (list
		+argb_red +argb_green +argb_blue +argb_yellow +argb_cyan +argb_magenta))))

(defun circle (r)
	;cached circle generation
	(defq cache (const (fmap 11)))
	(unless (defq p (. cache :find r))
		(. cache :insert r (defq p
			(list (path-gen-arc 0.0 0.0 0.0 +fp_2pi r 0.25 (path))))))
	p)

(defun oval (r s)
	;cached oval generation
	(defq cache (const (fmap 11)) key (str r s))
	(unless (defq p (. cache :find key))
		(. cache :insert key (defq p
			(path-stroke-polylines (list) r +eps +join_bevel +cap_round +cap_round (list s)))))
	p)

(defun batch (p)
	(defq s 0 e 0 b (list))
	(while (<= (setq e (inc e)) (length p))
		(when (or (= e (length p)) (/= (elem-get 2 (elem-get s p)) (elem-get 2 (elem-get e p))))
			(push b (slice s e p))
			(setq s e))) b)

(defun to-path (_)
	(reduce (lambda (p (x y &optional z))
		(push p (* zoom x) (* zoom y))) _ (path)))

(defun batch-to-path (_)
	(map (const to-path) _))

(defun pcb-draw-normal ()
	(each! 1 -1 (lambda ((id track_radius via_radius track_gap pads wires &optional paths))
		(if paths (setq wires (cat wires paths)))
		(setq track_radius (* zoom track_radius) via_radius (* zoom via_radius) track_gap (* zoom track_gap))
		(when (/= track_radius 0.0)
			;draw layers
			(defq batched_paths (map (const batch) wires)
				batched_paths_2d (map (const batch-to-path) batched_paths)
				layers (list (list) (list) (list) (list) (list) (list)))
			(each (lambda (p path_2d)
				(each (lambda (seg seg_2d)
					(when (or (= show (defq z (% (>> (elem-get 2 (elem-get 0 seg)) +fp_shift) pcb_depth))) (= show -1))
						(path-stroke-polylines (elem-get z layers) track_radius +eps +join_round +cap_round +cap_round (list seg_2d)))
					) p path_2d)
				) batched_paths batched_paths_2d)
			(each! 0 pcb_depth
				(lambda (layer color)
					(.-> canvas
						(:set_color color)
						(:fpoly pcb_border pcb_border +winding_none_zero layer)))
				(list layers +colors))
			;draw vias
			(each (lambda (path_2d)
				(each! 1 -1
					(lambda (seg_2d)
						(bind '(x y) (slice 0 2 seg_2d))
						(setq x (+ x pcb_border) y (+ y pcb_border))
						(.-> canvas
							(:set_color (const (trans +argb_white)))
							(:fpoly x y +winding_odd_even (circle via_radius))
							(:set_color (const (trans +argb_black)))
							(:fpoly x y +winding_odd_even (circle (/ via_radius 2.0)))))
					(list path_2d))
				) batched_paths_2d))
		;draw pads
		(each (lambda ((pad_radius pad_gap (pad_x pad_y pad_z) pad_shape))
			(when (or (= show -1) (= show (>> pad_z +fp_shift)))
				(setq pad_radius (* zoom pad_radius) pad_gap (* zoom pad_gap)
					pad_x (+ (* zoom pad_x) pcb_border) pad_y (+ (* zoom pad_y) pcb_border)
					pad_shape (to-path pad_shape))
				(. canvas :set_color (const (trans +argb_white)))
				(cond
					((= (length pad_shape) 0)
						;circular pad
						(. canvas :fpoly pad_x pad_y +winding_odd_even (circle pad_radius)))
					((= (length pad_shape) 4)
						;oval pad
						(. canvas :fpoly pad_x pad_y +winding_odd_even (oval pad_radius pad_shape)))
					(t  ;polygon pad
						(. canvas :fpoly pad_x pad_y +winding_odd_even (list pad_shape)))))) pads)
		) (list pcb)))

(defun pcb-draw-gerber-layer (with_gaps)
	(each! 1 -1 (lambda ((id track_radius via_radius track_gap pads wires &optional paths))
		(if paths (setq wires (cat wires paths)))
		(setq track_radius (* zoom track_radius) via_radius (* zoom via_radius) track_gap (* zoom track_gap))
		(when (/= track_radius 0.0)
			;draw layers
			(defq batched_paths (map (const batch) wires)
				batched_paths_2d (map (const batch-to-path) batched_paths)
				layer (list))
			(each (lambda (p path_2d)
				(each (lambda (seg seg_2d)
					(when (= show (defq z (% (>> (elem-get 2 (elem-get 0 seg)) +fp_shift) pcb_depth)))
						(path-stroke-polylines layer (+ track_radius (if with_gaps track_gap 0.0))
							+eps +join_round +cap_round +cap_round (list seg_2d)))
					) p path_2d)
				) batched_paths batched_paths_2d)
			(. canvas :fpoly pcb_border pcb_border +winding_none_zero layer)
			;draw vias
			(each (lambda (path_2d)
				(each! 1 -1
					(lambda (seg_2d)
						(bind '(x y) (slice 0 2 seg_2d))
						(setq x (+ x pcb_border) y (+ y pcb_border))
						(. canvas :fpoly x y +winding_odd_even (circle (+ via_radius (if with_gaps track_gap 0.0)))))
					(list path_2d))
				) batched_paths_2d))
		;draw pads
		(each (lambda ((pad_radius pad_gap (pad_x pad_y pad_z) pad_shape))
			(when (= show (>> pad_z +fp_shift))
				(setq pad_radius (* zoom pad_radius) pad_gap (* zoom pad_gap)
					pad_x (+ (* zoom pad_x) pcb_border) pad_y (+ (* zoom pad_y) pcb_border)
					pad_shape (to-path pad_shape))
				(cond
					((= (length pad_shape) 0)
						;circular pad
						(. canvas :fpoly pad_x pad_y +winding_odd_even (circle (+ pad_radius (if with_gaps pad_gap 0.0)))))
					((= (length pad_shape) 4)
						;oval pad
						(. canvas :fpoly pad_x pad_y +winding_odd_even (oval (+ pad_radius (if with_gaps pad_gap 0.0)) pad_shape)))
					(t  ;polygon pad
						(if with_gaps
							(. canvas :fpoly pad_x pad_y +winding_odd_even
								(path-stroke-polygons (list) pad_gap +eps +join_round (list pad_shape)))
							(. canvas :fpoly pad_x pad_y +winding_odd_even
								(list pad_shape))))))
			) pads)
		) (list pcb)))

(defun pcb-draw-gerber ()
	;first draw in white with gaps
	(. canvas :set_color +argb_white)
	(pcb-draw-gerber-layer t)
	;second draw in black without gaps
	(. canvas :set_color +argb_black)
	(pcb-draw-gerber-layer nil))

(defun pcb-canvas (pcb mode show zoom canvas_scale)
	(bind '(pcb_width pcb_height pcb_depth) (elem-get 0 pcb))
	(defq canvas (Canvas (* (+ pcb_width 4) (n2i zoom)) (* (+ pcb_height 4) (n2i zoom)) canvas_scale)
		zoom (* zoom (n2f canvas_scale)) pcb_border (* zoom 2.0))
	(.-> canvas (:set_canvas_flags +canvas_flag_antialias) (:fill +argb_black))
	(if (= mode 1)
		(pcb-draw-gerber)
		(pcb-draw-normal))
	canvas)

;module
(export-symbols
	pcb-canvas)
(env-pop)
