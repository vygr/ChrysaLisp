(import "usr/env.inc")
(import "gui/lisp.inc")
(import "lib/math/vector.inc")

(enums +select 0
	(enum main timer))

(enums +event 0
	(enum close))

(defq canvas_width 600 canvas_height 600 canvas_scale 1
	f_width (n2f canvas_width) f_height (n2f canvas_height) f_scale (n2f canvas_scale)
	rate (/ 1000000 30) angle 0.0
	font (or (create-font "fonts/OpenSans-Bold.ctf" 32)
			(create-font "fonts/OpenSans-Regular.ctf" 32)
			(create-font "fonts/Hack-Regular.ctf" 28))
	fp1 (font-glyph-paths font "    Vectors!")
	fp2 (font-glyph-paths font "    Beziers!")
	fp3 (font-glyph-paths font "    Strokes!")
	fp4 (font-glyph-paths font "    Quality!")

	; 1. Golden Arrow: Cubic bezier with round tail and barbed head (+cap_round, +cap_arrow)
	; Using the stroke-over-stroke technique: polyline stroke -> polygon stroke -> outline + core
	arrow1_body
		(path-stroke-polylines (list) (* f_width 0.035) +join_bevel +cap_round +cap_arrow
			(list (path-gen-cubic
				(* f_width -0.42) (* f_height 0.28)
				(* f_width -0.26) (* f_height -0.30)
				(* f_width 0.40) (* f_height 0.55)
				(* f_width 0.38) (* f_height -0.35)
				(path))))
	arrow1_outline
		(path-stroke-polygons (list) (* f_width 0.012) +join_miter arrow1_body)
	arrow1_core
		(slice arrow1_outline 1 2)

	; 2. Emerald Arrow: Cubic bezier with butt tail and triangle head (+cap_butt, +cap_tri)
	arrow2_body
		(path-stroke-polylines (list) (* f_width 0.030) +join_bevel +cap_butt +cap_tri
			(list (path-gen-cubic
				(* f_width 0.38) (* f_height 0.24)
				(* f_width 0.18) (* f_height -0.36)
				(* f_width -0.22) (* f_height 0.36)
				(* f_width -0.36) (* f_height -0.24)
				(path))))
	arrow2_outline
		(path-stroke-polygons (list) (* f_width 0.010) +join_bevel arrow2_body)
	arrow2_core
		(slice arrow2_outline 1 2)

	; 3. Rounded Rectangle Badge: Demonstrates path-gen-rect with corner radii rx/ry
	badge_rect
		(list (path-gen-rect
			(* f_width -0.22) (* f_height -0.13)
			(* f_width 0.22) (* f_height 0.13)
			(* f_width 0.04) (* f_height 0.04) (path)))
	badge_outline
		(path-stroke-polygons (list) (* f_width 0.012) +join_round badge_rect)
	badge_core
		(slice badge_outline 1 2)

	; 4. Arc Horns: Dual circular arcs with +cap_square and +cap_tri
	horns_body
		(path-stroke-polylines (list) (* f_width 0.048) +join_bevel +cap_square +cap_tri
			(list
				(path-gen-arc (* f_width -0.10) (* f_height -0.18) 0.8 1.6 (* f_width 0.20) (path))
				(path-gen-arc (* f_width -0.18) (* f_height -0.18) 3.8 2.0 (* f_width 0.11) (path))))
	horns_outline
		(path-stroke-polygons (list) (* f_width 0.020) +join_miter horns_body)
	horns_core
		(list (second horns_outline) (elem-get horns_outline 3))

	; 5. Circular Donut Ring: Full 2pi arc with +join_miter
	ring_body
		(list (path-gen-arc (* f_width 0.18) (* f_height 0.24) 0.0 +fp_2pi (* f_width 0.11) (path)))
	ring_outline
		(path-stroke-polygons (list) (* f_width 0.018) +join_miter ring_body)
	ring_core
		(slice ring_outline 0 1)

	; 6. Quadratic Ribbon: High parabolic arch using path-gen-quadratic with +join_round and +cap_round
	quad_ribbon
		(path-stroke-polylines (list) (* f_width 0.048) +join_round +cap_round +cap_round
			(list (path-gen-quadratic
				(* f_width -0.40) (* f_height 0.40)
				(* f_width -0.18) (* f_height -1.05)
				(* f_width 0.40) (* f_height 0.20)
				(path))))

	; 7. Self-Intersecting 5-Point Star: Demonstrates +winding_odd_even hollow center
	star_poly
		(list (path
			0.0 (* f_height -0.42)
			(* f_width 0.247) (* f_height 0.340)
			(* f_width -0.399) (* f_height -0.130)
			(* f_width 0.399) (* f_height -0.130)
			(* f_width -0.247) (* f_height 0.340)))

	; 8. Sharp Zigzag: Demonstrates acute +join_miter corners with +cap_square and +cap_butt
	zigzag_miter
		(path-stroke-polylines (list) (* f_width 0.030) +join_miter +cap_square +cap_butt
			(list (path
				(* f_width -0.35) (* f_height -0.32)
				(* f_width -0.20) (* f_height -0.45)
				(* f_width -0.05) (* f_height -0.32)
				(* f_width 0.10) (* f_height -0.45))))
)

(ui-window *window* ()
	(ui-title-bar _ "Canvas Showcase" (0xea19) +event_close)
	(ui-canvas *canvas* canvas_width canvas_height canvas_scale))

(defun transform-copy (angle %1 &optional tx ty)
	(defq sa (sin angle) ca (cos angle)
		tx (ifn tx (* f_width f_scale 0.5))
		ty (ifn ty (* f_height f_scale 0.5)))
	(map (lambda (%0)
		(path-transform (fixeds
			(* f_scale ca) (* f_scale (* sa -1.0)) tx
			(* f_scale sa) (* f_scale ca) ty)
			%0 (cat %0))) %1))

(defun transform-shadow (angle %1 &optional dx dy)
	(defq dx (ifn dx (* f_scale 6.0))
		dy (ifn dy (* f_scale 6.0)))
	(transform-copy angle %1
		(+ (* f_width f_scale 0.5) dx)
		(+ (* f_height f_scale 0.5) dy)))

(defun fpoly (col mode %2)
	(.-> *canvas* (:set_color col) (:fpoly 0.0 0.0 mode %2)))

(defun redraw ()
	(. *canvas* :fill 0)

	; --- 1. Background: Self-intersecting 5-point star (+winding_odd_even hollow center) ---
	(fpoly 0xa0d01828 +winding_odd_even (transform-copy (* angle 1.8) star_poly))

	; --- 2. Acute Zigzag (+join_miter, +cap_square, +cap_butt) ---
	(fpoly 0x40000000 +winding_none_zero (transform-shadow (* angle -1.0) zigzag_miter))
	(fpoly 0xc00091ff +winding_none_zero (transform-copy (* angle -1.0) zigzag_miter))

	; --- 3. Quadratic Bézier Ribbon (+join_round, +cap_round) ---
	(fpoly 0x35000000 +winding_none_zero (transform-shadow (* angle -0.7) quad_ribbon))
	(fpoly 0xb000e5ff +winding_none_zero (transform-copy (* angle -0.7) quad_ribbon))

	; --- 4. Circular Donut Ring (+join_miter outline + interior core) ---
	(fpoly 0x35000000 +winding_none_zero (transform-shadow (* angle 1.2) ring_outline))
	(fpoly 0xe0ff007f +winding_none_zero (transform-copy (* angle 1.2) ring_outline))
	(fpoly 0x50000000 +winding_none_zero (transform-copy (* angle 1.2) ring_core))

	; --- 5. Dual Arc Horns (+join_bevel, +cap_square, +cap_tri) ---
	(fpoly 0x35000000 +winding_none_zero (transform-shadow angle horns_outline))
	(fpoly 0xd00022ee +winding_none_zero (transform-copy angle horns_outline))
	(fpoly 0x90ffffff +winding_none_zero (transform-copy angle horns_core))

	; --- 6. Emerald Spear Arrow (+cap_butt, +cap_tri) ---
	(fpoly 0x40000000 +winding_none_zero (transform-shadow (* angle 1.4) arrow2_outline))
	(fpoly 0xe000e676 +winding_none_zero (transform-copy (* angle 1.4) arrow2_outline))
	(fpoly 0x80003322 +winding_none_zero (transform-copy (* angle 1.4) arrow2_core))

	; --- 7. Featured Golden Bézier Arrow (+cap_round, +cap_arrow) ---
	(fpoly 0x50000000 +winding_none_zero (transform-shadow (* angle -1.6) arrow1_outline))
	(fpoly +argb_yellow +winding_none_zero (transform-copy (* angle -1.6) arrow1_outline))
	(fpoly 0xa018202c +winding_none_zero (transform-copy (* angle -1.6) arrow1_core))

	; --- 8. Rounded Rectangle Badge (path-gen-rect, +join_round) ---
	(fpoly 0x40000000 +winding_none_zero (transform-shadow (* angle 0.6) badge_outline))
	(fpoly +argb_orange +winding_none_zero (transform-copy (* angle 0.6) badge_outline))
	(fpoly 0x85201040 +winding_none_zero (transform-copy (* angle 0.6) badge_core))

	; --- 9. Orbiting Vector Typography (font-glyph-paths) ---
	(fpoly 0x50000000 +winding_none_zero (transform-shadow (/ angle 2.0) fp1))
	(fpoly 0xff000000 +winding_none_zero (transform-copy (/ angle 2.0) fp1))

	(fpoly 0x50000000 +winding_none_zero (transform-shadow (+ (/ angle 2.0) +fp_pi) fp2))
	(fpoly 0xff000000 +winding_none_zero (transform-copy (+ (/ angle 2.0) +fp_pi) fp2))

	(fpoly 0x50000000 +winding_none_zero (transform-shadow (+ (/ angle 2.0) +fp_hpi) fp3))
	(fpoly 0xffffffff +winding_none_zero (transform-copy (+ (/ angle 2.0) +fp_hpi) fp3))

	(fpoly 0x50000000 +winding_none_zero (transform-shadow (+ (/ angle 2.0) (* -1.0 +fp_hpi)) fp4))
	(fpoly 0xffffffff +winding_none_zero (transform-copy (+ (/ angle 2.0) (* -1.0 +fp_hpi)) fp4))

	(. *canvas* :swap +pixmap_mode_normal))

(defun main ()
	(defq select (task-mboxes +select_size) *running* :t)
	(.-> *canvas* (:fill 0) (:set_canvas_flags +canvas_flag_antialias))
	(bind '(x y w h) (apply view-locate (. *window* :pref_size)))
	(gui-add-front-rpc (. *window* :change x y w h))
	(mail-timeout (elem-get select +select_timer) rate 0)
	(while *running*
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((= idx +select_main)
				(if (= (getf msg +ev_msg_target_id) +event_close)
					(setq *running* :nil)
					(. *window* :event msg)))
			((= idx +select_timer)
				(mail-timeout (elem-get select +select_timer) rate 0)
				(redraw)
				(++ angle 0.0025))
			(:t (. *window* :event msg))))
	(gui-sub-rpc *window*))