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

(defun harmonic-pos (dir_angle freq amp &optional phase)
	(defq p (ifn phase 0.0)
		r (* amp (sin (+ (* angle freq) p))))
	(list
		(+ (* f_width f_scale 0.5) (* r (cos dir_angle)))
		(+ (* f_height f_scale 0.5) (* r (sin dir_angle)))))

(defun harmonic-scale (freq amp &optional phase)
	(defq p (ifn phase 0.0))
	(* f_scale (+ 1.0 (* amp (sin (+ (* angle freq) p))))))

(defun transform-copy (angle %1 &optional tx ty scale)
	(defq scale (ifn scale f_scale)
		sa (sin angle) ca (cos angle)
		tx (ifn tx (* f_width f_scale 0.5))
		ty (ifn ty (* f_height f_scale 0.5)))
	(map (lambda (%0)
		(path-transform (fixeds
			(* scale ca) (* scale (* sa -1.0)) tx
			(* scale sa) (* scale ca) ty)
			%0 (cat %0))) %1))

(defun transform-shadow (angle %1 &optional tx ty scale dx dy)
	(defq scale (ifn scale f_scale)
		dx (ifn dx (* scale 6.0))
		dy (ifn dy (* scale 6.0))
		tx (+ (ifn tx (* f_width f_scale 0.5)) dx)
		ty (+ (ifn ty (* f_height f_scale 0.5)) dy))
	(transform-copy angle %1 tx ty scale))

(defun fpoly (col mode %2)
	(.-> *canvas* (:set_color col) (:fpoly 0.0 0.0 mode %2)))

(defun redraw ()
	(. *canvas* :fill 0)

	; --- 1. Background: Self-intersecting 5-point star (harmonic breathing pulse) ---
	(defq star_scale (harmonic-scale 3.2 0.22))
	(fpoly 0xa0d01828 +winding_odd_even (transform-copy (* angle 1.8) star_poly :nil :nil star_scale))

	; --- 2. Acute Zigzag (diagonal harmonic slide + breathing zoom) ---
	(defq zig_ang (* angle -1.0)
		zig_s (harmonic-scale 2.0 0.22 0.8))
	(bind '(zg_x zg_y) (harmonic-pos (* angle 0.6) 2.5 (* f_width 0.15) 0.8))
	(fpoly 0x40000000 +winding_none_zero (transform-shadow zig_ang zigzag_miter zg_x zg_y zig_s))
	(fpoly 0xc00091ff +winding_none_zero (transform-copy zig_ang zigzag_miter zg_x zg_y zig_s))

	; --- 3. Quadratic Bézier Ribbon (swooping wave + harmonic scale) ---
	(defq quad_ang (* angle -0.7)
		quad_s (harmonic-scale 1.8 0.20 1.4))
	(bind '(qd_x qd_y) (harmonic-pos (* angle -0.5) 1.8 (* f_width 0.14) 2.1))
	(fpoly 0x35000000 +winding_none_zero (transform-shadow quad_ang quad_ribbon qd_x qd_y quad_s))
	(fpoly 0xb000e5ff +winding_none_zero (transform-copy quad_ang quad_ribbon qd_x qd_y quad_s))

	; --- 4. Circular Donut Ring (harmonic orbit pulse + zoom) ---
	(defq ring_ang (* angle 1.2)
		ring_s (harmonic-scale 2.4 0.22 2.0))
	(bind '(rng_x rng_y) (harmonic-pos (* angle 1.0) 3.2 (* f_width 0.12) 1.5))
	(fpoly 0x35000000 +winding_none_zero (transform-shadow ring_ang ring_outline rng_x rng_y ring_s))
	(fpoly 0xe0ff007f +winding_none_zero (transform-copy ring_ang ring_outline rng_x rng_y ring_s))
	(fpoly 0x50000000 +winding_none_zero (transform-copy ring_ang ring_core rng_x rng_y ring_s))

	; --- 5. Dual Arc Horns (perpendicular oscillation + zoom) ---
	(defq horns_ang angle
		horns_s (harmonic-scale 1.9 0.24 0.5))
	(bind '(hrn_x hrn_y) (harmonic-pos (+ angle +fp_hpi) 2.2 (* f_width 0.15) 0.4))
	(fpoly 0x35000000 +winding_none_zero (transform-shadow horns_ang horns_outline hrn_x hrn_y horns_s))
	(fpoly 0xd00022ee +winding_none_zero (transform-copy horns_ang horns_outline hrn_x hrn_y horns_s))
	(fpoly 0x90ffffff +winding_none_zero (transform-copy horns_ang horns_core hrn_x hrn_y horns_s))

	; --- 6. Emerald Spear Arrow (in/out thrust along direction + zoom) ---
	(defq arrow2_ang (* angle 1.4)
		arrow2_s (harmonic-scale 2.5 0.22 1.6))
	(bind '(a2_x a2_y) (harmonic-pos arrow2_ang 2.8 (* f_width 0.18) 1.2))
	(fpoly 0x40000000 +winding_none_zero (transform-shadow arrow2_ang arrow2_outline a2_x a2_y arrow2_s))
	(fpoly 0xe000e676 +winding_none_zero (transform-copy arrow2_ang arrow2_outline a2_x a2_y arrow2_s))
	(fpoly 0x80003322 +winding_none_zero (transform-copy arrow2_ang arrow2_core a2_x a2_y arrow2_s))

	; --- 7. Featured Golden Bézier Arrow (gliding trajectory + zoom) ---
	(defq arrow1_ang (* angle -1.6)
		arrow1_s (harmonic-scale 2.1 0.22 2.7))
	(bind '(a1_x a1_y) (harmonic-pos arrow1_ang 2.4 (* f_width 0.16) 2.8))
	(fpoly 0x50000000 +winding_none_zero (transform-shadow arrow1_ang arrow1_outline a1_x a1_y arrow1_s))
	(fpoly +argb_yellow +winding_none_zero (transform-copy arrow1_ang arrow1_outline a1_x a1_y arrow1_s))
	(fpoly 0xa018202c +winding_none_zero (transform-copy arrow1_ang arrow1_core a1_x a1_y arrow1_s))

	; --- 8. Rounded Rectangle Badge (floating off-center + gentle breathing) ---
	(defq badge_ang (* angle 0.6)
		badge_s (harmonic-scale 1.5 0.18 0.9))
	(bind '(bdg_x bdg_y) (harmonic-pos (+ badge_ang 1.2) 1.5 (* f_width 0.16) 0.0))
	(fpoly 0x40000000 +winding_none_zero (transform-shadow badge_ang badge_outline bdg_x bdg_y badge_s))
	(fpoly +argb_orange +winding_none_zero (transform-copy badge_ang badge_outline bdg_x bdg_y badge_s))
	(fpoly 0x85201040 +winding_none_zero (transform-copy badge_ang badge_core bdg_x bdg_y badge_s))

	; --- 9. Orbiting Vector Typography (breathing radial expansion + subtle scale) ---
	(defq cx (* f_width f_scale 0.5)
		cy (* f_height f_scale 0.5)
		txt_r (* f_width 0.12 (sin (* angle 3.0)))
		txt_s (harmonic-scale 2.6 0.15 0.3)
		a1 (/ angle 2.0)
		a2 (+ a1 +fp_pi)
		a3 (+ a1 +fp_hpi)
		a4 (- a1 +fp_hpi))

	(defq t1_x (+ cx (* txt_r (cos a1))) t1_y (+ cy (* txt_r (sin a1))))
	(fpoly 0x50000000 +winding_none_zero (transform-shadow a1 fp1 t1_x t1_y txt_s))
	(fpoly 0xff000000 +winding_none_zero (transform-copy a1 fp1 t1_x t1_y txt_s))

	(defq t2_x (+ cx (* txt_r (cos a2))) t2_y (+ cy (* txt_r (sin a2))))
	(fpoly 0x50000000 +winding_none_zero (transform-shadow a2 fp2 t2_x t2_y txt_s))
	(fpoly 0xff000000 +winding_none_zero (transform-copy a2 fp2 t2_x t2_y txt_s))

	(defq t3_x (+ cx (* txt_r (cos a3))) t3_y (+ cy (* txt_r (sin a3))))
	(fpoly 0x50000000 +winding_none_zero (transform-shadow a3 fp3 t3_x t3_y txt_s))
	(fpoly 0xffffffff +winding_none_zero (transform-copy a3 fp3 t3_x t3_y txt_s))

	(defq t4_x (+ cx (* txt_r (cos a4))) t4_y (+ cy (* txt_r (sin a4))))
	(fpoly 0x50000000 +winding_none_zero (transform-shadow a4 fp4 t4_x t4_y txt_s))
	(fpoly 0xffffffff +winding_none_zero (transform-copy a4 fp4 t4_x t4_y txt_s))

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
				(++ angle 0.005))
			(:t (. *window* :event msg))))
	(gui-sub-rpc *window*))