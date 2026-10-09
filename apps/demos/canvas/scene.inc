;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The scene of the Canvas demo, and the message for a slice of it.
; The app draws it, and so do its children, each a slice of the rows,
; straight onto the shared pixmap of the app's canvas.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/math/vector.inc")
(import "lib/task/jobs.inc")

;a slice for a child to draw, the key of the shared pixels, the turn of
;the scene as a fixed, how many shapes there are, and the rows
(structure +slice +job_size
	(long shared angle count)
	(uint y y1))

;its answer, the first row, and how many shapes it drew, -1 if it could
;not reach the pixels
(structure +slice_reply +job_reply_size
	(uint y)
	;-1 from a child that could not reach the pixels
	(int drawn))

(defq +scene_width 1024 +scene_height 768
	;the shapes are made at the size they had on a canvas of 600
	f_width 600.0 f_height 600.0
	font (or (create-font "fonts/OpenSans-Bold.ctf" 32)
			(create-font "fonts/OpenSans-Regular.ctf" 32)
			(create-font "fonts/Hack-Regular.ctf" 28))
	;a word, the outlines of its glyphs made half as big again, and bolder,
	;the outline stroked and filled along with it. As they come from the
	;font the stems are a quarter of the width of the strokes around them,
	;and a couple of pixels wide when a shape is small
	scene-word (lambda (text)
		(defq m (fixeds 1.5 0.0 0.0 0.0 1.5 0.0)
			raw (map (# (path-transform m %0 (cat %0))) (font-glyph-paths font text)))
		(cat raw (path-stroke-polygons (list) 2.5 +join_round raw)))
	fp1 (scene-word "Vectors!")
	fp2 (scene-word "Beziers!")
	fp3 (scene-word "Strokes!")
	fp4 (scene-word "Quality!")

	; 1. Golden Arrow: Cubic bezier with round tail and barbed head (+cap_round, +cap_arrow)
	; Using the stroke-over-stroke technique: polyline stroke -> polygon stroke -> outline + core
	arrow1_body
		(path-stroke-polylines (list) (* f_width 0.062) +join_bevel +cap_round +cap_arrow
			(list (path-gen-cubic
				(* f_width -0.42) (* f_height 0.28)
				(* f_width -0.26) (* f_height -0.30)
				(* f_width 0.40) (* f_height 0.55)
				(* f_width 0.38) (* f_height -0.35)
				(path))))
	arrow1_outline
		(path-stroke-polygons (list) (* f_width 0.020) +join_miter arrow1_body)
	arrow1_core
		(slice arrow1_outline 1 2)

	; 2. Emerald Arrow: Cubic bezier with butt tail and triangle head (+cap_butt, +cap_tri)
	arrow2_body
		(path-stroke-polylines (list) (* f_width 0.054) +join_bevel +cap_butt +cap_tri
			(list (path-gen-cubic
				(* f_width 0.38) (* f_height 0.24)
				(* f_width 0.18) (* f_height -0.36)
				(* f_width -0.22) (* f_height 0.36)
				(* f_width -0.36) (* f_height -0.24)
				(path))))
	arrow2_outline
		(path-stroke-polygons (list) (* f_width 0.018) +join_bevel arrow2_body)
	arrow2_core
		(slice arrow2_outline 1 2)

	; 3. Rounded Rectangle Badge: Demonstrates path-gen-rect with corner radii rx/ry
	badge_rect
		(list (path-gen-rect
			(* f_width -0.22) (* f_height -0.13)
			(* f_width 0.22) (* f_height 0.13)
			(* f_width 0.04) (* f_height 0.04) (path)))
	badge_outline
		(path-stroke-polygons (list) (* f_width 0.025) +join_round badge_rect)
	badge_core
		(slice badge_outline 1 2)

	; 4. Arc Horns: Dual circular arcs with +cap_square and +cap_tri
	horns_body
		(path-stroke-polylines (list) (* f_width 0.078) +join_bevel +cap_square +cap_tri
			(list
				(path-gen-arc (* f_width -0.10) (* f_height -0.18) 0.8 1.6 (* f_width 0.20) (path))
				(path-gen-arc (* f_width -0.18) (* f_height -0.18) 3.8 2.0 (* f_width 0.11) (path))))
	horns_outline
		(path-stroke-polygons (list) (* f_width 0.031) +join_miter horns_body)
	horns_core
		(list (second horns_outline) (elem-get horns_outline 3))

	; 5. Circular Donut Ring: Full 2pi arc with +join_miter
	ring_body
		(list (path-gen-arc (* f_width 0.18) (* f_height 0.24) 0.0 +fp_2pi (* f_width 0.11) (path)))
	ring_outline
		(path-stroke-polygons (list) (* f_width 0.036) +join_miter ring_body)
	ring_core
		(slice ring_outline 0 1)

	; 6. Quadratic Ribbon: High parabolic arch using path-gen-quadratic with +join_round and +cap_round
	quad_ribbon
		(path-stroke-polylines (list) (* f_width 0.080) +join_round +cap_round +cap_round
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
		(path-stroke-polylines (list) (* f_width 0.056) +join_miter +cap_square +cap_butt
			(list (path
				(* f_width -0.35) (* f_height -0.32)
				(* f_width -0.20) (* f_height -0.45)
				(* f_width -0.05) (* f_height -0.32)
				(* f_width 0.10) (* f_height -0.45)))))

(defun scene-radius (paths)
	;how far from its own origin a shape reaches, and a little over
	(defq r 0.0)
	(each (lambda (p) (each (# (setq r (max r (abs %0)))) p)) paths)
	(* r 1.5))

(defun scene-kind (winding body body_col &optional core core_col)
	;a kind of shape, its paths, colours, and how far it reaches
	(list winding body body_col core core_col (scene-radius body) :nil))

(defun scene-live (body_col reach live)
	;a kind of shape that is made afresh for every frame, by (live turn
	;number), a curve that bends and is then stroked. That is a good deal
	;of work, and none of it is done for a shape that does not reach the
	;rows being drawn. So it must say how far it can ever reach.
	(list +winding_none_zero :nil body_col :nil :nil reach live))

(defun live-ribbon (turn fi)
	;a ribbon that bends two ways and swells
	(path-stroke-polylines (list) (* f_width (+ 0.056 (* 0.022 (sin (+ (* turn 5.0) fi)))))
		+join_round +cap_round +cap_round
		(list (path-gen-cubic
			(* f_width -0.34) 0.0
			(* f_width -0.12) (* f_height 0.32 (sin (+ (* turn 7.0) fi)))
			(* f_width 0.12) (* f_height -0.32 (sin (+ (* turn 5.5) (* fi 2.0))))
			(* f_width 0.34) 0.0 (path)))))

(defun live-horn (turn fi)
	;an arc that opens and closes, with a point on it
	(path-stroke-polylines (list) (* f_width 0.070) +join_bevel +cap_square +cap_tri
		(list (path-gen-arc 0.0 0.0 (* turn 2.0)
			(+ 2.6 (* 2.2 (sin (+ (* turn 6.0) fi)))) (* f_width 0.22) (path)))))

(defun live-ring (turn fi)
	;a ring that gets thick and thin
	(path-stroke-polygons (list) (* f_width (+ 0.054 (* 0.028 (sin (+ (* turn 8.0) fi)))))
		+join_miter
		(list (path-gen-arc 0.0 0.0 0.0 +fp_2pi (* f_width 0.20) (path)))))

(defq scene_kinds (list
	(scene-kind +winding_odd_even star_poly 0xa0d01828)
	(scene-kind +winding_none_zero zigzag_miter 0xc00091ff)
	(scene-kind +winding_none_zero quad_ribbon 0xb000e5ff)
	(scene-kind +winding_none_zero ring_outline 0xe0ff007f ring_core 0x50000000)
	(scene-kind +winding_none_zero horns_outline 0xd00022ee horns_core 0x90ffffff)
	(scene-kind +winding_none_zero arrow2_outline 0xe000e676 arrow2_core 0x80003322)
	(scene-kind +winding_none_zero arrow1_outline +argb_yellow arrow1_core 0xa018202c)
	(scene-kind +winding_none_zero badge_outline +argb_orange badge_core 0x85201040)
	(scene-kind +winding_none_zero fp1 0xffffffff)
	(scene-kind +winding_none_zero fp2 0xff80ffd0)
	(scene-kind +winding_none_zero fp3 0xffffd080)
	(scene-kind +winding_none_zero fp4 0xffd0a0ff)
	(scene-live 0xd040ffa0 (* f_width 0.60) live-ribbon)
	(scene-live 0xd0ff60c0 (* f_width 0.45) live-horn)
	(scene-live 0xd060b0ff (* f_width 0.42) live-ring)
	(scene-live 0xd0ffe040 (* f_width 0.60) live-ribbon)))

(defun scene-place (paths rot scale tx ty)
	;a shape turned, sized and moved to where it is in the scene
	(defq sa (* scale (sin rot)) ca (* scale (cos rot))
		m (fixeds ca (* sa -1.0) tx sa ca ty))
	(map (# (path-transform m %0 (cat %0))) paths))

(defun scene-draw (canvas angle count y y1)
	; (scene-draw canvas angle count y y1) -> drawn
	;the rows of the scene from y up to y1, how many of the shapes were
	;drawn is the result. A shape that does not reach the rows is not
	;turned, placed or drawn, nor stroked if it is a live one, and the clip
	;of the canvas cuts those that do to the rows.
	(defq fy (n2f y) fy1 (n2f y1) drawn 0
		cx (const (* (n2f +scene_width) 0.5)) cy (const (* (n2f +scene_height) 0.5))
		ax (* cx 0.92) ay (* cy 0.90))
	(.-> canvas (:set_clip 0 y +scene_width y1)
		(:set_color 0xff141c28) (:fbox 0 y +scene_width (- y1 y)))
	(each (lambda (i)
		(bind '(winding body body_col core core_col radius live)
			(elem-get scene_kinds (% i (length scene_kinds))))
		;every shape has a way of its own, from its number
		;the broad phase, how far down it is and how far it reaches is all
		;that is worked out for a shape that is not in these rows
		(defq fi (n2f i)
			scale (+ 0.20 (* 0.09 (sin (+ (* angle 2.3) (* fi 0.91)))))
			ty (+ cy (* ay (cos (+ (* angle (+ 0.40 (* 0.29 (n2f (% i 7))))) (* fi 3.883)))))
			reach (+ (* radius scale) 8.0))
		(when (and (> (+ ty reach) fy) (< (- ty reach) fy1))
			(defq tx (+ cx (* ax (sin (+ (* angle (+ 0.50 (* 0.37 (n2f (% i 5))))) (* fi 2.399)))))
				rot (* angle (if (= (logand i 1) 0) 1.0 -1.3) (+ 1.0 (* 0.4 (n2f (% i 3))))))
			;only now is a live shape bent and stroked
			(if live (setq body (live angle fi)))
			(.-> canvas (:set_color 0x48000000)
				(:fpoly 0.0 0.0 winding (scene-place body rot scale (+ tx 5.0) (+ ty 5.0))))
			(.-> canvas (:set_color body_col)
				(:fpoly 0.0 0.0 winding (scene-place body rot scale tx ty)))
			(when core
				(.-> canvas (:set_color core_col)
					(:fpoly 0.0 0.0 winding (scene-place core rot scale tx ty))))
			(setq drawn (inc drawn)))) (range 0 count))
	drawn)
