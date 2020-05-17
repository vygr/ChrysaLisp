;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/math.inc)

;read args from parent
(bind '(canvas canvas_width canvas_height canvas_scale) (mail-read (task-mailbox)))

(defq eps 0.25 angle 0.0 font (create-font "fonts/OpenSans-Regular.ctf" 36)
	fp1 (font-glyph-paths font "__Glyphs!")
	fp2 (font-glyph-paths font "__Easy!")
	fp3 (font-glyph-paths font "__Simple!")
	fp4 (font-glyph-paths font "__Quality!"))

(defun-bind transform-copy (angle _)
	(defq sa (sin angle) ca (cos angle))
	(map (lambda (_)
		(path-transform
			(* canvas_scale ca) (* canvas_scale (* sa -1.0))
			(* canvas_scale sa) (* canvas_scale ca)
			(* canvas_width canvas_scale 0.5) (* canvas_height canvas_scale 0.5)
			_ (cat _))) _))

(defun-bind transform (angle _)
	(defq sa (sin angle) ca (cos angle))
	(map (lambda (_)
		(path-transform
			(* canvas_scale ca) (* canvas_scale (* sa -1.0))
			(* canvas_scale sa) (* canvas_scale ca)
			(* canvas_width canvas_scale 0.5) (* canvas_height canvas_scale 0.5)
			_ _)) _))

(defun-bind transform-norm (angle _)
	(defq sa (sin angle) ca (cos angle))
	(map (lambda (_)
		(path-transform
			(* canvas_width canvas_scale ca) (* canvas_height canvas_scale (* sa -1.0))
			(* canvas_width canvas_scale sa) (* canvas_height canvas_scale ca)
			(* canvas_width canvas_scale 0.5) (* canvas_height canvas_scale 0.5)
			_ _)) _))

(defun-bind fpoly (col mode _)
	(canvas-set-color canvas col)
	(canvas-fpoly canvas 0.0 0.0 mode _))

(defun-bind redraw ()
	(canvas-fill canvas 0)

	(fpoly argb_red 0 (transform-norm (* angle (i2f 2)) (list
		(path -0.5 -0.5 -0.25 0.5 0.0 -0.5 0.25 0.5 0.5 -0.5 -0.05 0.5))))

	(fpoly 0xff0ff0ff 0 (transform (* angle -1.0)
		(path-stroke-polylines (list) (* canvas_width 0.05) eps join_bevel cap_square cap_square
			(list (path-gen-quadratic
				(* canvas_width -0.4) (* canvas_height 0.4)
				(* canvas_width -0.2) (* canvas_height -1.1)
				(* canvas_width 0.4) (* canvas_height 0.2)
				eps (path))))))

	(fpoly 0xc000ff00 0 (transform angle
		(path-stroke-polylines (list) (* canvas_width 0x0.1) eps join_round cap_round cap_round
			(list (path (* canvas_width -0.4) (* canvas_height -0.4)
				(* canvas_width 0.3) (* canvas_height -0.3)
				(* canvas_width 0.4) (* canvas_height 0.4))))))

	(fpoly argb_yellow 0 (defq p (transform (* angle (i2f -2))
		(path-stroke-polygons (list) (* canvas_width 0.011) eps join_miter
			(path-stroke-polylines (list) (* canvas_width 0.033) eps join_bevel cap_round cap_arrow
				(list (path-gen-cubic
					(* canvas_width -0.45) (* canvas_height 0.3)
					(* canvas_width -0.3) (* canvas_height -0.3)
					(* canvas_width 0.45) (* canvas_height 0.6)
					(* canvas_width 0.4) (* canvas_height -0.4)
					eps (path))))))))
	(fpoly 0x80000000 0 (slice 1 2 p))

	(fpoly 0xd0ff00ff 0 (defq p (transform angle
		(path-stroke-polygons (list) (* canvas_width 0.02) eps join_miter
			(list (path-gen-arc
				(* canvas_width 0.2) (* canvas_height 0.3) 0.0 (const fp_2pi)
				(* canvas_width 0.125) eps (path)))))))
	(fpoly 0x60000000 0 (slice 0 1 p))

	(fpoly 0xc00000ff 0 (defq polygons (transform angle
		(path-stroke-polygons (list) (* canvas_width 0.025) eps join_miter
			(path-stroke-polylines (list) (* canvas_width 0.05) eps join_bevel cap_square cap_tri (list
				(path-gen-arc
					(* canvas_width -0.1) (* canvas_height -0.2) 0.9 1.5
					(* canvas_width 0.2) eps (path))
				(path-gen-arc
					(* canvas_width -0.2) (* canvas_height -0.2) 4.0 2.0
					(* canvas_width 0o0.1) eps (path))))))))
	(fpoly 0xa0ffffff 0 (list (elem 1 polygons) (elem 3 polygons)))

	(fpoly 0xff000000 0 (transform-copy angle fp1))
	(fpoly 0xff000000 0 (transform-copy (+ angle (const fp_pi)) fp2))
	(fpoly 0xffffffff 0 (transform-copy (+ angle (const fp_hpi)) fp3))
	(fpoly 0xffffffff 0 (transform-copy (+ angle (* -1.0 (const fp_hpi))) fp4))

	(canvas-swap canvas))

(defun-bind main ()
	;until quit
	(until (mail-poll (array (task-mailbox)))
		(redraw)
		(task-sleep 10000)
		(setq angle (+ angle 0.0025))))
