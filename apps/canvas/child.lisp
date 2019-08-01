;imports
(import 'sys/lisp.inc)
(import 'gui/lisp.inc)
(import 'apps/math.inc)

;read args from parent
(bind '(canvas canvas_width canvas_height canvas_scale) (mail-mymail))

(defq eps 0.25 angle 0.0)

(defun-bind transform (_ angle)
	(defq sa (fsin angle) ca (fcos angle))
	(map (lambda (_)
		(points-transform _ _
			(fmul canvas_scale ca) (fmul canvas_scale (neg sa))
			(fmul canvas_scale sa) (fmul canvas_scale ca)
			(fmul canvas_width canvas_scale 0.5) (fmul canvas_height canvas_scale 0.5))) _))

(defun-bind transform-norm (_ angle)
	(defq sa (fsin angle) ca (fcos angle))
	(map (lambda (_)
		(points-transform _ _
			(fmul canvas_width canvas_scale ca) (fmul canvas_height canvas_scale (neg sa))
			(fmul canvas_width canvas_scale sa) (fmul canvas_height canvas_scale ca)
			(fmul canvas_width canvas_scale 0.5) (fmul canvas_height canvas_scale 0.5))) _))

(defun-bind fpoly (col mode _)
	(canvas-set-color canvas col)
	(canvas-fpoly canvas 0 0 mode _))

(defun-bind redraw ()
	(canvas-fill canvas 0)

	(fpoly argb_red 0 (transform-norm (list
		(points -0.5 -0.5 -0.25 0.5 0 -0.5 0.25 0.5 0.5 -0.5 -0.05 0.5)) (mul angle 2)))

	(fpoly 0xff0ff0ff 0 (transform
		(points-stroke-polylines (fmul canvas_width 0.05) eps join-bevel cap-square cap-square
			(list (points-gen-quadratic
				(fmul canvas_width -0.4) (fmul canvas_height 0.4)
				(fmul canvas_width -0.2) (fmul canvas_height -1.1)
				(fmul canvas_width 0.4) (fmul canvas_height 0.2)
				eps (points)))
			(list)) (neg angle)))

	(fpoly 0xc000ff00 0 (transform
		(points-stroke-polylines (fmul canvas_width 0x0.1) eps join-round cap-round cap-round
			(list (points (fmul canvas_width -0.4) (fmul canvas_height -0.4)
				(fmul canvas_width 0.3) (fmul canvas_height -0.3)
				(fmul canvas_width 0.4) (fmul canvas_height 0.4)))
			(list)) angle))

	(fpoly argb_yellow 0 (defq p (transform
		(points-stroke-polygons (fmul canvas_width 0.011) eps join-miter
			(points-stroke-polylines (fmul canvas_width 0.033) eps join-bevel cap-round cap-arrow
				(list (points-gen-cubic
					(fmul canvas_width -0.45) (fmul canvas_height 0.3)
					(fmul canvas_width -0.3) (fmul canvas_height -0.3)
					(fmul canvas_width 0.45) (fmul canvas_height 0.6)
					(fmul canvas_width 0.4) (fmul canvas_height -0.4)
					eps (points)))
				(list)) (list)) (mul angle -2))))
	(fpoly 0x80000000 0 (slice 1 2 p))

	(fpoly 0xd0ff00ff 0 (defq p (transform
		(points-stroke-polygons (fmul canvas_width 0.02) eps join-miter
			(list (points-gen-arc
				(fmul canvas_width 0.2) (fmul canvas_height 0.3) 0 fp_2pi
				(fmul canvas_width 0.125) eps (points)))
			(list)) angle)))
	(fpoly 0x60000000 0 (slice 0 1 p))

	(fpoly 0xc00000ff 0 (defq polygons (transform
		(points-stroke-polygons (fmul canvas_width 0.025) eps join-miter
			(points-stroke-polylines (fmul canvas_width 0.05) eps join-bevel cap-square cap-tri
				(list
					(points-gen-arc
						(fmul canvas_width -0.1) (fmul canvas_height -0.2) 0.9 1.5
						(fmul canvas_width 0.2) eps (points))
					(points-gen-arc
						(fmul canvas_width -0.2) (fmul canvas_height -0.2) 4.0 2.0
						(fmul canvas_width 0o0.1) eps (points)))
				(list)) (list)) angle)))
	(fpoly 0xa0ffffff 0 (list (elem 1 polygons) (elem 3 polygons)))

	(canvas-swap canvas))

;until quit
(until (mail-trymail)
	(redraw)
	(task-sleep 10000)
	(setq angle (add angle 0.0025)))
