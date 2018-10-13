;import settings
(run 'sys/lisp.inc)
(run 'gui/lisp.inc)

;math tools
(run 'apps/math.inc)

;read args from parent
(bind '(canvas canvas_width canvas_height canvas_scale) (mail-mymail))

(defq stack (array) eps 0.25 angle 0.0)

(defun transform (_ angle)
	(defq sa (fsin angle) ca (fcos angle))
	(map (lambda (_)
		(points-transform _ _
			(fmul canvas_scale ca) (fmul canvas_scale (neg sa))
			(fmul canvas_scale sa) (fmul canvas_scale ca)
			(fmul canvas_width canvas_scale 0.5) (fmul canvas_height canvas_scale 0.5))) _))

(defun transform-norm (_ angle)
	(defq sa (fsin angle) ca (fcos angle))
	(map (lambda (_)
		(points-transform _ _
			(fmul canvas_width canvas_scale ca) (fmul canvas_height canvas_scale (neg sa))
			(fmul canvas_width canvas_scale sa) (fmul canvas_height canvas_scale ca)
			(fmul canvas_width canvas_scale 0.5) (fmul canvas_height canvas_scale 0.5))) _))

(defun fpoly (col mode _)
	(canvas-set-fpoly canvas _ col mode))

(defun bpoly (col mode _)
	(canvas-blend-fpoly canvas _ col mode))

(defun redraw ()
	(canvas-fill canvas 0)

	(fpoly argb_red 1 (transform-norm (list
		(points -0.5 -0.5 -0.25 0.5 0 -0.5 0.25 0.5 0.5 -0.5 -0.05 0.5)) (mul angle 2)))

	(fpoly 0xff0ff0ff 0 (transform
		(points-stroke-polylines (list) stack
			(list (points-gen-quadratic (points) stack
				(fmul canvas_width -0.4) (fmul canvas_height 0.4)
				(fmul canvas_width -0.2) (fmul canvas_height -1.1)
				(fmul canvas_width 0.4) (fmul canvas_height 0.2)
				eps))
			join-bevel
			cap-square
			cap-square
			(fmul canvas_width 0.05)
			eps) (neg angle)))

	(bpoly 0xc000ff00 1 (transform
		(points-stroke-polylines (list) stack
			(list (points (fmul canvas_width -0.4) (fmul canvas_height -0.4)
				(fmul canvas_width 0.3) (fmul canvas_height -0.3)
				(fmul canvas_width 0.4) (fmul canvas_height 0.4)))
			join-round
			cap-round
			cap-round
			(fmul canvas_width 0x0.1)
			eps) angle))

	(fpoly argb_yellow 0 (defq p (transform
		(points-stroke-polygons (list) stack
			(points-stroke-polylines (list) stack
				(list (points-gen-cubic (points) stack
					(fmul canvas_width -0.45) (fmul canvas_height 0.3)
					(fmul canvas_width -0.3) (fmul canvas_height -0.3)
					(fmul canvas_width 0.45) (fmul canvas_height 0.6)
					(fmul canvas_width 0.4) (fmul canvas_height -0.4)
					eps))
				join-bevel
				cap-round
				cap-arrow
				(fmul canvas_width 0.033)
				eps)
			join-miter
			(fmul canvas_width 0.011)
			eps) (mul angle -2))))
	(bpoly 0x80000000 0 (slice 1 2 p))

	(bpoly 0xd0ff00ff 0 (defq p (transform
		(points-stroke-polygons (list) stack
			(list (points-gen-arc (points) stack
				(fmul canvas_width 0.2) (fmul canvas_height 0.3)
				0 fp_2pi
				(fmul canvas_width 0.125) eps))
			join-miter
			(fmul canvas_width 0.02)
			eps) angle)))
	(bpoly 0x60000000 0 (slice 0 1 p))

	(bpoly 0xc00000ff 0 (defq polygons (transform
		(points-stroke-polygons (list) stack
			(points-stroke-polylines (list) stack
				(list
					(points-gen-arc (points) stack
						(fmul canvas_width -0.1) (fmul canvas_height -0.2)
						0.9 1.5
						(fmul canvas_width 0.2) eps)
					(points-gen-arc (points) stack
						(fmul canvas_width -0.2) (fmul canvas_height -0.2)
						4.0 2.0
						(fmul canvas_width 0o0.1) eps))
				join-bevel
				cap-square
				cap-tri
				(fmul canvas_width 0.05)
				eps)
			join-miter
			(fmul canvas_width 0.025)
			eps) angle)))
	(bpoly 0xa0ffffff 0
		(list (elem 1 polygons) (elem 3 polygons)))

	(canvas-swap canvas))

(while (lt angle fp_2pi)
	(redraw)
	(setq angle (add angle 0.0025)))
