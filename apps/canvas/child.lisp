;import ui settings
(run 'apps/ui.lisp)

;math tools
(run 'apps/math.lisp)

;read args from parent
(bind '(canvas canvas_width canvas_height canvas_scale) (slot mail_mymail nil))

(defq stack (array) eps 1.0 angle 0.0)

(defun as-point ((x y))
	(bit-or (bit-and 0xffffffff x) (bit-shl y 32)))

(defun as-points (_)
	(apply points (map as-point _)))

(defun transform (_ angle)
	(defq sa (fsin angle) ca (fcos angle))
	(map (lambda (_)
		(slot transform _ _
			(as-point (list (fmul canvas_scale ca) (fmul canvas_scale (neg sa))))
			(as-point (list (fmul canvas_scale sa) (fmul canvas_scale ca)))
			(as-point (list (fmul canvas_width canvas_scale 0.5) (fmul canvas_height canvas_scale 0.5))))) _))

(defun transform-norm (_ angle)
	(defq sa (fsin angle) ca (fcos angle))
	(map (lambda (_)
		(slot transform _ _
			(as-point (list (fmul canvas_width canvas_scale ca) (fmul canvas_height canvas_scale (neg sa))))
			(as-point (list (fmul canvas_width canvas_scale sa) (fmul canvas_height canvas_scale ca)))
			(as-point (list (fmul canvas_width canvas_scale 0.5) (fmul canvas_height canvas_scale 0.5))))) _))

(defun fpoly (col mode _)
	(slot set_fpoly canvas _ col mode))

(defun bpoly (col mode _)
	(slot blend_fpoly canvas _ col mode))

(defun redraw ()
	(slot fill canvas 0x000000ff)

	(fpoly 0xff0000ff 1 (transform-norm (list (as-points (list
		(list -0.5 -0.5)
		(list -0.25 0.5)
		(list 0 -0.5)
		(list 0.25 0.5)
		(list 0.5 -0.5)
		(list -0.05 0.5)))) (mul angle 2)))

	(fpoly 0xfffff00f 0 (transform
		(slot stroke_polylines (list) stack
			(list
				(slot gen_quadratic (points) stack
					(as-point (list (fmul canvas_width -0.4) (fmul canvas_height 0.4)))
					(as-point (list (fmul canvas_width -0.2) (fmul canvas_height -1.1)))
					(as-point (list (fmul canvas_width 0.4) (fmul canvas_height 0.2)))
					eps))
			join-bevel
			cap-square
			cap-square
			(fmul canvas_width 0.05)
			eps) (neg angle)))

	(bpoly 0xc000ff00 1 (transform
		(slot stroke_polylines (list) stack
			(list
				(as-points (list
					(list (fmul canvas_width -0.4) (fmul canvas_height -0.4))
					(list (fmul canvas_width 0.3) (fmul canvas_height -0.3))
					(list (fmul canvas_width 0.4) (fmul canvas_height 0.4)))))
			join-round
			cap-round
			cap-round
			(fmul canvas_width 0x0.1)
			eps) angle))

	(fpoly 0xff00ffff 0 (defq p (transform
		(slot stroke_polygons (list) stack
			(slot stroke_polylines (list) stack
				(list
					(slot gen_cubic (points) stack
						(as-point (list (fmul canvas_width -0.45) (fmul canvas_height 0.3)))
						(as-point (list (fmul canvas_width -0.3) (fmul canvas_height -0.3)))
						(as-point (list (fmul canvas_width 0.45) (fmul canvas_height 0.6)))
						(as-point (list (fmul canvas_width 0.4) (fmul canvas_height -0.4)))
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
		(slot stroke_polygons (list) stack
			(list
				(slot gen_arc (points) stack
					(as-point (list (fmul canvas_width 0.2) (fmul canvas_height 0.3)))
					0.0
					fp_2pi
					(fmul canvas_width 0.125)
					eps))
			join-miter
			(fmul canvas_width 0.02)
			eps) angle)))
	(bpoly 0x60000000 0 (slice 0 1 p))

	(bpoly 0xc0ff0000 0 (defq polygons (transform
		(slot stroke_polygons (list) stack
			(slot stroke_polylines (list) stack
				(list
					(slot gen_arc (points) stack
						(as-point (list (fmul canvas_width -0.1) (fmul canvas_height -0.2)))
						0.9
						1.5
						(fmul canvas_width 0.2)
						eps)
					(slot gen_arc (points) stack
						(as-point (list (fmul canvas_width -0.2) (fmul canvas_height -0.2)))
						4.0
						2.0
						(fmul canvas_width 0o0.1)
						eps))
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

	(slot swap canvas))

(while (lt angle fp_2pi)
	(redraw)
	(setq angle (add angle 0.02)))
