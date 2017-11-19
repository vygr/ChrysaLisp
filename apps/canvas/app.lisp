;import canvas and points method slots
(bind '(slot_set_fbox slot_set_fpoly slot_blend_fpoly slot_fill slot_swap slot_transform slot_simplify
	slot_gen_quadratic slot_gen_cubic slot_gen_arc slot_stroke_polylines slot_stroke_polygons)
	(within-compile-env (lambda ()
		(import 'class/canvas/canvas.inc)
		(import 'class/points/points.inc)
		(list (method-slot 'canvas 'set_fbox)
			(method-slot 'canvas 'set_fpoly)
			(method-slot 'canvas 'blend_fpoly)
			(method-slot 'canvas 'fill)
			(method-slot 'canvas 'swap)
			(method-slot 'points 'transform)
			(method-slot 'points 'simplify)
			(method-slot 'points 'gen_quadratic)
			(method-slot 'points 'gen_cubic)
			(method-slot 'points 'gen_arc)
			(method-slot 'points 'stroke_polylines)
			(method-slot 'points 'stroke_polygons)))))

;math tools
(run 'apps/canvas/math.lisp)

(bind '(canvas canvas_width canvas_height canvas_scale) argv)
(defq stack (array) eps 1.0 angle 0.0)

(defun as-point ((x y))
	(bit-or (bit-and 0xffffffff x) (bit-shl y 32)))

(defun as-points (_)
	(apply points (map as-point _)))

(defun transform (_)
	(defq sa (fsin angle) ca (fcos angle))
	(map (lambda (_)
		(call slot_transform _ _
			(as-point (list (fmul canvas_scale ca) (fmul canvas_scale (neg sa))))
			(as-point (list (fmul canvas_scale sa) (fmul canvas_scale ca)))
			(as-point (list (fmul canvas_width canvas_scale 0.5) (fmul canvas_height canvas_scale 0.5))))) _))

(defun transform-norm (_)
	(defq sa (fsin angle) ca (fcos angle))
	(map (lambda (_)
		(call slot_transform _ _
			(as-point (list (fmul canvas_width canvas_scale ca) (fmul canvas_height canvas_scale (neg sa))))
			(as-point (list (fmul canvas_width canvas_scale sa) (fmul canvas_height canvas_scale ca)))
			(as-point (list (fmul canvas_width canvas_scale 0.5) (fmul canvas_height canvas_scale 0.5))))) _))

(defun fpoly (col mode _)
	(call slot_set_fpoly canvas _ col mode))

(defun bpoly (col mode _)
	(call slot_blend_fpoly canvas _ col mode))

(defun redraw ()
	(call slot_fill canvas 0x00000000)

	(fpoly 0xff0000ff 1 (transform-norm (list (as-points (list
		(list -0.5 -0.5)
		(list -0.25 0.5)
		(list 0 -0.5)
		(list 0.25 0.5)
		(list 0.5 -0.5)
		(list -0.05 0.5))))))

	(fpoly 0xfffff00f 0 (transform
		(call slot_stroke_polylines (list) stack
			(list
				(call slot_gen_quadratic (points) stack
					(as-point (list (fmul canvas_width -0.4) (fmul canvas_height 0.4)))
					(as-point (list (fmul canvas_width -0.2) (fmul canvas_height -1.1)))
					(as-point (list (fmul canvas_width 0.4) (fmul canvas_height 0.2)))
					eps))
			join-bevel
			cap-square
			cap-square
			(fmul canvas_width 0.05)
			eps)))

	(bpoly 0xc000ff00 1 (transform
		(call slot_stroke_polylines (list) stack
			(list
				(as-points (list
					(list (fmul canvas_width -0.4) (fmul canvas_height -0.4))
					(list (fmul canvas_width 0.3) (fmul canvas_height -0.3))
					(list (fmul canvas_width 0.4) (fmul canvas_height 0.4)))))
			join-round
			cap-round
			cap-round
			(fmul canvas_width 0x0.1)
			eps)))

	(fpoly 0xff00ffff 0 (defq p (transform
		(call slot_stroke_polygons (list) stack
			(call slot_stroke_polylines (list) stack
				(list
					(call slot_gen_cubic (points) stack
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
			eps))))
	(bpoly 0x80000000 0 (slice 1 2 p))

	(bpoly 0xd0ff00ff 0 (defq p (transform
		(call slot_stroke_polygons (list) stack
			(list
				(call slot_gen_arc (points) stack
					(as-point (list (fmul canvas_width 0.2) (fmul canvas_height 0.3)))
					0.0
					fp_2pi
					(fmul canvas_width 0.125)
					eps))
			join-miter
			(fmul canvas_width 0.02)
			eps))))
	(bpoly 0x60000000 0 (slice 0 1 p))

	(bpoly 0xc0ff0000 0 (defq polygons (transform
		(call slot_stroke_polygons (list) stack
			(call slot_stroke_polylines (list) stack
				(list
					(call slot_gen_arc (points) stack
						(as-point (list (fmul canvas_width -0.1) (fmul canvas_height -0.2)))
						0.9
						1.5
						(fmul canvas_width 0.2)
						eps)
					(call slot_gen_arc (points) stack
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
			eps))))
	(bpoly 0xa0ffffff 0
		(list (elem 1 polygons) (elem 3 polygons)))

	(call slot_swap canvas))

(while (lt angle fp_2pi)
	(redraw)
	(setq angle (add angle 0.025)))
