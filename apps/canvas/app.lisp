;import canvas and points method slots
(defq slot_set_fbox nil slot_set_fpoly nil slot_blend_fpoly nil slot_fill nil slot_swap nil
	slot_gen_bezier nil slot_gen_arc nil slot_stroke_polylines nil slot_stroke_polygons nil)
(within-compile-env (lambda ()
	(import 'class/canvas/canvas.inc)
	(import 'class/points/points.inc)
	(setq slot_set_fbox (method-slot 'canvas 'set_fbox)
		slot_set_fpoly (method-slot 'canvas 'set_fpoly)
		slot_blend_fpoly (method-slot 'canvas 'blend_fpoly)
		slot_fill (method-slot 'canvas 'fill)
		slot_swap (method-slot 'canvas 'swap)
		slot_stroke_polylines (method-slot 'points 'stroke_polylines)
		slot_stroke_polygons (method-slot 'points 'stroke_polygons)
		slot_gen_bezier (method-slot 'points 'gen_bezier)
		slot_gen_arc (method-slot 'points 'gen_arc))))

;math tools
(run 'apps/canvas/math.lisp)

(defq canvas_scale (pop argv) canvas_height (pop argv) canvas_width (pop argv) canvas (pop argv)
	stack (array))

(defun as-point (_)
	(bit-or (bit-shl (elem 1 _) 32) (bit-and 0xffffffff (elem 0 _))))

(defun as-points (_)
	(defq polygon (points))
	(each (lambda (_)
		(push polygon (as-point _))) _) polygon)

(defun scale-points (_)
	(defq polygon (points))
	(each (lambda (_)
		(push polygon (bit-or (bit-shl (fmul (bit-asr _ 32) canvas_scale) 32)
			(bit-and 0xffffffff (fmul (bit-asr (bit-shl _ 32) 32) canvas_scale))))) _) polygon)

(defun fpoly (col mode _)
	(call slot_set_fpoly canvas (map scale-points _) col mode))

(defun bpoly (col mode _)
	(call slot_blend_fpoly canvas (map scale-points _) col mode))

(call slot_fill canvas 0x00000000)
(call slot_set_fbox canvas 0xffffffff
	(div (fmul canvas_width canvas_scale 0.1) 1.0) (div (fmul canvas_height canvas_scale 0.05) 1.0)
	(div (fmul canvas_width canvas_scale 0.5) 1.0) (div (fmul canvas_height canvas_scale 0.5) 1.0))

(fpoly 0xff0000ff 0 (list (as-points (list
	(list 0 0)
	(list (fmul canvas_width 0.25) canvas_height)
	(list (fmul canvas_width 0.5) 0)
	(list (add (fmul canvas_width 0.5) (fmul canvas_width 0.25)) canvas_height)
	(list canvas_width 0)
	(list (fmul canvas_width 0x0.1) canvas_height)))))

(bpoly 0xc000ff00 1
	(call slot_stroke_polylines (list) stack
		(list (as-points (list
			(list (fmul canvas_width 0o0.1) (fmul canvas_height 0o0.1))
			(list (sub canvas_width (fmul canvas_width 0.25)) (fmul canvas_height 0.166))
			(list (sub canvas_width (fmul canvas_width 0o0.1)) (sub canvas_height (fmul canvas_height 0o0.1))))))
		join-round
		cap-round
		cap-round
		(fmul canvas_width 0x0.1)
		2.0))

(fpoly 0xff00ffff 1
	(call slot_stroke_polygons (list) stack
		(call slot_stroke_polylines (list) stack
			(list (call slot_gen_bezier (points) stack
				(as-point (list (fmul canvas_width 0x0.1) (sub canvas_height (fmul canvas_height 0x0.1))))
				(as-point (list (fmul canvas_width 0o0.1) (fmul canvas_height 0x0.1)))
				(as-point (list (fmul canvas_width 0.25) (fmul canvas_height 0.33)))
				(as-point (list (sub canvas_width (fmul canvas_width 0.1)) (fmul canvas_height 0.1)))
				2.0))
			join-bevel
			cap-round
			cap-arrow
			(fmul canvas_width 0.033)
			2.0)
		join-miter
		(fmul canvas_width 0.01)
		2.0))

(bpoly 0xc0ff0000 0
	(call slot_stroke_polygons (list) stack
		(call slot_stroke_polylines (list) stack
			(list
				(call slot_gen_arc (points) stack
					(as-point (list
						(add (fmul canvas_width 0.33) (fmul canvas_width 0x0.1))
						(add (fmul canvas_height 0.5) (fmul canvas_height 0o0.1))))
					1.0 1.0 (fmul canvas_width 0.25) 2.0)
				(call slot_gen_arc (points) stack
					(as-point (list
						(add (fmul canvas_width 0.33) (fmul canvas_width 0x0.1))
						(add (fmul canvas_height 0.5) (fmul canvas_height 0o0.1))))
					4.0 2.0 (fmul canvas_width 0o0.1) 2.0))
			join-bevel
			cap-square
			cap-tri
			(fmul canvas_width 0.05)
			2.0)
		join-miter
		(fmul canvas_width 0.025)
		2.0))

(call slot_swap canvas)
