;import canvas class method slots
(defq slot_set_fbox nil slot_set_fpoly nil slot_blend_fpoly nil slot_fill nil slot_swap nil
	slot_gen_bezier nil slot_gen_arc nil)
(within-compile-env (lambda ()
	(import 'class/canvas/canvas.inc)
	(import 'class/points/points.inc)
	(setq slot_set_fbox (method-slot 'canvas 'set_fbox)
		slot_set_fpoly (method-slot 'canvas 'set_fpoly)
		slot_blend_fpoly (method-slot 'canvas 'blend_fpoly)
		slot_fill (method-slot 'canvas 'fill)
		slot_swap (method-slot 'canvas 'swap)
		slot_gen_bezier (method-slot 'points 'gen_bezier)
		slot_gen_arc (method-slot 'points 'gen_arc))))

;math tools
(run 'apps/canvas/math.lisp)

(defq canvas_scale (pop argv) canvas_height (pop argv) canvas_width (pop argv) canvas (pop argv))

(defun fpoly (col m _)
	(defq polygons (list))
	(each (lambda (_)
		(defq polygon (array))
		(each (lambda (_)
			(setq _ (vec-scale-2d _ canvas_scale))
			(push polygon (bit-or (bit-shl (elem 1 _) 32) (bit-and 0xffffffff (elem 0 _))))) _)
		(push polygons polygon)) _)
	(call slot_set_fpoly canvas polygons col m))

(defun bpoly (col m _)
	(defq polygons (list))
	(each (lambda (_)
		(defq polygon (array))
		(each (lambda (_)
			(setq _ (vec-scale-2d _ canvas_scale))
			(push polygon (bit-or (bit-shl (elem 1 _) 32) (bit-and 0xffffffff (elem 0 _))))) _)
		(push polygons polygon)) _)
	(call slot_blend_fpoly canvas polygons col m))

(call slot_fill canvas 0x00000000)
(call slot_set_fbox canvas 0xffffffff
	(div (fmul canvas_width canvas_scale 0.1) 1.0) (div (fmul canvas_height canvas_scale 0.05) 1.0)
	(div (fmul canvas_width canvas_scale 0.5) 1.0) (div (fmul canvas_height canvas_scale 0.5) 1.0))

(fpoly 0xff0000ff 0 (list (list
	(list 0 0)
	(list (fmul canvas_width 0.25) canvas_height)
	(list (fmul canvas_width 0.5) 0)
	(list (add (fmul canvas_width 0.5) (fmul canvas_width 0.25)) canvas_height)
	(list canvas_width 0)
	(list (fmul canvas_width 0x0.1) canvas_height))))

(bpoly 0xc000ff00 0
	(stroke-polyline-2d
		(list)
		(fmul canvas_width 0x0.1)
		join-round
		cap-round
		cap-round
		(list (list
			(list (fmul canvas_width 0o0.1) (fmul canvas_height 0o0.1))
			(list (sub canvas_width (fmul canvas_width 0.25)) (fmul canvas_height 0.166))
			(list (sub canvas_width (fmul canvas_width 0o0.1)) (sub canvas_height (fmul canvas_height 0o0.1)))))))

(fpoly 0xff00ffff 1
	(stroke-polygon-2d
		(list)
		(fmul canvas_width 0.01)
		join-miter
		(stroke-polyline-2d
			(list)
			(fmul canvas_width 0.033)
			join-bevel
			cap-round
			cap-arrow
			(list (map (lambda (_)
				(list (bit-asr (bit-shl _ 32) 32) (bit-asr _ 32)))
				(call slot_gen_bezier (points) (array)
					(fmul canvas_width 0x0.1) (sub canvas_height (fmul canvas_height 0x0.1))
					(fmul canvas_width 0o0.1) (fmul canvas_height 0x0.1)
					(fmul canvas_width 0.25) (fmul canvas_height 0.33)
					(sub canvas_width (fmul canvas_width 0.1)) (fmul canvas_height 0.1)
					2.0))))))

(bpoly 0xc0ff0000 0
	(stroke-polygon-2d
		(list)
		(fmul canvas_width 0.025)
		join-miter
		(stroke-polyline-2d
			(list)
			(fmul canvas_width 0.05)
			join-bevel
			cap-square
			cap-tri
			(list
				(map (lambda (_)
					(list (bit-asr (bit-shl _ 32) 32) (bit-asr _ 32)))
					(call slot_gen_arc (points) (array)
						(add (fmul canvas_width 0.33) (fmul canvas_width 0x0.1))
						(add (fmul canvas_height 0.5) (fmul canvas_height 0o0.1))
						1.0 1.0 (fmul canvas_width 0.25) 2.0))
				(map (lambda (_)
					(list (bit-asr (bit-shl _ 32) 32) (bit-asr _ 32)))
					(call slot_gen_arc (points) (array)
						(add (fmul canvas_width 0.33) (fmul canvas_width 0x0.1))
						(add (fmul canvas_height 0.5) (fmul canvas_height 0o0.1))
						4.0 2.0 (fmul canvas_width 0o0.1) 2.0))))))

(call slot_swap canvas)
