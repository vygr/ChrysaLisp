;import canvas class method slots
(defq canvas-set-fbox nil canvas-set-fpoly nil canvas-fill nil canvas-swap nil)
(within-compile-env (lambda ()
	(import 'class/canvas/canvas.inc)
	(setq canvas-set-fbox (method-slot 'canvas 'set_fbox)
		canvas-set-fpoly (method-slot 'canvas 'set_fpoly)
		canvas-fill (method-slot 'canvas 'fill)
		canvas-swap (method-slot 'canvas 'swap))))

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
	(call canvas-set-fpoly canvas polygons col m))

(call canvas-fill canvas 0x00000000)
(call canvas-set-fbox canvas 0xffffffff
	(div (fmul canvas_width canvas_scale 0.1) 1.0) (div (fmul canvas_height canvas_scale 0.05) 1.0)
	(div (fmul canvas_width canvas_scale 0.5) 1.0) (div (fmul canvas_height canvas_scale 0.5) 1.0))

(fpoly 0xff0000ff 0 (list (list
	(list 0 0)
	(list (fmul canvas_width 0.25) canvas_height)
	(list (fmul canvas_width 0.5) 0)
	(list (add (fmul canvas_width 0.5) (fmul canvas_width 0.25)) canvas_height)
	(list canvas_width 0)
	(list (fmul canvas_width 0x0.1) canvas_height))))

(fpoly 0xff00ff00 0
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
			(list (gen-bezier-polyline-2d
				(list)
				(list (fmul canvas_width 0x0.1) (sub canvas_height (fmul canvas_height 0x0.1)))
				(list (fmul canvas_width 0o0.1) (fmul canvas_height 0x0.1))
				(list (fmul canvas_width 0.25) (fmul canvas_height 0.33))
				(list (sub canvas_width (fmul canvas_width 0.1)) (fmul canvas_height 0.1)))))))

(fpoly 0xffff0000 0
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
				(gen-arc-polyline-2d
					(list)
					(list (add (fmul canvas_width 0.33) (fmul canvas_width 0x0.1))
						(add (fmul canvas_height 0.5) (fmul canvas_height 0o0.1)))
					(fmul canvas_width 0.25)
					1.0
					1.0)
				(gen-arc-polyline-2d
					(list)
					(list (add (fmul canvas_width 0.33) (fmul canvas_width 0x0.1))
						(add (fmul canvas_height 0.5) (fmul canvas_height 0o0.1)))
					(fmul canvas_width 0o0.1)
					4.0
					2.0)))))

(call canvas-swap canvas)
