;import canvas class method slots
(defq canvas-set-pixel nil canvas-set-fbox nil canvas-set-hline nil canvas-fill nil canvas-swap nil)
(within-compile-env (lambda ()
	(import 'class/canvas/canvas.inc)
	(setq canvas-set-pixel (method-slot 'canvas 'set_pixel)
		canvas-set-hline (method-slot 'canvas 'set_hline)
		canvas-set-fbox (method-slot 'canvas 'set_fbox)
		canvas-fill (method-slot 'canvas 'fill)
		canvas-swap (method-slot 'canvas 'swap))))

;math tools
(run 'apps/canvas/math.lisp)

(defq canvas_scale (pop argv) canvas_height (pop argv) canvas_width (pop argv) canvas (pop argv)
	pen-col 0 brush-col 0)

(call canvas-swap canvas)
