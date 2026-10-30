;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; What the scenes of the Motion demo share. stage is the Rostrum of the moment,
; and restart says the scene is over and to begin again.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/motion/rostrum.inc")

(defq +max_figures 500 +font "fonts/OpenSans-Bold.ctf"
	;the outline of a spark, they are all alike
	*spark_outline* (list (list :t (path -4.0 -2.0 4.0 -2.0 0.0 5.0))))

(defun random-fixed (from to)
	;a number between two others
	(+ from (* (- to from) (/ (n2f (random 1000)) 1000.0))))

(defun spark-at (x y colour)
	;a bit that flies off, spins, fades and goes. Only so many at once
	(when (< (length (. stage :children)) +max_figures)
		(defq fig (Figure "spark"))
		(. fig :set_outline *spark_outline*)
		(def fig :fig_fill colour :fig_x x :fig_y y :fig_z 0.0)
		(. fig :add (Rate :fig_x (random-fixed -160.0 160.0)) (Rate :fig_y (random-fixed -160.0 160.0) 200.0)
			(Rate :fig_rz (random-fixed -12.0 12.0))
			(Tween :fig_alpha 0.0 (random-fixed 0.3 0.8) (const ease-in)
				(lambda (f c) (. f :kill))))
		(. stage :add_node fig)))
