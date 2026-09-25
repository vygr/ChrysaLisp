;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/games/onslaught/sky.inc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught sky vertical gradient view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "gui/view/lisp.inc")
(import "./enums.inc")

(defq +sky_palette
	''(0xff7fbf7f   ; green pastel
	  0xff7f7fbf   ; blue pastel
	  0xff7fbfbf   ; cyan pastel
	  0xffbf7fbf   ; magenta pastel
	  0xffbfbf7f   ; yellow pastel
	  0xffbfbf7f)) ; red pastel

(defclass Sky () (View)
	; (Sky) -> sky view
	(def this
		:color 0
		:color_top 0
		:color_bottom 0
		:scroll_y 0
		:step 2)
	(. this :set_size (* *zoom* (* +map_width +tile_width)) (* *zoom* (* +map_height +tile_height)))

	(defmethod :layout ()
		; (. sky :layout) -> sky
		(defq c (get :color_top this))
		(if (and c (= (>> c 24) 0xff))
			(. this :set_flags +view_flag_opaque +view_flag_opaque)
			(. this :set_flags 0 +view_flag_opaque))
		this)

	(defmethod :set_cols (c1 c2)
		; (. sky :set_cols c1 c2) -> sky
		(def this :color c1 :color_top c1 :color_bottom c2)
		(. this :dirty)
		this)

	(defmethod :set_random_sky ()
		; pick one of the 6 field pastel colors to white gradient
		(defq top (elem-get +sky_palette (random 6)))
		(. this :set_cols top +argb_white))

	(defmethod :set_black ()
		(. this :set_cols +argb_black +argb_black))

	(defmethod :set_scroll_y (sy)
		(def this :scroll_y sy)
		this)

	(defmethod :draw ()
		; (. sky :draw) -> sky
		(bind '(w h) (. this :get_size))
		(defq c1 (get :color_top this)
			c2 (get :color_bottom this))
		(if (= c1 c2)
			(.-> this
				(:ctx_set_color c1)
				(:ctx_filled_box 0 0 w h))
			(progn
				(defq
					step (* *zoom* (get :step this))
					grad_h (* *zoom* (* +map_height +tile_height))
					r1 (logand (>> c1 16) 0xff)
					g1 (logand (>> c1 8) 0xff)
					b1 (logand c1 0xff)
					r2 (logand (>> c2 16) 0xff)
					g2 (logand (>> c2 8) 0xff)
					b2 (logand c2 0xff)
					dr (- r2 r1)
					dg (- g2 g1)
					db (- b2 b1)
					y 0)
				(while (< y h)
					(defq
						t (max 0 (min grad_h y))
						r (+ r1 (/ (* dr t) grad_h))
						g (+ g1 (/ (* dg t) grad_h))
						b (+ b1 (/ (* db t) grad_h))
						col (logior 0xff000000 (<< r 16) (<< g 8) b))
					(.-> this
						(:ctx_set_color col)
						(:ctx_filled_box 0 y w step))
					(setq y (+ y step)))))
		this)
	)

(defmacro ui-sky (n &optional p &rest x)
	; (ui-sky name [props] [body]) -> sky
	(ui-props p
		:color 0)
	(static-qqp (ui-element ,n (Sky) ,p ~x)))