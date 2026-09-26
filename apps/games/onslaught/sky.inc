;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; onslaught sky vertical gradient view
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +sky_palette
	''(0xff7fbf7f   ; green pastel
	  0xff7f7fbf   ; blue pastel
	  0xff7fbfbf   ; cyan pastel
	  0xffbf7fbf   ; magenta pastel
	  0xffbfbf7f   ; yellow pastel
	  0xffbf7f7f)) ; red pastel

(defclass Sky () (View)
	; (Sky) -> sky view
	(def this
		:color +argb_black
		:color_top +argb_black
		:color_bottom +argb_black
		:scroll_y 0
		:step 2)
	(. this :set_size (* +map_width +tile_width) (* +map_height +tile_height))

	(defmethod :set_cols (c1 c2)
		; (. sky :set_cols c1 c2) -> sky
		(def this :color c1 :color_top c1 :color_bottom c2)
		this)

	(defmethod :set_random_sky ()
		; pick one of the 6 field pastel colors to white gradient
		(defq top (elem-get +sky_palette (random (length +sky_palette))))
		(. this :set_cols top +argb_white))

	(defmethod :set_black ()
		(. this :set_cols +argb_black +argb_black))

	(defmethod :set_scroll_y (sy)
		(def this :scroll_y sy)
		this)

	(defmethod :draw ()
		; (. sky :draw) -> sky
		(bind '(w h) (. this :get_size))
		(when (and (> w 0) (> h 0))
			(defq c1 (or (get :color_top this) +argb_black)
				c2 (or (get :color_bottom this) +argb_white))
			(if (= c1 c2)
				(.-> this
					(:ctx_set_color c1)
					(:ctx_filled_box 0 0 w h))
				(defq
					step (max 1 (or (get :step this) 2))
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
						r (+ r1 (/ (* dr y) h))
						g (+ g1 (/ (* dg y) h))
						b (+ b1 (/ (* db y) h))
						col (logior 0xff000000 (<< r 16) (<< g 8) b))
					(. this :ctx_set_color col)
					(. this :ctx_filled_box 0 y w step)
					(setq y (+ y step)))))
		this)
	)

(defmacro ui-sky (n &optional p &rest x)
	; (ui-sky name [props] [body]) -> sky
	(ui-props p
		:color +argb_black)
	(static-qqp (ui-element ,n (Sky) ,p ~x)))