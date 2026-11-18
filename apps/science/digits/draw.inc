;the charts, drawn on canvases: the loss as it trains and the tests, what
;the first hidden units have learnt to look for, and the confusion

(defq +chart_back 0xff12161c +chart_grid 0xff2a313c +chart_text 0xffc8d0dc
	+chart_loss 0xffffb040 +chart_valid 0xff40c060 +chart_test 0xffffffff +chart_steps 400)

(defun draw-words (canvas text x y size colour)
	; (draw-words canvas text x y size colour)
	;words with their left at x and their foot at y
	(.-> canvas (:set_color colour)
		(:fpoly (n2q x) (n2q y) +winding_none_zero
			(map (const second) (outline-text text +nn_view_font (* size 2))))))

(defun draw-line (canvas colour width points)
	; (draw-line canvas colour width (x y x y ...))
	(when (>= (length points) 4)
		(.-> canvas (:set_color colour)
			(:fpoly 0.0 0.0 +winding_none_zero
				(list (path-stroke-polyline (apply (const path) (map (const n2q) points))
					(n2q width) +join_round +cap_round +cap_round))))))

(defun draw-loss (canvas losses valids tests)
	; (draw-loss canvas losses valids tests)
	;the loss of each step, the last of them, the check of the kept back
	;part after each epoch, a line, and each test, a point, how many right
	;of 10000 on a scale of their own from 80% to 100%
	(bind '(w h) (. canvas :pref_size))
	(defq x0 44 y0 34 x1 (- w 48) y1 (- h 30) shown (slice losses (max 0 (- (length losses) +chart_steps)) -1)
		first_step (- (length losses) (length shown))
		top (max (n2d 0.5) (reduce (const max) shown (n2d 0))))
	(.-> canvas (:fill +chart_back) (:set_color +chart_grid))
	(each! (lambda () (defq y (+ y0 (/ (* (!) (- y1 y0)) 4))) (. canvas :fbox x0 y (- x1 x0) 1)) '() 0 5)
	(draw-words canvas "loss" 6 16 11 +chart_loss)
	(draw-words canvas (double-to-str top 2) 6 (+ y0 4) 10 +chart_text)
	(draw-words canvas "0" 30 (+ y1 4) 10 +chart_text)
	(draw-words canvas "valid" (- w 100) 16 11 +chart_valid)
	(draw-words canvas "test" (- w 40) 16 11 +chart_test)
	(draw-words canvas "100%" (- w 44) (+ y0 4) 10 +chart_text)
	(draw-words canvas "80%" (- w 40) (+ y1 4) 10 +chart_text)
	(defq span (max 1 (dec (length shown))))
	(draw-line canvas +chart_loss 1.5
		(reduce (lambda (out v)
				(push out (+ x0 (/ (* (!) (- x1 x0)) span))
					(- y1 (n2i (* (/ (min v top) top) (n2d (- y1 y0)))))))
			shown (list)))
	(defq place (lambda (out (step right))
			(defq at (- step first_step))
			(if (>= at 0)
				(push out (+ x0 (/ (* at (- x1 x0)) span))
					(- y1 (/ (* (max 0 (- right 8000)) (- y1 y0)) 2000))))
			out)
		points (reduce place valids (list)))
	(draw-line canvas +chart_valid 2.0 points)
	(each (lambda ((x y)) (.-> canvas (:set_color +chart_valid) (:fbox (- x 2) (- y 2) 5 5)))
		(partition points 2))
	(each (lambda ((x y)) (.-> canvas (:set_color +chart_test) (:fbox (- x 3) (- y 3) 7 7)))
		(partition (reduce place tests (list)) 2))
	(draw-words canvas (cat "steps " (str first_step) " to " (str (length losses))) x0 (- h 8) 10 +chart_text)
	(. canvas :swap +swap_write))

(defun draw-features (canvas params)
	; (draw-features canvas params)
	;the weights of the first 16 hidden units, each its 28 by 28 of what
	;it looks for, warm where it wants ink and blue where it wants none
	(.-> canvas (:fill +chart_back))
	(draw-words canvas "hidden units" 6 16 11 +chart_text)
	(when params
		(each! (lambda ()
				(defq unit (!) row (slice params (* unit +digits_inputs) (* (inc unit) +digits_inputs))
					top (max (const (n2d 0.0001)) (reduce (# (max %0 (abs (n2d %1)))) row +double_0))
					x0 (+ 6 (* (% unit 4) 58)) y0 (+ 26 (* (/ unit 4) 58)))
				(each (lambda (v)
						(defq v (n2d v) k (min 255 (n2i (* (/ (abs v) top) (const (n2d 255))))))
						(when (> k 24)
							(.-> canvas (:set_color (if (> v +double_0)
									(+ 0xff000000 (<< k 16) (<< (/ (* k 176) 255) 8) 0x30)
									(+ 0xff000000 (<< (/ (* k 64) 255) 16) (<< (/ (* k 128) 255) 8) k)))
								(:fbox (+ x0 (* (% (!) 28) 2)) (+ y0 (* (/ (!) 28) 2)) 2 2))))
					row))
			'() 0 16))
	(. canvas :swap +swap_write))

(defun draw-confusion (canvas confusion right)
	; (draw-confusion canvas confusion right)
	;each digit it was, down, and each it was taken for, across, the share
	;of that digit taken for each, green right and red wrong
	(.-> canvas (:fill +chart_back))
	(draw-words canvas (if confusion (cat "test " (double-to-str (/ (n2d right) (n2d 100)) 2) "% right")
		"confusion") 6 16 11 +chart_text)
	(when confusion
		(each! (lambda ()
				(defq row (!) total (max 1 (reduce! (const +) (list confusion) 0 (* row 10) (* (inc row) 10))))
				(draw-words canvas (str row) 8 (+ 44 (* row 19)) 10 +chart_text)
				(draw-words canvas (str row) (+ 30 (* row 20)) 238 10 +chart_text)
				(each! (lambda ()
						(defq n (elem-get confusion (+ (* row 10) (!))) k (min 255 (/ (* n 255) total))
							k (if (> n 0) (max k 40) 0))
						(.-> canvas (:set_color (cond ((= n 0) +chart_grid) ((= (!) row)
								(+ 0xff000000 (<< (/ (* k 64) 255) 16) (<< k 8) (/ (* k 96) 255)))
								((+ 0xff000000 (<< k 16) (<< (/ (* k 72) 255) 8) (/ (* k 72) 255)))))
							(:fbox (+ 26 (* (!) 20)) (+ 30 (* row 19)) 18 17)))
					'() 0 10))
			'() 0 10))
	(. canvas :swap +swap_write))
