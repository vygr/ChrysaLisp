(report-header "Paint: gradients drawn through the canvas's span, and a paint of one color the solid color to the byte")

(import "gui/lisp.inc")

(defun pt-pixels (canvas)
	; (pt-pixels canvas) -> (data offset width)
	;the 32 bit pixels of a canvas, a row after another from offset
	(defq stream (memory-stream) pixmap (getf canvas +canvas_pixmap 0) w (getf pixmap +pixmap_width 0))
	(pixmap-write pixmap stream 32)
	(stream-seek stream 0 0)
	(defq d (read-blk stream 10000000))
	(list d (- (length d) (* w (getf pixmap +pixmap_height 0) +int_size)) w))

(defun pt-pixel (pixels x y)
	; (pt-pixel pixels x y) -> argb
	(bind '(d off w) pixels)
	(get-uint d (+ off (* +int_size (+ (* y w) x)))))

(defq pt_font (create-font "fonts/OpenSans-Regular.ctf" 18)
	pt_text "Paint, 0123 the quick brown fox"
	pt_disc (list (path-gen-arc 48.0 48.0 0.0 +fp_2pi 40.0 (path)))
	pt_box (list (path 20.0 30.0 110.0 30.0 110.0 90.0 20.0 90.0)))

(defun pt-scene (flags paint)
	; (pt-scene flags paint) -> pixels
	;shapes, boxes and text, each drawn after (paint canvas argb)
	(defq canvas (Canvas 128 128 1))
	(. canvas :set_canvas_flags flags)
	(. canvas :fill 0)
	(paint canvas 0xc0ff8040)
	(. canvas :fpoly 0.0 0.0 +winding_odd_even pt_disc)
	(paint canvas 0x80204080)
	(. canvas :fpoly 20.0 10.0 +winding_none_zero pt_disc)
	(paint canvas 0xff00c000)
	(. canvas :fpoly 0.0 0.0 +winding_odd_even pt_box)
	(paint canvas 0x7fffffff)
	(.-> canvas (:fbox 5 5 30 100) (:fbox -10 120 300 30))
	(paint canvas 0xe0102030)
	(. canvas :fpoly 2.0 110.0 +winding_none_zero (font-glyph-paths pt_font pt_text))
	(pt-pixels canvas))

;a gradient of one color draws what that color does, every pixel, hard
;and soft, in shapes, boxes and text: the paint's blend is the solid one
(each (lambda (flags name)
		(defq solid (first (pt-scene flags (# (. %0 :set_color %1)))))
		(assert-true (cat "the scene has something drawn, " name) (> (length (filter (# (nql %0 (const (char 0)))) solid)) 1000))
		(assert-eq (cat "a linear gradient of one color, " name) solid
			(first (pt-scene flags (# (. %0 :set_linear 0.0 0.0 100.0 37.0 (list (list 0.0 %1) (list 1.0 %1)))))))
		(assert-eq (cat "a radial gradient of one color, " name) solid
			(first (pt-scene flags (# (. %0 :set_radial 64.0 64.0 50.0 (list (list 0.0 %1) (list 1.0 %1)))))))
		(assert-eq (cat "and the solid color again after a gradient, " name) solid
			(first (pt-scene flags (# (. %0 :set_linear 0.0 0.0 9.0 9.0 '((0.0 0xff0000ff) (1.0 0xffff0000)))
				(. %0 :set_color %1))))))
	(list 0 +canvas_flag_antialias) '("hard" "soft"))

;a ramp of black to white across 256 pixels: pixel x is x, exactly
(defq pt_ramp (Canvas 256 2 1))
(.-> pt_ramp (:fill 0) (:set_linear 0.0 0.0 256.0 0.0 '((0.0 0xff000000) (1.0 0xffffffff))) (:fbox 0 0 256 2))
(defq pt_px (pt-pixels pt_ramp))
(assert-eq "a ramp of 256, each pixel its own step" '()
	(filter (# (/= (pt-pixel pt_px %0 1) (+ 0xff000000 (* %0 0x010101)))) (map! (# (!)) '() 0 256)))

(defun pt-off (canvas expect most)
	; (pt-off canvas expect most) -> (x y got want) | :nil
	;the first pixel whose blue is more than most from what (expect x y) says
	(defq px (pt-pixels canvas) w (third px) h (/ (- (length (first px)) (second px)) (* w +int_size)) bad :nil y 0)
	(while (and (not bad) (< y h))
		(defq x 0)
		(while (and (not bad) (< x w))
			(defq got (logand (pt-pixel px x y) 0xff) want (expect x y))
			(if (> (abs (- got want)) most) (setq bad (list x y got want)))
			(++ x))
		(++ y))
	bad)

(defun pt-floor (n d)
	; (pt-floor n d) -> int
	(defq q (/ n d))
	(if (< (- n (* q d)) 0) (dec q) q))

(defun pt-step (i spread)
	;a step of the 256 by the spread, black to white is that step's blue
	(case spread
		(0 (max 0 (min 255 i)))
		(1 (setq i (logand i 511)) (if (> i 255) (- 511 i) i))
		(:t (logand i 255))))

;every pixel against the gradient worked out here: t at a pixel's centre,
;its step of the 256 the floor of 256 t. A linear exactly, t of a pixel
;x + 1/2 and y + 1/2 from 10 50 along 30 -30 by its length squared, 1800,
;in halves so all of it is integers; a radial within one, its root of a
;double here and of fixed there
(defq pt_bw '((0.0 0xff000000) (1.0 0xffffffff)))
(each (lambda (spread name)
		(defq c (Canvas 64 64 1))
		(.-> c (:fill 0) (:set_linear 10.0 50.0 40.0 20.0 pt_bw spread) (:fbox 0 0 64 64))
		(assert-eq (cat "a linear gradient, every pixel exactly, " name) :nil
			(pt-off c (lambda (x y)
				(pt-step (pt-floor (* 256 (+ (* (- (inc (* 2 x)) 20) 30) (* (- (inc (* 2 y)) 100) -30))) 3600) spread)) 0))
		(defq c (Canvas 64 64 1))
		(.-> c (:fill 0) (:set_radial 30.0 34.0 13.0 pt_bw spread) (:fbox 0 0 64 64))
		(assert-eq (cat "a radial gradient, every pixel, " name) :nil
			(pt-off c (lambda (x y)
				(defq dx (- (+ (n2d x) (const (n2d 0.5))) (const (n2d 30))) dy (- (+ (n2d y) (const (n2d 0.5))) (const (n2d 34))))
				(pt-step (n2i (* (/ (sqrt (+ (* dx dx) (* dy dy))) (const (n2d 13))) (const (n2d 256)))) spread)) 1)))
	(list +spread_pad +spread_reflect +spread_repeat) '("pad" "reflect" "repeat"))

;a gradient far from the canvas's corner is as exact as one at it: the
;step of a pixel is worked out from the integers for every run
(defq c (Canvas 4096 2 1))
(.-> c (:fill 0) (:set_linear 3000.0 0.0 3256.0 0.0 pt_bw) (:fbox 0 0 4096 2))
(assert-eq "a ramp 3000 pixels along, each pixel its own step" :nil
	(pt-off c (lambda (x y) (pt-step (- x 3000) 0)) 0))

;the strips: a child draws on a canvas of a shared pixmap, its own canvas
;of the same pixels, and a gradient there draws what it does on any canvas
(cond
	((defq pt_shared (canvas-shared 64 64 1))
		(defq pt_own (Canvas 64 64 1))
		(each (# (.-> %0 (:fill 0) (:set_radial 30.0 34.0 13.0 pt_bw +spread_reflect)
				(:fpoly 0.0 0.0 +winding_odd_even pt_disc)))
			(list pt_shared pt_own))
		(assert-eq "a gradient on a canvas of shared pixels" (first (pt-pixels pt_own)) (first (pt-pixels pt_shared))))
	(:t (test-skip "a gradient on a canvas of shared pixels" "no shared memory on this host")))

;the paint's str holds a row as wide as the pixmap, or it is refused
(ffi "gui/canvas/lisp_paint" pt-paint)
(assert-true "a paint too short for the pixmap is refused"
	(catch (progn (pt-paint (Canvas 64 4 1) 1 (str-alloc 100)) :nil) :t))
(assert-true "and a paint that is not a str"
	(catch (progn (pt-paint (Canvas 64 4 1) 1 '(1 2 3)) :nil) :t))
