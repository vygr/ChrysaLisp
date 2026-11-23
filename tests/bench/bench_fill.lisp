;the polygon fill, :canvas :fpoly: text, which is many small shapes side by
;side, and one big shape, with soft edges and with hard. A line of text is
;what a Text makes a texture of, at twice the size and soft
(import "gui/lisp.inc")

(defq bf_font (create-font "fonts/OpenSans-Regular.ctf" 14)
	bf_text (apply (const cat) (map! (# (str (% (!) 10) "abcdef7")) '() 0 125)))

(defun bf-line (len)
	; (bf-line len) -> (canvas paths y)
	(defq text (slice bf_text 0 len) b (font-glyph-bounds bf_font text)
		canvas (Canvas (first b) (second b) 2))
	(. canvas :set_canvas_flags +canvas_flag_antialias)
	(list canvas (font-glyph-paths bf_font text) (n2q (second b))))

(each (lambda (len runs)
		(bind '(canvas paths y) (bf-line len))
		(bench (cat "a line of " (str len) " characters, soft, non zero, each character") (* runs len)
			(times runs (.-> canvas (:set_color +argb_white) (:fpoly 0.0 y +winding_none_zero paths)))))
	'(5 20 80 400 1000) '(2000 1000 400 100 40))

(bench "the paths of 80 characters made, each character" (* 2000 80)
	(times 2000 (font-glyph-paths bf_font (slice bf_text 0 80))))
(bench "the bounds of 80 characters, each character" (* 20000 80)
	(times 20000 (font-glyph-bounds bf_font (slice bf_text 0 80))))

(defq bf_canvas (Canvas 512 512 1) bf_disc (list (path-gen-arc 256.0 256.0 0.0 +fp_2pi 200.0 (path)))
	bf_box (list (path 56.0 56.0 456.0 56.0 456.0 456.0 56.0 456.0)))
(each (lambda (name flags)
		(. bf_canvas :set_canvas_flags flags)
		(bench (cat "a disc of 400 across, " name ", odd even") 500
			(times 500 (.-> bf_canvas (:set_color +argb_white) (:fpoly 0.0 0.0 +winding_odd_even bf_disc))))
		(bench (cat "a disc of 400 across, " name ", non zero") 500
			(times 500 (.-> bf_canvas (:set_color +argb_white) (:fpoly 0.0 0.0 +winding_none_zero bf_disc))))
		(bench (cat "a box of 400 across, " name ", odd even") 500
			(times 500 (.-> bf_canvas (:set_color +argb_white) (:fpoly 0.0 0.0 +winding_odd_even bf_box)))))
	'("hard" "soft") (list 0 +canvas_flag_antialias))

;the box fill, :fbox, a row at a time to the span with no :fpoly: one
;big box, and many small ones, where the span is called for few pixels
(. bf_canvas :set_canvas_flags 0)
(bench "a box of 400 across, :fbox" 500
	(times 500 (.-> bf_canvas (:set_color +argb_white) (:fbox 56 56 400 400))))
(bench "boxes of 4 across, :fbox, each box" (* 50 1024)
	(times 50 (. bf_canvas :set_color +argb_white)
		(each! (# (. bf_canvas :fbox (* 16 (logand (!) 31)) (* 16 (>> (!) 5)) 4 4)) '() 0 1024)))

;a field of small soft discs, nearly all edge: a run of one pixel to the
;span for most of what is drawn, what costs most for a call each run
(defq bf_dots (map! (# (path-gen-arc (n2q (+ 8 (* 16 (logand (!) 31)))) (n2q (+ 8 (* 16 (>> (!) 5))))
		0.0 +fp_2pi 4.0 (path))) '() 0 1024))
(. bf_canvas :set_canvas_flags +canvas_flag_antialias)
(bench "discs of 8 across, soft, each disc" (* 20 1024)
	(times 20 (.-> bf_canvas (:set_color +argb_white) (:fpoly 0.0 0.0 +winding_odd_even bf_dots))))

;a paint against the solid color, the same disc: the cost of the paint
;span, a row of colors made then blended, over the one color. The paint
;is set the once, its table of colors made in Lisp, timed on its own
(defq bf_stops '((0.0 0xffff4040) (0.5 0xffffff40) (1.0 0xff4040ff)))
(bench "a linear gradient set, its 256 colors made" 50
	(times 50 (. bf_canvas :set_linear 56.0 56.0 456.0 456.0 bf_stops)))
(bench "a radial gradient set, its 256 colors made" 50
	(times 50 (. bf_canvas :set_radial 256.0 256.0 200.0 bf_stops)))
(each (lambda (name flags)
		(. bf_canvas :set_canvas_flags flags)
		(. bf_canvas :set_linear 56.0 56.0 456.0 456.0 bf_stops)
		(bench (cat "a disc of 400 across, " name ", a linear gradient") 500
			(times 500 (. bf_canvas :fpoly 0.0 0.0 +winding_odd_even bf_disc)))
		(bench (cat "a box of 400 across, " name ", :fbox, a linear gradient") 500
			(times 500 (. bf_canvas :fbox 56 56 400 400)))
		(. bf_canvas :set_radial 256.0 256.0 200.0 bf_stops)
		(bench (cat "a disc of 400 across, " name ", a radial gradient") 500
			(times 500 (. bf_canvas :fpoly 0.0 0.0 +winding_odd_even bf_disc))))
	'("hard" "soft") (list 0 +canvas_flag_antialias))
(. bf_canvas :set_color +argb_white)
