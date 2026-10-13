;the polygon fill, :canvas :fpoly: text, which is many small shapes side by
;side, and one big shape, with soft edges and with hard. A line of text is
;what a Text makes a texture of, at twice the size and soft
(import "gui/lisp.inc")

(defq bf_font (create-font "fonts/OpenSans-Regular.ctf" 14)
	bf_text (apply (const cat) (map (# (str (% %0 10) "abcdef7")) (range 0 125))))

(defun bf-line (len)
	; (bf-line len) -> (canvas paths y)
	(defq text (slice bf_text 0 len) b (font-glyph-bounds bf_font text)
		canvas (Canvas (first b) (second b) 2))
	(. canvas :set_canvas_flags +canvas_flag_antialias)
	(list canvas (font-glyph-paths bf_font text) (n2f (second b))))

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
