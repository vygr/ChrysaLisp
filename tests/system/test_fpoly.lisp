(report-header "Fpoly: a shape is filled where it is, the same pixels with soft edges on as off")

(import "gui/lisp.inc")

(defun fp-draw (flags x y x1 y1 &optional clip)
	; (fp-draw flags x y x1 y1 [clip]) -> (row col)
	;a box filled white on a canvas of 20 by 20, and the alpha of each
	;pixel along row 10, and down column 10
	(defq canvas (Canvas 20 20 1) stream (memory-stream))
	(. canvas :set_canvas_flags flags)
	(if clip (apply (const .) (cat (list canvas :set_clip) clip)))
	(.-> canvas (:set_color +argb_white)
		(:fpoly 0.0 0.0 +winding_odd_even (list (path x y x1 y x1 y1 x y1))))
	(pixmap-write (getf canvas +canvas_pixmap 0) stream 32)
	(stream-seek stream 0 0)
	(defq d (read-blk stream 1000000) off (- (length d) (* 20 20 4)))
	(list (map (# (logand (get-uint d (+ off (* 4 (+ 200 %0)))) 0xff)) (range 0 20))
		(map (# (logand (get-uint d (+ off (* 4 (+ (* %0 20) 10)))) 0xff)) (range 0 20))))

(defun fp-run (line)
	; (fp-run line) -> (first last count)
	;the first and the last pixel of a line that has anything, and how many do
	(defq at (filter (# (/= 0 (elem-get line %0))) (range 0 20)))
	(if (empty? at) (list -1 -1 0) (list (first at) (last at) (length at))))

;a box from 5 to 15 is pixels 5 to 14, each way
(bind '(row col) (fp-draw 0 5.0 5.0 15.0 15.0))
(assert-eq "hard edges, across: pixels 5 to 14" "(5 14 10)" (str (fp-run row)))
(assert-eq "hard edges, down" "(5 14 10)" (str (fp-run col)))
(bind '(srow scol) (fp-draw 1 5.0 5.0 15.0 15.0))
(assert-eq "soft edges, across: the same pixels" (str row) (str srow))
(assert-eq "soft edges, down: the same pixels" (str col) (str scol))

;half way across a pixel is half of it, at both ends and each way
(bind '(row col) (fp-draw 1 5.5 5.5 15.5 15.5))
(assert-eq "a box from 5.5 to 15.5 touches pixels 5 to 15" "(5 15 11)" (str (fp-run row)))
(assert-eq "and down" "(5 15 11)" (str (fp-run col)))
(assert-eq "its first pixel is as much there as its last" (elem-get row 5) (elem-get row 15))
(assert-eq "down too" (elem-get col 5) (elem-get col 15))
(assert-true "and that is half" (< 120 (elem-get row 5) 135))
(assert-eq "between them it is all there" 255 (elem-get row 10))

;a clip cuts it to the pixels of the clip, the first column and row too
(bind '(row col) (fp-draw 1 0.0 0.0 20.0 20.0 '(4 4 16 16)))
(assert-eq "soft edges cut by a clip of 4 to 16: pixels 4 to 15" "(4 15 12)" (str (fp-run row)))
(assert-eq "and down" "(4 15 12)" (str (fp-run col)))
(assert-eq "the first column of the clip is all there" 255 (elem-get row 4))
(bind '(hrow hcol) (fp-draw 0 0.0 0.0 20.0 20.0 '(4 4 16 16)))
(assert-eq "as with hard edges" (str hrow) (str row))

;all of a canvas is all of it
(bind '(row col) (fp-draw 1 0.0 0.0 20.0 20.0))
(assert-eq "a box of the whole canvas fills its first column" 255 (first row))
(assert-eq "its last" 255 (last row))
(assert-eq "its first row" 255 (first col))
(assert-eq "and its last" 255 (last col))

;a hair off a whole number is still the whole pixel, or none of it
(bind '(row col) (fp-draw 1 5.0001 5.0001 14.9999 14.9999))
(assert-eq "a box a hair inside 5 to 15 is still pixels 5 to 14" "(5 14 10)" (str (fp-run row)))
(assert-eq "all of each" 255 (elem-get row 5))
(assert-eq "at both ends" 255 (elem-get row 14))
(assert-eq "and down" "(5 14 10)" (str (fp-run col)))

;the outline of a stroke is as far from the line as it is asked to be
(defq fp_out (path-stroke-polygons (list) 0.5 +join_miter
	(list (path 40.0 40.0 100.0 40.0 100.0 100.0 40.0 100.0))))
(assert-eq "a stroke 1.0 wide round a box has an outer edge half a pixel out"
	"(39.50000 100.50000)"
	(str (list (reduce (const min) (first fp_out)) (reduce (const max) (first fp_out)))))
(assert-eq "and an inner edge half a pixel in"
	"(40.50000 99.50000)" (str (list (reduce (const min) (second fp_out)) (reduce (const max) (second fp_out)))))

(defun fp-count (flags width)
	; (fp-count flags width) -> pixels
	;how many pixels a stroke of this width round a box of 60 has
	(defq canvas (Canvas 140 140 1) stream (memory-stream))
	(. canvas :set_canvas_flags flags)
	(.-> canvas (:set_color +argb_white) (:fpoly 0.0 0.0 +winding_none_zero
		(path-stroke-polygons (list) (* width 0.5) +join_miter
			(list (path 40.0 40.0 100.0 40.0 100.0 100.0 40.0 100.0)))))
	(pixmap-write (getf canvas +canvas_pixmap 0) stream 32)
	(stream-seek stream 0 0)
	(defq d (read-blk stream 1000000) off (- (length d) (* 140 140 4)))
	(length (filter (# (/= 0 (logand (get-uint d (+ off (* 4 %0))) 0xff))) (range 0 (* 140 140)))))

(assert-eq "hard edges, a stroke 1.0 wide is a line of pixels one wide" 240 (fp-count 0 1.0))
(assert-eq "one 2.0 wide is two" 480 (fp-count 0 2.0))
(assert-eq "soft edges, one 1.0 wide along whole numbers is half of two" 480 (fp-count 1 1.0))
(assert-eq "and one 2.0 wide is two, no more" 480 (fp-count 1 2.0))
