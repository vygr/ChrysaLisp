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

;The edges a scan line cuts are kept in order across. What is drawn must not
;turn on the order the paths are given in, nor on which way a list of edges
;that is nearly in order is nearly in order: a line of text is, left to
;right, and it was that which cost the square of its length to sort
(import "lib/crypto/sha256.inc")

(defq fp_seed 12345 fp_font (create-font "fonts/OpenSans-Regular.ctf" 14))

(defun fp-rnd (n)
	; (fp-rnd n) -> 0 to n-1, the same each run
	(setq fp_seed (logand (+ (* fp_seed 1103515245) 12345) 0x7fffffff))
	(% (>> fp_seed 8) n))

(defun fp-mix (paths)
	; (fp-mix paths) -> paths
	(defq paths (cat paths))
	(each (lambda (_) (swap paths (!) (fp-rnd (length paths)))) paths)
	paths)

(defun fp-hash (w h flags mode y paths)
	; (fp-hash w h flags mode y paths) -> str
	;what the pixels are, of these paths filled
	(defq canvas (Canvas w h 1) stream (memory-stream))
	(. canvas :set_canvas_flags flags)
	(.-> canvas (:set_color +argb_white) (:fpoly 0.0 y mode paths))
	(pixmap-write (getf canvas +canvas_pixmap 0) stream 32)
	(stream-seek stream 0 0)
	(hex-encode (sha256 (read-blk stream 100000000))))

(defq fp_text "The quick brown fox jumps over the lazy dog, 0123456789 times; (WAVE) %$#@!"
	fp_bounds (font-glyph-bounds fp_font fp_text) fp_glyphs (font-glyph-paths fp_font fp_text)
	fp_boxes (map (lambda (_)
			(defq x (n2f (fp-rnd 380)) y (n2f (fp-rnd 180)) w (n2f (+ 3 (fp-rnd 60))) h (n2f (+ 3 (fp-rnd 60))))
			(if (= 0 (fp-rnd 2))
				(path x y (+ x w) y (+ x w) (+ y h) x (+ y h))
				(path x y x (+ y h) (+ x w) (+ y h) (+ x w) y)))
		(range 0 300))
	fp_lines (path-stroke-polylines (list) 1.5 +join_bevel +cap_butt +cap_butt
		(map (lambda (_) (path (n2f (fp-rnd 400)) (n2f (fp-rnd 200)) (n2f (fp-rnd 400)) (n2f (fp-rnd 200))))
			(range 0 120))))

(each (lambda (flags)
	(each (lambda (mode)
		(defq what (cat (if (= flags 0) "hard" "soft") " edges, " (if (eql mode +winding_odd_even) "odd even" "non zero") ": ")
			h (second fp_bounds) y (n2f h)
			want (fp-hash (first fp_bounds) h flags mode y fp_glyphs))
		(assert-eq (cat what "a line of text is the same with its glyphs right to left")
			want (fp-hash (first fp_bounds) h flags mode y (reverse fp_glyphs)))
		(assert-eq (cat what "and in any order")
			want (fp-hash (first fp_bounds) h flags mode y (fp-mix fp_glyphs)))
		(setq want (fp-hash 400 200 flags mode 0.0 fp_boxes))
		(assert-eq (cat what "300 boxes that overlap are the same the other way round")
			want (fp-hash 400 200 flags mode 0.0 (reverse fp_boxes)))
		(assert-eq (cat what "and in any order")
			want (fp-hash 400 200 flags mode 0.0 (fp-mix fp_boxes)))
		(assert-eq (cat what "120 lines that cross are the same in any order")
			(fp-hash 400 200 flags mode 0.0 fp_lines) (fp-hash 400 200 flags mode 0.0 (fp-mix fp_lines))))
		(list +winding_odd_even +winding_none_zero)))
	'(0 1))

;a line of 2,000 characters, 3,000 paths and 120,000 points, was 8 seconds on
;the fastest of the machines and is a few hundredths. Twice as long was four
;times as much. It is filled here, its time is in tests/bench/bench_fill.lisp
(defq fp_long (apply (const cat) (map (# (str (% %0 10) "abcdef7")) (range 0 250)))
	fp_bounds (font-glyph-bounds fp_font fp_long) fp_glyphs (font-glyph-paths fp_font fp_long)
	fp_canvas (Canvas (first fp_bounds) (second fp_bounds) 2))
(. fp_canvas :set_canvas_flags +canvas_flag_antialias)
(.-> fp_canvas (:set_color +argb_white) (:fpoly 0.0 (n2f (second fp_bounds)) +winding_none_zero fp_glyphs))

;and a Text is asked to draw no more of a line than any screen is wide
(assert-eq "a Text draws a short line whole" "abc" (text-most "abc"))
(assert-eq "and of a very long one its first 512 bytes" 512 (length (text-most (pad "" 4000 "x"))))
(assert-eq "cut between characters, not in one"
	511 (length (text-most (cat (pad "" 511 "x") (char 0xa9c3 2) (pad "" 100 "x")))))
(defq fp_label (Text))
(def fp_label :font fp_font :text fp_long)
(assert-true "so it wants no more width than that much of it has"
	(= (first (. fp_label :constraint)) (first (font-glyph-bounds fp_font (slice fp_long 0 512)))))
