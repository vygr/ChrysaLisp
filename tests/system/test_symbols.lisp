(report-header "Symbols: the symbol font is made by the system, and what is in the repo is what it makes")

(import "gui/lisp.inc")
(import "lib/font/symbol_set.inc")

(defq sy_names (map (const first) *symbols*))
(assert-true "there are symbols" (> (length sy_names) 100))
(assert-eq "no two have the one name" (length sy_names) (length (unique (sort (cat sy_names)))))
(assert-true "a name is lower case, digits and _" (every (# (eql %0 (to-lower %0))) sy_names))
(assert-eq "a symbol has a constant, the first is close" +sf_base +sym_close)
(assert-eq "and its code is where it is in the list" (+ +sf_base (find "save_all" sy_names)) +sym_save_all)

;the kit
(assert-list-eq "a line is its points" '(:line (6.0 6.0 18.0 18.0)) (first (s-line 6 6 18 18)))
(assert-list-eq "mirrored, left is right" '(:line (18.0 6.0 6.0 18.0)) (first (s-mirror (s-line 6 6 18 18))))
(assert-list-eq "flipped, top is bottom" '(:line (6.0 18.0 18.0 6.0)) (first (s-flip (s-line 6 6 18 18))))
(assert-list-eq "turned, across is down" '(:ring 7.0 5.0 2.0) (first (s-turn (s-ring 5 7 2))))
(assert-eq "an arrow is a line and the two sides of its head" 3 (length (s-arrow 4 8 19 8)))
(assert-eq "a thing made small has its strokes, and room for a badge" 4 (length (s-all (s-line 6 6 18 18))))

;a stroke is an outline, and two that cross are two outlines, filled the once
(defq sy_cross (sym-paths (second (first *symbols*)) 1.15 +join_round +cap_round))
(assert-eq "the cross of close is the outlines of two strokes" 2 (length sy_cross))
(defq sy_canvas (Canvas 192 192 1))
(.-> sy_canvas (:set_color +argb_white) (:fpoly 0.0 0.0 +winding_none_zero sy_cross))
(defun sy-pixel (x y)
	(defq stream (memory-stream))
	(pixmap-write (getf sy_canvas +canvas_pixmap 0) stream 32)
	(stream-seek stream 0 0)
	(defq d (read-blk stream 1000000))
	(logand (get-uint d (+ (- (length d) (* 192 192 4)) (* 4 (+ (* y 192) x)))) 0xff))
(assert-eq "where they cross is filled, by the rule a glyph is drawn with" 255 (sy-pixel 96 96))
(assert-eq "an arm of it is" 255 (sy-pixel 60 60))
(assert-eq "and between the arms is not" 0 (sy-pixel 96 50))

;a disc and a stroke that overlap go round the one way, so there is no hole
(defq sy_canvas (Canvas 192 192 1))
(.-> sy_canvas (:set_color +argb_white) (:fpoly 0.0 0.0 +winding_none_zero
	(sym-paths (second (elem-get *symbols* (find "comment" sy_names))) 1.15 +join_round +cap_round)))
(assert-eq "where the tail of comment leaves its dot is filled" 255 (sy-pixel 104 126))
(assert-eq "and its other dot is" 255 (sy-pixel 96 60))

;a font
(defq sy_font (sym-font *symbols* 1.15 +join_round +cap_round))
(assert-eq "a font starts with its ascent, 0.9375 of 8192" 7680 (get-short sy_font 0))
(assert-eq "its one page ends at the last symbol" (+ +sf_base (dec (length *symbols*))) (get-ushort sy_font 8))
(assert-eq "and starts at the first" +sf_base (get-ushort sy_font 10))
(defq sy_first (get-uint sy_font 12))
(assert-eq "the first glyph is after the page and its end" (+ 8 4 (* 4 (length *symbols*)) 4) sy_first)
(assert-eq "and is that of the first code" +sf_base (get-ushort sy_font sy_first))
(assert-true "every glyph is where its offset says, with its own code"
	(every (# (= (get-ushort sy_font (get-uint sy_font (+ 12 (* 4 %0)))) (+ +sf_base %0)))
		(range 0 (length *symbols*))))
(assert-true "and no glyph is empty"
	(every (# (> (get-uint sy_font (+ (get-uint sy_font (+ 12 (* 4 %0))) 4)) 0)) (range 0 (length *symbols*))))

;what is in the repo is what the system makes now
(each (lambda ((file radius joint cap))
	(assert-true (cat file " is the font the symbols make")
		(eql (load file) (sym-font *symbols* radius joint cap))))
	*sym_themes*)
(assert-true "lib/consts/symbols.inc is the names the symbols make"
	(eql (load "lib/consts/symbols.inc") (sym-names-text)))
