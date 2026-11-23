(report-header "SVG: what draws nothing passed over, colors, lengths, opacity and visibility, nothing thrown")

(import "gui/lisp.inc")

(defun sv-pixels (svg)
	; (sv-pixels svg) -> (data offset width)
	;an SVG of the body given, 40 by 40, drawn, its 32 bit pixels
	(defq canvas (SVG-load (string-stream (cat "<svg width=\q40\q height=\q40\q>" svg "</svg>")))
		stream (memory-stream) pixmap (getf canvas +canvas_pixmap 0))
	(pixmap-write pixmap stream 32)
	(stream-seek stream 0 0)
	(defq d (read-blk stream 1000000))
	(list d (- (length d) (* 40 40 +int_size)) 40))

(defun sv-at (svg x y)
	; (sv-at svg x y) -> argb
	(bind '(d off w) (sv-pixels svg))
	(get-uint d (+ off (* +int_size (+ (* y w) x)))))

(defq sv_box "<rect x=\q10\q y=\q10\q width=\q20\q height=\q20\q")

;what draws nothing, and all that is in it
(each (lambda (outer)
		(assert-eq (cat "nothing in " outer " is drawn") 0
			(sv-at (cat "<" outer ">" sv_box " fill=\qred\q/></" (first (split outer " ")) ">") 20 20)))
	'("defs" "clipPath id=\qc\q" "symbol" "mask" "pattern" "linearGradient" "title" "metadata"
		"sodipodi:namedview" "g display=\qnone\q" "g style=\qdisplay: none\q"))
(assert-eq "and what is after it is" 0xffff0000
	(sv-at (cat "<defs><g><rect/></g></defs>" sv_box " fill=\qred\q/>") 20 20))

;colors: the CSS names, #, rgb() of numbers and of %, currentColor
(each (lambda ((fill argb))
		(assert-eq (cat "fill " fill) argb (sv-at (cat sv_box " fill=\q" fill "\q/>") 20 20)))
	'(("rebeccapurple" 0xff663399) ("DarkOrange" 0xffff8c00) ("#0f0" 0xff00ff00) ("#123456" 0xff123456)
		("rgb(10, 20, 30)" 0xff0a141e) ("rgb(100%, 0%, 0%)" 0xffff0000)))
(assert-eq "currentColor, the color of the element it is of or its parent's" 0xff00ff00
	(sv-at (cat "<g color=\q#00ff00\q>" sv_box " fill=\qcurrentColor\q/></g>") 20 20))
(assert-eq "a fill not known, the parent's, nothing thrown" 0xff0000ff
	(sv-at (cat "<g fill=\qblue\q>" sv_box " fill=\qnot-a-color\q/></g>") 20 20))
(assert-eq "inherit, the parent's" 0xff0000ff
	(sv-at (cat "<g fill=\qblue\q>" sv_box " fill=\qinherit\q/></g>") 20 20))
(assert-eq "a value in style over its attribute, with spaces" 0xff00ff00
	(sv-at (cat sv_box " fill=\qred\q style=\q fill : lime ; \q/>") 20 20))

;opacity, of an element and of a color, premultiplied in the pixmap
(defun sv-alpha (svg) (>> (sv-at svg 20 20) 24))
(assert-true "opacity 0.5, half" (<= 126 (sv-alpha (cat sv_box " fill=\qwhite\q opacity=\q0.5\q/>")) 128))
(assert-true "fill-opacity 50%, half" (<= 126 (sv-alpha (cat sv_box " fill=\qwhite\q fill-opacity=\q50%\q/>")) 128))
(assert-true "rgba of 0.5, half" (<= 126 (sv-alpha (cat sv_box " fill=\qrgba(255, 255, 255, 0.5)\q/>")) 128))
(assert-eq "opacity 0, nothing" 0 (sv-alpha (cat sv_box " fill=\qwhite\q opacity=\q0\q/>")))

;visibility: hidden is not drawn, and a child of it that is visible is
(assert-eq "visibility hidden" 0 (sv-at (cat sv_box " fill=\qred\q visibility=\qhidden\q/>") 20 20))
(assert-eq "a visible child of a hidden group" 0xffff0000
	(sv-at (cat "<g visibility=\qhidden\q>" sv_box " fill=\qred\q visibility=\qvisible\q/></g>") 20 20))

;lengths with units, and numbers with an exponent
(defun sv-right (svg)
	;the first pixel of row 20 from the right that has anything, from x 0
	(bind '(d off w) (sv-pixels svg))
	(defq x 39)
	(while (and (>= x 0) (= 0 (get-uint d (+ off (* +int_size (+ (* 20 w) x)))))) (setq x (dec x)))
	x)
(assert-eq "a width in px" 19 (sv-right "<rect x=\q0\q y=\q0\q width=\q20px\q height=\q40\q fill=\qred\q/>"))
(assert-eq "in pt, 4/3 of a pixel" 19 (sv-right "<rect x=\q0\q y=\q0\q width=\q15pt\q height=\q40\q fill=\qred\q/>"))
(assert-eq "in mm, 96 pixels to 25.4" 37 (sv-right "<rect x=\q0\q y=\q0\q width=\q10mm\q height=\q40\q fill=\qred\q/>"))
(assert-eq "with an exponent" 19 (sv-right "<rect x=\q0\q y=\q0\q width=\q2e1\q height=\q40\q fill=\qred\q/>"))
(assert-eq "a value with a unit not known, the default" -1
	(sv-right "<rect x=\q0\q y=\q0\q width=\q20furlongs\q height=\q40\q fill=\qred\q/>"))

;the SVGs of the Images app, each drawn, nothing thrown
(each (lambda (name)
		(assert-true (cat name ".svg drawn")
			(SVG-load (file-stream (cat "apps/media/images/data/" name ".svg")))))
	'("burger" "chrysalisp" "clock" "dial" "golfer" "kennedy" "sticker" "tiger"))
