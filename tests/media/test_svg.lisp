(report-header "SVG: what draws nothing passed over, colors, lengths, opacity and visibility, nothing thrown")

(import "gui/lisp.inc")

(defun sv-pixels (svg &optional head)
	; (sv-pixels svg [head]) -> (data offset width)
	;an SVG of the body given drawn, 40 by 40 unless head is its <svg ...>,
	;its 32 bit pixels
	(defq canvas (SVG-load (string-stream (cat (ifn head "<svg width=\q40\q height=\q40\q>") svg "</svg>")))
		stream (memory-stream) pixmap (getf canvas +canvas_pixmap 0))
	(pixmap-write pixmap stream 32)
	(stream-seek stream 0 0)
	(defq d (read-blk stream 1000000) w (getf pixmap +pixmap_width 0))
	(list d (- (length d) (* w (getf pixmap +pixmap_height 0) +int_size)) w))

(defun sv-at (svg x y &optional head)
	; (sv-at svg x y [head]) -> argb
	(bind '(d off w) (sv-pixels svg head))
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

;use: what it names drawn again, moved by its x and y, its own attributes
;over it, a symbol as a group, whether what it names is in a defs or not
(defq sv_def "<defs><rect id=\qr\q width=\q10\q height=\q10\q fill=\qred\q/></defs>")
(assert-eq "use of what is in a defs, moved" (str '(0 0xffff0000))
	(str (map (# (sv-at (cat sv_def "<use href=\q#r\q x=\q20\q y=\q20\q/>") %0 %0)) '(5 25))))
(assert-eq "by xlink:href, of what is drawn already, so it is twice" (str '(0xff0000ff 0xff0000ff))
	(str (map (# (sv-at (cat "<rect id=\qa\q width=\q10\q height=\q10\q fill=\qblue\q/><use xlink:href=\q#a\q x=\q20\q/>") %0 5)) '(5 25))))
(assert-eq "its own fill given what it uses, which has none" 0xff00ff00
	(sv-at "<defs><rect id=\qb\q width=\q10\q height=\q10\q/></defs><use href=\q#b\q fill=\qlime\q x=\q20\q y=\q20\q/>" 25 25))
(assert-eq "a symbol, as a group" 0xffff0000
	(sv-at "<symbol id=\qs\q><rect width=\q10\q height=\q10\q fill=\qred\q/></symbol><use href=\q#s\q x=\q20\q y=\q20\q/>" 25 25))
(assert-eq "after its transform" 0xffff0000
	(sv-at (cat sv_def "<use href=\q#r\q transform=\qtranslate(20 0)\q y=\q20\q/>") 25 25))
(assert-eq "of an id that is not there, nothing, nothing thrown" 0
	(sv-at "<use href=\q#none\q x=\q20\q y=\q20\q/>" 25 25))
(assert-true "of itself, an end to it" (sv-pixels "<g id=\qg\q><use href=\q#g\q x=\q1\q/></g>"))

;dashes: on and off along a line, from an offset into the pattern
(defun sv-row (svg)
	;which of the pixels 0 to 39 of row 20 have anything, a str of 1 and 0
	(bind '(d off w) (sv-pixels svg))
	(apply (const cat) (map! (# (if (= 0 (get-uint d (+ off (* +int_size (+ (* 20 w) (!)))))) "0" "1")) '() 0 40)))
(defq sv_line "<line x1=\q0\q y1=\q20.5\q x2=\q40\q y2=\q20.5\q stroke=\qred\q stroke-width=\q2\q")
(assert-eq "dashes 10 on 10 off" "1111111111000000000011111111110000000000"
	(sv-row (cat sv_line " stroke-dasharray=\q10 10\q/>")))
(assert-eq "an offset of 5 into them" "1111100000000001111111111000000000011111"
	(sv-row (cat sv_line " stroke-dasharray=\q10,10\q stroke-dashoffset=\q5\q/>")))
(assert-eq "an odd number said twice, 5 is 5 5" "1111100000111110000011111000001111100000"
	(sv-row (cat sv_line " stroke-dasharray=\q5\q/>")))
(assert-eq "none, and one less than 0, is the line whole" "(40 40)"
	(str (map (# (length (filter (# (eql %0 "1")) (sv-row (cat sv_line " stroke-dasharray=\q" %0 "\q/>")))))
		'("none" "5 -1"))))

;a viewBox fitted to the width and height, as preserveAspectRatio says
(defun sv-fitted (par)
	;a box of 10 filling a viewBox of 10 in an svg of 40 by 20, its row 10
	(bind '(d off w) (sv-pixels "<rect width=\q10\q height=\q10\q fill=\qred\q/>"
		(cat "<svg width=\q40\q height=\q20\q viewBox=\q0 0 10 10\q" (if par (cat " preserveAspectRatio=\q" par "\q") "") ">")))
	(apply (const cat) (map! (# (if (= 0 (get-uint d (+ off (* +int_size (+ (* 10 w) (!)))))) "0" "1")) '() 0 w)))
(assert-eq "xMidYMid meet, the default, fitted in and in the middle"
	"0000000000111111111111111111110000000000" (sv-fitted :nil))
(assert-eq "xMinYMid, at the left" "1111111111111111111100000000000000000000" (sv-fitted "xMinYMid"))
(assert-eq "xMaxYMax meet, at the right" "0000000000000000000011111111111111111111" (sv-fitted "xMaxYMax meet"))
(assert-eq "none, stretched" "1111111111111111111111111111111111111111" (sv-fitted "none"))

;gradients: of the shape's box by default, or of the user's space, with
;their transform, a radial, by href, stops and their opacity, the spread
(defun sv-grad (defs fill x y)
	; (sv-grad defs fill x y) -> blue
	;the blue of a pixel of a box 10 to 30 filled so, defs before it
	(logand 0xff (sv-at (cat "<defs>" defs "</defs><rect x=\q10\q y=\q10\q width=\q20\q height=\q20\q fill=\q" fill "\q/>") x y)))
(defun sv-near (want got) (<= (abs (- want got)) 8))
(defq sv_bw "<stop offset=\q0\q stop-color=\qblack\q/><stop offset=\q1\q stop-color=\qwhite\q/>")
(assert-true "a linear gradient across the box, black at its left, white at its right, grey between"
	(every (const sv-near) '(6 128 249)
		(map (# (sv-grad (cat "<linearGradient id=\qg\q>" sv_bw "</linearGradient>") "url(#g)" %0 20)) '(10 20 29))))
(assert-true "of the user's space, from 10 to 30, the same"
	(every (const sv-near) '(6 128 249)
		(map (# (sv-grad (cat "<linearGradient id=\qg\q gradientUnits=\quserSpaceOnUse\q x1=\q10\q x2=\q30\q>" sv_bw "</linearGradient>") "url(#g)" %0 20)) '(10 20 29))))
(assert-true "turned by its transform, down the box"
	(every (const sv-near) '(6 128 249)
		(map (# (sv-grad (cat "<linearGradient id=\qg\q gradientTransform=\qrotate(90)\q>" sv_bw "</linearGradient>") "url(#g)" 20 %0)) '(10 20 29))))
;a pixel's centre is half a pixel on: of a radial of 10 about 20 20, the
;pixel 20 is 0.07 out, 15 0.45, 25 0.55, 10 0.95
(assert-true "a radial, black at its centre to white at its edge"
	(every (const sv-near) '(18 115 141 243)
		(map (# (sv-grad (cat "<radialGradient id=\qg\q>" sv_bw "</radialGradient>") "url(#g)" %0 20)) '(20 15 25 10))))
(assert-true "its stops by href from another"
	(sv-near 128 (sv-grad (cat "<linearGradient id=\qa\q>" sv_bw "</linearGradient><linearGradient id=\qg\q href=\q#a\q/>") "url(#g)" 20 20)))
;of a line half the box, pixels 14, 15 and 19 at t 0.45, 0.55 and 0.95,
;25 at 1.55, 0.45 reflected
(assert-true "reflected past a line half the box"
	(every (const sv-near) '(115 140 243 115)
		(map (# (sv-grad (cat "<linearGradient id=\qg\q x2=\q0.5\q spreadMethod=\qreflect\q>" sv_bw "</linearGradient>") "url(#g)" %0 20)) '(14 15 19 25))))
(assert-eq "a stop's color in its style" 0xffff0000
	(sv-at "<defs><linearGradient id=\qg\q><stop offset=\q0\q style=\qstop-color: red\q/></linearGradient></defs><rect x=\q10\q y=\q10\q width=\q20\q height=\q20\q fill=\qurl(#g)\q/>" 20 20))
(assert-true "stop-opacity and fill-opacity, half and half, a quarter"
	(<= 60 (>> (sv-at "<defs><linearGradient id=\qg\q><stop offset=\q0\q stop-color=\qwhite\q stop-opacity=\q0.5\q/><stop offset=\q1\q stop-color=\qwhite\q stop-opacity=\q0.5\q/></linearGradient></defs><rect x=\q10\q y=\q10\q width=\q20\q height=\q20\q fill=\qurl(#g)\q fill-opacity=\q0.5\q/>" 20 20) 24) 68))
(assert-eq "one not there, its fallback color" 0xffff0000
	(sv-at "<rect x=\q10\q y=\q10\q width=\q20\q height=\q20\q fill=\qurl(#none) red\q/>" 20 20))
(assert-eq "one not there and none to fall back on, nothing" 0
	(sv-at "<rect x=\q10\q y=\q10\q width=\q20\q height=\q20\q fill=\qurl(#none)\q/>" 20 20))
(assert-true "a stroke of a gradient"
	(sv-near 128 (logand 0xff (sv-at (cat "<defs><linearGradient id=\qg\q gradientUnits=\quserSpaceOnUse\q x1=\q0\q x2=\q40\q>" sv_bw "</linearGradient></defs><line x1=\q0\q y1=\q20.5\q x2=\q40\q y2=\q20.5\q stroke=\qurl(#g)\q stroke-width=\q4\q/>") 20 20))))
(assert-eq "a transform not known is no change, nothing thrown" 0xffff0000
	(sv-at "<rect x=\q10\q y=\q10\q width=\q20\q height=\q20\q fill=\qred\q transform=\qwobble(3)\q/>" 20 20))

;what took a node down, or threw, found by resvg's tests, 2026-11-23: each
;drawn, the rest of the picture with it
(defq sv_red (cat sv_box " fill=\qred\q/>"))
(assert-eq "a circle of a radius less than 0 draws nothing, the rest is drawn" (str '(0 0xffff0000))
	(str (map (# (sv-at (cat sv_red "<circle cx=\q5\q cy=\q5\q r=\q-4\q fill=\qblue\q/>") %0 %0)) '(4 20))))
(assert-eq "nor an ellipse of one, nor a rect of no height or a width less than 0" "(0 0 0)"
	(str (map (# (sv-at %0 4 4)) (list "<ellipse cx=\q5\q cy=\q5\q rx=\q-4\q ry=\q3\q/>"
		"<rect width=\q10\q/>" "<rect width=\q-10\q height=\q10\q/>"))))
(assert-eq "a polygon of an odd number of numbers, the last left out" 0xff00ff00
	(sv-at "<polygon points=\q0 0 40 0 0 40 99\q fill=\qlime\q/>" 5 5))
(assert-true "an svg of a size less than 0, the default size"
	(SVG-load (string-stream "<svg width=\q-50\q height=\q-100\q><rect width=\q10\q height=\q10\q/></svg>")))
(assert-eq "a path that can not be read draws nothing, the rest is drawn" 0xffff0000
	(sv-at (cat "<path d=\qM 0 0 L 40 0 L\q fill=\qblue\q/><path d=\qA 5 5 0 0 1 50 50\q/>" sv_red) 20 20))
(assert-eq "RGB() in capitals" 0xff00ff00 (sv-at (cat sv_box " fill=\qRGB(0, 255, 0)\q/>") 20 20))
(assert-eq "currentColor of a color on the same element" 0xff00ff00
	(sv-at (cat sv_box " fill=\qcurrentColor\q color=\qlime\q/>") 20 20))
(assert-eq "what is in a shape is not drawn" 0xff00ff00
	(sv-at (cat sv_box " fill=\qlime\q><rect width=\q40\q height=\q40\q fill=\qred\q/></rect>") 20 20))
(assert-eq "#qqq is no color, the parent's" 0xff0000ff
	(sv-at (cat "<g fill=\qblue\q>" sv_box " fill=\q#qqq\q/></g>") 20 20))
(assert-eq "white space of any kind about a value" 0xff00ff00
	(sv-at (cat sv_box " fill=\q\n\t lime \n\q/>") 20 20))
(assert-eq "an opacity with a unit is none given" 0xffff0000
	(sv-at (cat sv_box " fill=\qred\q opacity=\q0.1mm\q/>") 20 20))
(assert-eq "a gradient of no stops, its fallback" 0xff00ff00
	(sv-at (cat "<linearGradient id=\qn\q/>" sv_box " fill=\qurl(#n) lime\q/>") 20 20))
(assert-true "of the user's space, x2 100% of the viewport by default"
	(every (const sv-near) '(67 188)
		(map (# (sv-grad (cat "<linearGradient id=\qg\q gradientUnits=\quserSpaceOnUse\q>" sv_bw "</linearGradient>") "url(#g)" %0 20)) '(10 29))))

;the SVGs of the Images app, each drawn, nothing thrown
(each (lambda (name)
		(assert-true (cat name ".svg drawn")
			(SVG-load (file-stream (cat "apps/media/images/data/" name ".svg")))))
	'("burger" "chrysalisp" "clock" "dial" "golfer" "kennedy" "sticker" "tiger"))
