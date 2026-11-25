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
(assert-eq "and over it whichever is written first, as CSS has it" 0xff00ff00
	(sv-at (cat sv_box " style=\qfill:lime\q fill=\qred\q/>") 20 20))
(assert-eq "a style's display over the attribute's, that says none" 0xffff0000
	(sv-at (cat sv_box " display=\qnone\q style=\qdisplay:inline\q fill=\qred\q/>") 20 20))
(assert-eq "and its none over the attribute's" 0
	(sv-at (cat sv_box " display=\qinline\q style=\qdisplay:none\q fill=\qred\q/>") 20 20))
(assert-eq "a rect's width in its style over its attribute" 0
	(sv-at "<rect x=\q10\q y=\q10\q width=\q20\q height=\q20\q style=\qwidth:5\q fill=\qred\q/>" 20 20))

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
;a radial gradient's focus, fx fy: its first color there, not at its centre
(defq sv_focal "<defs><radialGradient id=\qg\q fx=\q75%\q><stop offset=\q0\q stop-color=\q#f00\q/><stop offset=\q1\q stop-color=\q#00f\q/></radialGradient></defs><rect width=\q40\q height=\q40\q fill=\qurl(#g)\q/>")
(assert-true "a radialGradient's fx, its first color at the focus" (< (logand (sv-at sv_focal 30 20) 0xff) 16))
(assert-true "and at its centre a third of the way to its last" (<= 70 (logand (sv-at sv_focal 20 20) 0xff) 100))
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

;an svg in another: a viewport of its own, its viewBox fitted in it, what
;is in it kept to it unless overflow is visible
(assert-eq "a nested svg, its viewBox of 10 fitted to its 20 at 10 10" "(0 4278255360 4278255360 0)"
	(str (map (# (sv-at "<svg x=\q10\q y=\q10\q width=\q20\q height=\q20\q viewBox=\q0 0 10 10\q><rect width=\q10\q height=\q10\q fill=\qlime\q/></svg>" %0 %0))
		'(5 15 29 35))))
(assert-eq "what is in it kept to it" "(4278255360 0)"
	(str (map (# (sv-at "<svg x=\q10\q y=\q10\q width=\q10\q height=\q10\q><rect width=\q40\q height=\q40\q fill=\qlime\q/></svg>" %0 %0))
		'(15 25))))
(assert-eq "unless its overflow is visible, and the clip it was in back after it" "(4278255360 4278190335)"
	(str (map (# (sv-at "<svg x=\q10\q y=\q10\q width=\q10\q height=\q10\q overflow=\qvisible\q><rect width=\q40\q height=\q40\q fill=\qlime\q/></svg><rect x=\q34\q y=\q34\q width=\q6\q height=\q6\q fill=\qblue\q/>" %0 %0))
		'(25 37))))
(assert-eq "a symbol by a use, the use's size its viewport" "(0 4278255360 0)"
	(str (map (# (sv-at "<symbol id=\qs\q viewBox=\q0 0 10 10\q><rect width=\q10\q height=\q10\q fill=\qlime\q/></symbol><use href=\q#s\q x=\q10\q y=\q10\q width=\q20\q height=\q20\q/>" %0 %0))
		'(5 25 35))))

;a switch: the first child that may be drawn, and one not shown is chosen
(assert-eq "a switch draws the first that passes" 0xff00ff00
	(sv-at (cat "<switch><rect systemLanguage=\qru\q width=\q40\q height=\q40\q fill=\qred\q/>"
		"<rect width=\q40\q height=\q40\q fill=\qlime\q/><rect width=\q40\q height=\q40\q fill=\qblue\q/></switch>") 20 20))
(assert-eq "of English" 0xff00ff00
	(sv-at "<switch><rect systemLanguage=\qru, en-GB\q width=\q40\q height=\q40\q fill=\qlime\q/><rect width=\q40\q height=\q40\q fill=\qred\q/></switch>" 20 20))
(assert-eq "a first not shown is chosen, nothing drawn" 0
	(sv-at "<switch><rect display=\qnone\q width=\q40\q height=\q40\q fill=\qred\q/><rect width=\q40\q height=\q40\q fill=\qred\q/></switch>" 20 20))

;a use of what is open, or of a use being drawn, is not drawn
(assert-eq "a use round to itself through another, drawn the once" 0xff00ff00
	(sv-at (cat "<g id=\qa\q><rect width=\q40\q height=\q40\q fill=\qlime\q/><use href=\q#b\q/></g>"
		"<g id=\qb\q><use href=\q#a\q/></g>") 20 20))

;text, 2026-11-23: of its size, as much as a browser draws it, where its
;glyphs were twice that; runs one after another, a chunk placed by its
;anchor whole; faces of the .ctf fonts there are
(defq sv_head "<svg width=\q200\q height=\q200\q>")
(defun sv-ink (svg &optional head)
	; (sv-ink svg [head]) -> (x y x1 y1) | :nil
	;the box of what is drawn more than half covered
	(bind '(d off w) (sv-pixels svg (ifn head sv_head)))
	(defq x0 w y0 w x1 -1 y1 -1 i 0 n (/ (- (length d) off) +int_size))
	(while (< i n)
		(when (> (get-uint d (+ off (* i +int_size))) 0x80ffffff)
			(defq x (% i w) y (/ i w))
			(setq x0 (min x0 x) y0 (min y0 y) x1 (max x1 x) y1 (max y1 y)))
		(++ i))
	(if (>= x1 0) (list x0 y0 x1 y1)))
(defun sv-close (a b most) (<= (abs (- a b)) most))
(defun sv-text (body &optional attrs)
	(cat "<text x=\q20\q y=\q150\q font-size=\q100\q " (ifn attrs "") ">" body "</text>"))
(bind '(& hy & hy1) (sv-ink (sv-text "H")))
(assert-true "an H of size 100 is 71 tall, its cap height" (sv-close (- hy1 hy) 71 2))
(bind '(& sy & sy1) (sv-ink "<text x=\q0.2\q y=\q1.5\q font-size=\q1\q>H</text>"
	"<svg width=\q200\q height=\q200\q viewBox=\q0 0 2 2\q>"))
(assert-true "and of size 1 in a viewBox of 2 drawn at 200, the same" (sv-close (- sy1 sy) 71 2))
(defun sv-same (a b &optional most)
	;the same within a pixel for each place runs meet, a run's width a
	;whole number of the font's pixels
	(every (# (sv-close %0 %1 (ifn most 1))) a b))
(assert-true "a tspan goes on from where the run before it ends, within a pixel"
	(sv-same (sv-ink "<text x=\q10\q y=\q150\q font-size=\q40\q>Hello</text>")
		(sv-ink "<text x=\q10\q y=\q150\q font-size=\q40\q>He<tspan>llo</tspan></text>")))
(bind '(mx & mx1 &ignore) (sv-ink "<text x=\q100\q y=\q150\q font-size=\q40\q text-anchor=\qmiddle\q>He<tspan>llo</tspan></text>"))
(assert-true "a chunk of runs placed by its anchor whole" (sv-close (+ mx mx1) 200 4))
(assert-true "a space between runs is kept, once"
	(sv-same (sv-ink "<text x=\q10\q y=\q150\q font-size=\q20\q>Hello wide world</text>")
		(sv-ink "<text x=\q10\q y=\q150\q font-size=\q20\q>Hello <tspan> wide </tspan> world</text>") 2))
(assert-true "and kept as it is where xml:space is preserve"
	(> (third (sv-ink "<text x=\q10\q y=\q150\q font-size=\q20\q xml:space=\qpreserve\q>A     B</text>"))
		(third (sv-ink "<text x=\q10\q y=\q150\q font-size=\q20\q>A     B</text>"))))
(bind '(tx & & ty1) (sv-ink "<text x=\q10\q y=\q100\q font-size=\q40\q><tspan x=\q100\q y=\q180\q>H</tspan></text>"))
(assert-true "a tspan's own x and y begin a chunk there, after the font's gap"
	(and (<= 100 tx 106) (= (inc ty1) 180)))
(assert-true "a list of x, one for each character"
	(>= (third (sv-ink "<text x=\q20 120\q y=\q150\q font-size=\q40\q>HH</text>")) 140))
(assert-eq "a dy of a list moves the second down" 30
	(- (elem-get (sv-ink "<text x=\q20 120\q y=\q150\q dy=\q0 30\q font-size=\q40\q>HH</text>") 3)
		(elem-get (sv-ink "<text x=\q20 120\q y=\q150\q font-size=\q40\q>HH</text>") 3)))
(assert-true "italic is a face of its own"
	(nql (str (sv-pixels (sv-text "H" "font-style=\qitalic\q") sv_head)) (str (sv-pixels (sv-text "H") sv_head))))
(assert-eq "a weight of 700 is bold" (str (sv-pixels (sv-text "H" "font-weight=\qbold\q") sv_head))
	(str (sv-pixels (sv-text "H" "font-weight=\q700\q") sv_head)))
(assert-true "a mono family is Hack, its i as wide as its m"
	(> (third (sv-ink (sv-text "iiii" "font-family=\qmonospace\q font-size=\q20\q")))
		(third (sv-ink (sv-text "iiii" "font-family=\qsans-serif\q font-size=\q20\q")))))
(each (lambda (size name)
		(bind '(& y & y1) (sv-ink (cat "<g font-size=\q50\q><text x=\q20\q y=\q150\q font-size=\q" size "\q>H</text></g>")))
		(assert-true (cat "a font-size of " name) (sv-close (- y1 y) 71 2)))
	'("2em" "200%") '("2em" "200%"))
(bind '(& ky & ky1) (sv-ink (sv-text "H" "font-size=\qxx-large\q")))
(assert-true "of xx-large, 32" (sv-close (- ky1 ky) 23 2))
(assert-eq "a text in a text is not drawn" :nil (sv-ink "<text x=\q10\q y=\q150\q><text>H</text></text>"))
(bind '(& cy & cy1) (sv-ink (sv-text "H" "dominant-baseline=\qcentral\q")))
(assert-true "a central baseline lowers it" (> cy (+ hy 20)))

;decorations, of the element that names them, and letter and word spacing
(defun sv-deco (deco &optional inner)
	(sv-ink (cat "<text x=\q20\q y=\q100\q font-size=\q40\q " deco ">H" (ifn inner "") "</text>")))
(assert-true "an underline is below the baseline"
	(> (elem-get (sv-deco "text-decoration=\qunderline\q") 3) 102))
(assert-true "an overline just over the H, whose top is at 71"
	(<= 65 (second (sv-deco "text-decoration=\qoverline\q")) 69))
(defun sv-gaps (svg y)
	; (sv-gaps svg y) -> (x ...)
	;the empty pixels of row y between its first and last that are not
	(bind '(d off w) (sv-pixels svg sv_head))
	(defq row (map! (# (>> (get-uint d (+ off (* +int_size (+ (* y w) (!))))) 24)) '() 0 w)
		inked (filter (# (> (elem-get row %0) 0)) (map! (const !) '() 0 w)))
	(filter (# (= (elem-get row %0) 0)) (map! (const !) '() (first inked) (last inked))))
(defq sv_gaps (sv-gaps "<text x=\q20\q y=\q100\q font-size=\q40\q>HH</text>" 90))
(assert-true "two H's have a gap between them" (nempty? sv_gaps))
(assert-eq "a line-through fills it" '()
	(filter (# (/= (sv-at "<text x=\q20\q y=\q100\q font-size=\q40\q text-decoration=\qline-through\q>HH</text>" %0 90 sv_head) 0xff000000)) sv_gaps))
(assert-eq "an underline of a group is of the group's paint and size, under a tspan of its own" 0xff00ff00
	(sv-at "<g text-decoration=\qunderline\q fill=\qlime\q font-size=\q40\q><text x=\q20\q y=\q100\q><tspan fill=\qred\q>HH</tspan></text></g>" 40 104 sv_head))
(defq sv_hhhh (sv-ink "<text x=\q20\q y=\q100\q font-size=\q20\q>HHHH</text>"))
(assert-true "letter-spacing of 10 widens four letters by 30, within a pixel each"
	(sv-close (- (third (sv-ink "<text x=\q20\q y=\q100\q font-size=\q20\q letter-spacing=\q10\q>HHHH</text>")) (third sv_hhhh)) 30 4))
(assert-true "word-spacing of 20 widens a space by 20"
	(sv-close (- (third (sv-ink "<text x=\q20\q y=\q100\q font-size=\q20\q word-spacing=\q20\q>H H</text>"))
		(third (sv-ink "<text x=\q20\q y=\q100\q font-size=\q20\q>H H</text>"))) 20 2))

;rotate, a character turned about its place, the last angle going on
(bind '(lx ly lx1 ly1) (sv-ink "<text x=\q60\q y=\q100\q font-size=\q60\q>l</text>"))
(bind '(rx ry rx1 ry1) (sv-ink "<text x=\q60\q y=\q100\q font-size=\q60\q rotate=\q90\q>l</text>"))
(assert-true "an l is tall, turned by 90 it lies on its side"
	(and (> (- ly1 ly) (* 4 (- lx1 lx))) (> (- rx1 rx) (* 4 (- ry1 ry)))))
(assert-true "clockwise, its top to the right of its place, along the baseline" (and (>= rx 60) (> rx1 100) (> ry 100)))
(assert-eq "the last angle goes on: all three turned as each one is" (str (sv-ink "<text x=\q20\q y=\q100\q font-size=\q40\q rotate=\q90 90 90\q>lll</text>"))
	(str (sv-ink "<text x=\q20\q y=\q100\q font-size=\q40\q rotate=\q90\q>lll</text>")))
(assert-eq "a rotate with a unit is none" (str (sv-ink "<text x=\q20\q y=\q100\q font-size=\q40\q>Text</text>"))
	(str (sv-ink "<text x=\q20\q y=\q100\q font-size=\q40\q rotate=\q5mm\q>Text</text>")))

;textPath: each character's middle at its distance along the path, its
;baseline on it and turned to it, off either end not drawn
(defun sv-tp (d text &optional attrs)
	(sv-ink (cat "<defs><path id=\qp\q d=\q" d "\q/></defs><text font-size=\q40\q><textPath href=\q#p\q "
		(ifn attrs "") ">" text "</textPath></text>")))
(assert-true "along a level path as it would be on that baseline, within a pixel for each letter"
	(sv-same (sv-tp "M 20 100 L 190 100" "HHH") (sv-ink "<text x=\q20\q y=\q100\q font-size=\q40\q>HHH</text>") 3))
(bind '(vx vy vx1 vy1) (sv-tp "M 100 20 L 100 190" "HHH"))
(assert-true "down a path the letters go down it, on their sides" (> (- vy1 vy) (* 2 (- vx1 vx))))
(bind '(cx & cx1 &ignore) (sv-tp "M 20 100 L 180 100" "HHH" "startOffset=\q50%\q text-anchor=\qmiddle\q"))
(assert-true "a startOffset of 50% and an anchor of middle centre it" (sv-close (+ cx cx1) 200 4))
(assert-true "a letter whose middle is past the end is not drawn: two H's of six on 40"
	(<= 65 (third (sv-tp "M 20 100 L 60 100" "HHHHHH")) 76))
(assert-eq "nor what has no path" :nil (sv-tp "" "HHH" "href=\q#none\q"))
;baseline-shift, of a tspan, up by its length, sub and super by the font's
(bind '(& py &ignore) (sv-ink "<text x=\q20\q y=\q100\q font-size=\q40\q>H</text>"))
(bind '(& sy &ignore) (sv-ink "<text x=\q20\q y=\q100\q font-size=\q40\q><tspan baseline-shift=\qsuper\q>H</tspan></text>"))
(bind '(& by &ignore) (sv-ink "<text x=\q20\q y=\q100\q font-size=\q40\q><tspan baseline-shift=\qsub\q>H</tspan></text>"))
(bind '(& ly &ignore) (sv-ink "<text x=\q20\q y=\q100\q font-size=\q40\q><tspan baseline-shift=\q10\q>H</tspan></text>"))
(assert-true "a super raises it, by 0.48 of its size" (sv-close (- py sy) 19 1))
(assert-true "a sub lowers it, by 0.14" (sv-close (- by py) 6 1))
(assert-true "a length raises it by that" (sv-close (- py ly) 10 1))
(assert-eq "on a text it is nothing" py
	(second (sv-ink "<text x=\q20\q y=\q100\q font-size=\q40\q baseline-shift=\qsuper\q>H</text>")))

;textArea, SVG Tiny's: words in lines no wider than it, a tbreak a line,
;placed across and down, what is below its height not drawn
(defun sv-ta (attrs text)
	(sv-ink (cat "<textArea x=\q20\q y=\q20\q font-size=\q20\q " attrs ">" text "</textArea>")))
(defq sv_words "one two three four five six seven eight nine ten")
(bind '(wx wy wx1 wy1) (sv-ta "width=\q100\q" sv_words))
(assert-true "it wraps, no wider than its width, in lines" (and (<= wx1 120) (> (- wy1 wy) 60)))
(bind '(& oy & oy1) (sv-ta "" sv_words))
(assert-true "of no width, one line" (< (- oy1 oy) 30))
(bind '(& by & by1) (sv-ta "width=\q160\q" "one<tbreak/>two"))
(assert-true "a tbreak begins a line" (> (- by1 by) 30))
(assert-true "text-align end, against its right"
	(sv-close (third (sv-ta "width=\q160\q text-align=\qend\q" "end")) 180 2))
(assert-true "display-align after, against its foot"
	(sv-close (elem-get (sv-ta "width=\q160\q height=\q100\q display-align=\qafter\q" "foot") 3) 115 6))
(assert-true "what is below its height is not drawn"
	(<= (elem-get (sv-ta "width=\q100\q height=\q50\q" sv_words) 3) 70))
(assert-true "a word in two elements with no space between is one word"
	(sv-same (sv-ta "width=\q160\q" "abc") (sv-ta "width=\q160\q" "ab<tspan>c</tspan>") 2))

;the SVGs of the Images app, each drawn, nothing thrown
(each (lambda (name)
		(assert-true (cat name ".svg drawn")
			(SVG-load (file-stream (cat "apps/media/images/data/" name ".svg")))))
	'("burger" "chrysalisp" "clock" "dial" "golfer" "gradients" "kennedy" "sticker" "text" "tiger"))
