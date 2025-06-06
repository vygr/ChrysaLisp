;;;;;;;;;;;;
; svg parser
;;;;;;;;;;;;

(import "./parse.inc")
(import "gui/canvas/lisp.inc")

;module
(env-push)

(defun deg-to-rad (d)
	(/ (* d +fp_pi) 180.0))

(defmacro Mat3x2-f (m00 m01 m02 m10 m11 m12)
	`(fixeds ,m00 ,m01 ,m02 ,m10 ,m11 ,m12))

(defun Mat3x2-rotz-f (a)
	(defq ca (cos a) sa (sin a))
	(Mat3x2-f ca (* sa -1.0) 0.0 sa ca 0.0))

(defun Mat3x2-skewx-f (a)
	(defq ca (cos a) sa (sin a))
	(Mat3x2-f ca (* sa -1.0) 0.0 0.0 1.0 0.0))

(defun Mat3x2-skewy-f (a)
	(defq ca (cos a) sa (sin a))
	(Mat3x2-f 1.0 0.0 0.0 sa ca 0.0))

(defun mat3x2-mul-f (ma mb)
	; (mat3x2-mul-f mat3x2_a mat3x2_b) -> mat3x2-f
	(defq mar0 (slice ma 0 3) mar1 (slice ma 3 -1)
		mbr0 (slice mb 0 3) mbr1 (slice mb 3 -1)
		mbc0 (fixeds (first mbr0) (first mbr1))
		mbc1 (fixeds (second mbr0) (second mbr1))
		mbc2 (fixeds (third mbr0) (third mbr1) 1.0))
	(Mat3x2-f (nums-dot mbc0 mar0) (nums-dot mbc1 mar0) (nums-dot mbc2 mar0)
		(nums-dot mbc0 mar1) (nums-dot mbc1 mar1) (nums-dot mbc2 mar1)))

(defun read-col (val)
	(case (length val)
		(4 (canvas-to-argb32 (str-to-num (cat "0xf" (rest val))) 12))
		(7 (canvas-to-argb32 (str-to-num (cat "0x" (rest val))) 24))
		(:t (throw "Unknown # val !" val))))

(defun read-rgb (val)
	(bind '(r g b) (map (const str-to-num) (slice (split val (const (char-class "(), "))) 1 4)))
	(+ +argb_black (<< r 16) (<< g 8) b))

(defun read-transform (entry val)
	(defq stack (reverse (split val ")")) ma (. entry :find "transform"))
	(while (defq transform (pop stack))
		(defq args (split transform (const (char-class "(, \t\n")))
			key (first args)
			vals (map (# (n2f (str-to-num %0))) (rest args))
			mb (case key
			("matrix"
				(bind '(m00 m01 m10 m11 tx ty) vals)
				(Mat3x2-f m00 m01 tx m10 m11 ty))
			("translate"
				(bind '(tx &optional ty) vals)
				(setd ty 0.0)
				(Mat3x2-f 1.0 0.0 tx 0.0 1.0 ty))
			("scale"
				(bind '(sx &optional sy) vals)
				(setd sy sx)
				(Mat3x2-f sx 0.0 0.0 0.0 sy 0.0))
			("rotate"
				(bind '(a &optional tx ty) vals)
				(setd tx 0.0 ty 0.0)
				(defq mb (Mat3x2-rotz-f (deg-to-rad a)))
				(bind '(m00 m01 m02 m10 m11 m12) mb)
				(bind '(rx ry) (path-transform mb (path tx ty) (path 0 0)))
				(Mat3x2-f m00 m01 (- tx rx) m10 m11 (- ty ry)))
			("skewX"
				(bind '(a) vals)
				(Mat3x2-skewx-f (* (deg-to-rad a) -1.0)))
			("skewY"
				(bind '(a) vals)
				(Mat3x2-skewy-f (deg-to-rad a)))
			(:t (throw "Unknown transform !" key))))
		(setq ma (mat3x2-mul-f ma mb)))
	ma)

(defun read-attribute (entry key val)
	(case key
		(("x" "y" "x1" "y1" "x2" "y2" "rx" "ry" "cx" "cy" "r"
			"width" "height" "stroke-width" "fill-opacity" "stroke-opacity")
			(n2f (str-to-num val)))
		("viewBox"
			(map (# (n2f (str-to-num %0))) (split val (const (char-class ", ")))))
		("points"
			(reduce (# (push %0 (n2f (str-to-num %1)))) (split val (const (char-class ", "))) (path)))
		("transform"
			(read-transform entry val))
		(("stroke" "fill")
			(case val
				("none" 0)
				("transparent" 0)
				("black" +argb_black)
				("white" +argb_white)
				("red" +argb_red)
				("green" +argb_green)
				("blue" +argb_blue)
				("yellow" +argb_yellow)
				("magenta" +argb_magenta)
				("cyan" +argb_cyan)
				(:t (cond
					((starts-with "#" val) (read-col val))
					((starts-with "rgb(" val) (read-rgb val))
					((throw "Unknown stroke/fill style !" val))))))
		("stroke-linecap"
			(case val
				("butt" +cap_butt)
				("round" +cap_round)
				("square" +cap_square)
				((throw "Unknown stroke-linecap style !" val))))
		("stroke-linejoin"
			(case val
				("miter" +join_miter)
				("bevel" +join_bevel)
				("round" +join_round)
				("miter-clip" +join_miter)
				("arcs" +join_miter)
				((throw "Unknown stroke-linejoin style !" val))))
		("fill-rule"
			(case val
				("nonzero" +winding_none_zero)
				("evenodd" +winding_odd_even)
				((throw "Unknown fill-rule !" val))))
		("font-weight"
			(case val
				("normal" "Regular")
				("bold" "Bold")
				("bolder" "Bolder")
				("lighter" "Lighter")
				((throw "Unknown font-weight !" val))))
		("font-size"
			(n2f (str-to-num (pop (split val "pt")))))
		("font-family"
			"OpenSans")
		("text-anchor"
			(case val
				("start" "start")
				("middle" "middle")
				("end" "end")
				((throw "Unknown text-anchor !" val))))
		("dominant-baseline"
			(case val
				("auto" "auto")
				("middle" "middle")
				("hanging" "hanging")
				((throw "Unknown dominant-baseline !" val))))
		("d"
			(path-gen-paths val))
		(:t val)))

(defun push-attributes (stack keys vals)
	(push stack (defq entry (. (last stack) :copy)))
	(each (lambda (key val)
		(cond
			((eql key "style")
				(each (lambda ((k v))
						(. entry :insert k (read-attribute entry k v)))
					(map (# (split %0 ":")) (split val ";"))))
			(:t (setq val (read-attribute entry key val))))
		(. entry :insert key val)) keys vals)
	entry)

(defun path-fill-and-stroke (canvas entry paths)
	(bind '(stroke fill stroke_width stroke_linecap stroke_linejoin
			fill_rule fill_opacity stroke_opacity transform &rest _) (get :vals entry))
	(when (and (/= fill 0) (/= fill_opacity 0.0))
		(.-> canvas
			(:set_color (+ (<< (n2i (* fill_opacity 0xff.0)) 24) (logand fill 0xffffff)))
			(:fpoly 0.0 0.0 fill_rule
				(map (lambda ((f p)) (path-transform transform p (cat p))) paths))))
	(when (and (/= stroke 0) (/= stroke_opacity 0.0))
		(.-> canvas
			(:set_color (+ (<< (n2i (* stroke_opacity 0xff.0)) 24) (logand stroke 0xffffff)))
			(:fpoly 0.0 0.0 fill_rule
				(map (lambda (p) (path-transform transform p p))
					(reduce (lambda (l (f p))
						(cond
							(f  ;closed
								(bind '(p1 p2) (path-stroke-polygon p (* 0.5 stroke_width)
									stroke_linejoin))
								(push l p1 p2))
							(:t ;open
								(push l (path-stroke-polyline p (* 0.5 stroke_width)
									stroke_linejoin stroke_linecap stroke_linecap)))))
							paths (list)))))))

(defun SVG-Canvas (stream &optional scale)
	; (SVG-Canvas stream [scale]) -> :nil | canvas
	(setq scale (n2f (ifn scale 1)))
	(defq canvas :nil stack
		(list (scatter (Lmap)
			;this block of defaults are in order !
			"stroke" 0
			"fill" +argb_black
			"stroke-width" 1.0
			"stroke-linecap" +cap_butt
			"stroke-linejoin" +join_miter
			"fill-rule" +winding_none_zero
			"fill-opacity" 1.0
			"stroke-opacity" 1.0
			"transform" (Mat3x2-f scale 0.0 0.0 0.0 scale 0.0)
			;this block can be any order
			"d" (path) "points" (path)
			"x" 0.0 "y" 0.0 "x1" 0.0 "y1" 0.0 "x2" 0.0 "y2" 0.0
			"cx" 0.0 "cy" 0.0 "r" 0.0
			"text-anchor" "start" "dominant-baseline" "auto"
			"font-size" 16.0 "font-family" "OpenSans" "font-weight" "Regular"
			)))
	(XML-parse stream
		(lambda (command keys vals)
			(case command
				("svg"
					(bind '(w h vb) (gather (defq entry (push-attributes stack keys vals))
						"width" "height" "viewBox"))
					(defq x 0.0 y 0.0) (setd w 256.0 h 256.0)
					(if vb (bind '(x y w h) vb))
					(. (setq canvas (Canvas (n2i w) (n2i h) (n2i scale)))
						:set_canvas_flags +canvas_flag_antialias)
					(. entry :insert "transform" (Mat3x2-f scale 0.0 (* scale x -1.0) 0.0 scale (* scale y -1.0))))
				("circle"
					(bind '(cx cy r) (gather (defq entry (push-attributes stack keys vals))
						"cx" "cy" "r"))
					(defq d (path-gen-arc cx cy 0.0 +fp_2pi r (path)))
					(path-fill-and-stroke canvas entry (list (list :t d))))
				("ellipse"
					(bind '(cx cy rx ry) (gather (defq entry (push-attributes stack keys vals))
						"cx" "cy" "rx" "ry"))
					(setd rx ry ry rx)
					(defq d (path-gen-ellipse cx cy rx ry (path)))
					(path-fill-and-stroke canvas entry (list (list :t d))))
				("line"
					(bind '(x1 y1 x2 y2) (gather (defq entry (push-attributes stack keys vals))
						"x1" "y1" "x2" "y2"))
					(defq d (path x1 y1 x2 y2))
					(path-fill-and-stroke canvas entry (list (list :nil d))))
				("rect"
					(bind '(x y rx ry x1 y1) (gather (defq entry (push-attributes stack keys vals))
						"x" "y" "rx" "ry" "width" "height"))
					(setq x1 (+ x x1) y1 (+ y y1))
					(setd rx ry ry rx)
					(defq d (path-gen-rect x y x1 y1 rx ry (path)))
					(path-fill-and-stroke canvas entry (list (list :t d))))
				("polygon"
					(defq entry (push-attributes stack keys vals)
						d (. entry :find "points"))
					(path-fill-and-stroke canvas entry (list (list :t d))))
				("polyline"
					(defq entry (push-attributes stack keys vals)
						d (. entry :find "points"))
					(path-fill-and-stroke canvas entry (list (list :nil d))))
				("path"
					(defq entry (push-attributes stack keys vals)
						d (. entry :find "d"))
					(path-fill-and-stroke canvas entry d))
				(("text" "g")
					(push-attributes stack keys vals))
				))
		(lambda (command)
			(case command
				(("svg" "g" "circle" "ellipse" "path" "polygon" "polyline" "line" "rect" "text")
					(pop stack))
				))
		(lambda (text)
			;render text
			(bind '(x y family weight size anchor baseline)
				(gather (defq entry (last stack))
					"x" "y" "font-family" "font-weight" "font-size"
					"text-anchor" "dominant-baseline"))
			(defq font (create-font (cat "fonts/" family "-" weight ".ctf") (n2i size)))
			(setd font (create-font (cat "fonts/" family "-Regular.ctf") (n2i size)))
			(setd font (create-font "fonts/OpenSans-Regular.ctf" (n2i size)))
			(bind '(w h) (font-glyph-bounds font text))
			(defq ox (case anchor
					("middle" (* (n2f w) -1.0))
					("end" (* (n2f w) -2.0))
					(:t 0.0))
				oy (case baseline
					("middle" (* (n2f h) 0.5))
					("hanging" (n2f h))
					(:t 0.0))
				d (map (# (list :t (path-transform
						(Mat3x2-f 1.0 0.0 (+ x ox) 0.0 1.0 (+ y oy)) %0 %0)))
					(font-glyph-paths font text)))
			(path-fill-and-stroke canvas entry d)))
	;resize to final output
	(bind '(w h) (. canvas :pref_size))
	(. (Canvas w h 1) :resize canvas))

(defun SVG-info (stream)
	; (SVG-info stream) -> (width height type) | (-1 -1 -1)
	(defq stack (list (Lmap)) width -1 height -1)
	(XML-parse stream
		(lambda (command keys vals)
			(when (eql command "svg")
				(bind '(w h vb) (gather (defq entry (push-attributes stack keys vals))
					"width" "height" "viewBox"))
				(setd w 256.0 h 256.0)
				(if vb (bind '(_ _ w h) vb))
				(setq width w height h)))
		(lambda (command))
		(lambda (text)))
	(list width height 32))

;module
(export-symbols '(SVG-Canvas SVG-info))
(env-pop)
