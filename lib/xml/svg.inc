;;;;;;;;;;;;
; svg parser
;;;;;;;;;;;;

(import "./parse.inc")
(import "gui/canvas/lisp.inc")

;module
(env-push)

(defun Mat3x3-f (m00 m01 m02 m10 m11 m12 m20 m21 m22)
	(list (fixeds m00 m01 m02)
		(fixeds m10 m11 m12)
		(fixeds m20 m21 m22)))

(defun mat3x3-mul-f (ma mb)
	; (mat3x3-mul-f mat3x3_a mat3x3_b) -> mat3x3-f
	(bind '(mar0 mar1 mar2) ma)
	(bind '(mbr0 mbr1 mbr2) mb)
	(defq mbc0 (fixeds (elem 0 mbr0) (elem 0 mbr1) (elem 0 mbr2))
		mbc1 (fixeds (elem 1 mbr0) (elem 1 mbr1) (elem 1 mbr2))
		mbc2 (fixeds (elem 2 mbr0) (elem 2 mbr1) (elem 2 mbr2)))
	(list (fixeds (nums-dot mar0 mbc0) (nums-dot mar0 mbc1) (nums-dot mar0 mbc2))
		(fixeds (nums-dot mar1 mbc0) (nums-dot mar1 mbc1) (nums-dot mar1 mbc2))
		(fixeds (nums-dot mar2 mbc0) (nums-dot mar2 mbc1) (nums-dot mar2 mbc2))))

(defq +eps 0.25
	+command_chars "MLHVCSQTAZmlhvcsqtaz"
	+num_start_chars "-+0123456789"
	+num_mid_chars ".0123456789")

(defmacro is-command-char? (_)
	(list 'find-rev _ +command_chars))

(defmacro is-num-start-char? (_)
	(list 'find-rev _ +num_start_chars))

(defmacro is-num-mid-char? (_)
	(list 'find-rev _ +num_mid_chars))

(defmacro is-whitespace-char? (_)
	`(<= (code ,_) +char_space))

(defun read-num (s)
	(elem 0 (read (string-stream s) +char_space)))

(defun col-to-num (val)
	(case (length val)
		(4 (canvas-to-argb32 (read-num (cat "0xf" (slice 1 -1 val))) 12))
		(7 (canvas-to-argb32 (read-num (cat "0x" (slice 1 -1 val))) 24))
		(t (throw "Unknown # val !" val))))

(defun rgb-to-num (val)
	(bind '(r g b) (map (const read-num) (slice 1 4 (split val "(), "))))
	(+ +argb_black (<< r 16) (<< g 8) b))

(defun parse-svg-path (d)
	(defq commands (list) token nil state :text)
	(each (lambda (c)
		(while c
			(case state
				(:text
					(cond
						((is-whitespace-char? c))
						((eql c (ascii-char +char_comma)))
						((is-command-char? c)
							(push commands c))
						((is-num-start-char? c)
							(setq token (list c) state :number))
						(t (throw "Unknown path char !" c)))
					(setq c nil))
				(:number
					(cond
						((is-num-mid-char? c)
							(push token c)
							(setq c nil))
						(t  (push commands (n2f (read-num (apply cat token))))
							(setq state :text))))
			))) (cat d " "))
	(push commands "x"))

(defun gen-svg-path (d)
	(defq i 0 paths (list) d (parse-svg-path d)
		cx 0.0 cy 0.0 cpx 0.0 cpy 0.0 last_cmd " ")
	(until (eql (defq cmd (elem i d)) "x")
		(setq i (inc i))
		(case cmd
			("M"
				(push paths (list nil (defq p (path))))
				(while (num? (elem i d))
					(bind '(cpx cpy) (slice i (setq i (+ i 2)) d))
					(push p cpx cpy)))
			("m"
				(push paths (list nil (defq p (path))))
				(while (num? (elem i d))
					(bind '(x y) (slice i (setq i (+ i 2)) d))
					(setq cpx (+ cpx x) cpy (+ cpy y))
					(push p cpx cpy)))
			("L"
				(while (num? (elem i d))
					(bind '(cpx cpy) (slice i (setq i (+ i 2)) d))
					(push p cpx cpy)))
			("l"
				(while (num? (elem i d))
					(bind '(x y) (slice i (setq i (+ i 2)) d))
					(setq cpx (+ cpx x) cpy (+ cpy y))
					(push p cpx cpy)))
			("H"
				(while (num? (elem i d))
					(bind '(cpx) (slice i (setq i (+ i 1)) d))
					(push p cpx cpy)))
			("h"
				(while (num? (elem i d))
					(bind '(x) (slice i (setq i (+ i 1)) d))
					(setq cpx (+ cpx x))
					(push p cpx cpy)))
			("V"
				(while (num? (elem i d))
					(bind '(cpy) (slice i (setq i (+ i 1)) d))
					(push p cpx cpy)))
			("v"
				(while (num? (elem i d))
					(bind '(y) (slice i (setq i (+ i 1)) d))
					(setq cpy (+ cpy y))
					(push p cpx cpy)))
			("C"
				(while (num? (elem i d))
					(bind '(x1 y1 x2 y2 x y) (slice i (setq i (+ i 6)) d))
					(path-gen-cubic cpx cpy x1 y1 x2 y2 x y +eps p)
					(setq cpx x cpy y cx x2 cy y2)))
			("c"
				(while (num? (elem i d))
					(bind '(x1 y1 x2 y2 x y) (slice i (setq i (+ i 6)) d))
					(setq x1 (+ cpx x1) y1 (+ cpy y1)
						x2 (+ cpx x2) y2 (+ cpy y2)
						x (+ cpx x) y (+ cpy y))
					(path-gen-cubic cpx cpy x1 y1 x2 y2 x y +eps p)
					(setq cpx x cpy y cx x2 cy y2)))
			("S"
				(while (num? (elem i d))
					(bind '(x2 y2 x y) (slice i (setq i (+ i 4)) d))
					(if (find last_cmd "CScs")
						(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
						(defq x1 cpx y1 cpy))
					(path-gen-cubic cpx cpy x1 y1 x2 y2 x y +eps p)
					(setq cpx x cpy y cx x2 cy y2)))
			("s"
				(while (num? (elem i d))
					(bind '(x2 y2 x y) (slice i (setq i (+ i 4)) d))
					(setq x2 (+ cpx x2) y2 (+ cpy y2)
						x (+ cpx x) y (+ cpy y))
					(if (find last_cmd "CScs")
						(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
						(defq x1 cpx y1 cpy))
					(path-gen-cubic cpx cpy x1 y1 x2 y2 x y +eps p)
					(setq cpx x cpy y cx x2 cy y2)))
			("Q"
				(while (num? (elem i d))
					(bind '(x1 y1 x y) (slice i (setq i (+ i 4)) d))
					(path-gen-quadratic cpx cpy x1 y1 x y +eps p)
					(setq cpx x cpy y cx x1 cy y1)))
			("q"
				(while (num? (elem i d))
					(bind '(x1 y1 x y) (slice i (setq i (+ i 4)) d))
					(setq x1 (+ cpx x1) y1 (+ cpy y1)
						x (+ cpx x) y (+ cpy y))
					(path-gen-quadratic cpx cpy x1 y1 x y +eps p)
					(setq cpx x cpy y cx x1 cy y1)))
			("T"
				(while (num? (elem i d))
					(bind '(x y) (slice i (setq i (+ i 2)) d))
					(if (find last_cmd "QTqt")
						(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
						(defq x1 cpx y1 cpy))
					(path-gen-quadratic cpx cpy x1 y1 x y +eps p)
					(setq cpx x cpy y cx x1 cy y1)))
			("t"
				(while (num? (elem i d))
					(bind '(x y) (slice i (setq i (+ i 2)) d))
					(setq x (+ cpx x) y (+ cpy y))
					(if (find last_cmd "QTqt")
						(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
						(defq x1 cpx y1 cpy))
					(path-gen-quadratic cpx cpy x1 y1 x y +eps p)
					(setq cpx x cpy y cx x1 cy y1)))
			("A"
				(while (num? (elem i d))
					(throw "Not implemted path command !" cmd)
					(bind '(rx ry x_axis_rotation large_arc_flag sweep_flag x y)
						(slice i (setq i (+ i 7)) d))))
			("a"
				(while (num? (elem i d))
					(throw "Not implemted path command !" cmd)
					(bind '(rx ry x_axis_rotation large_arc_flag sweep_flag dx dy)
						(slice i (setq i (+ i 7)) d))))
			(("Z" "z")
				(bind '(cpx cpy) (slice 0 2 p))
				(push p cpx cpy)
				(elem-set 0 (elem -2 paths) t)))
		(setq last_cmd cmd))
	paths)

(defun read-attribute (entry key val)
	(case key
		(("x" "y" "x1" "y1" "x2" "y2" "rx" "ry" "cx" "cy" "r"
			"width" "height" "stroke-width" "fill-opacity" "stroke-opacity")
			(n2f (read-num val)))
		("viewBox"
			(map (# (n2i (read-num %0))) (split val ", ")))
		("points"
			(reduce (# (push %0 (n2f (read-num %1)))) (split val ", ") (path)))
		("transform"
			(bind '(m00 m01 m10 m11 tx ty) (. entry :find "transform"))
			(defq ma (Mat3x3-f m00 m01 tx m10 m11 ty 0.0 0.0 1.0))
			(bind '(m00 m01 m10 m11 tx ty) (map (# (n2f (read-num %0)))
				(split (slice (inc (find "(" val)) (find-rev ")" val) val) ", ")))
			(bind '((m00 m01 tx) (m10 m11 ty) _)
				(mat3x3-mul-f ma (Mat3x3-f m00 m01 tx m10 m11 ty 0.0 0.0 1.0)))
			(list m00 m01 m10 m11 tx ty))
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
				(t (cond
					((starts-with "#" val) (col-to-num val))
					((starts-with "rgb(" val) (rgb-to-num val))
					(t (throw "Unknown stroke/fill style !" val))))))
		(("stroke-linecap")
			(case val
				("butt" +cap_butt)
				("round" +cap_round)
				("square" +cap_square)
				(t (throw "Unknown stroke-linecap style !" val))))
		(("stroke-linejoin")
			(case val
				("miter" +join_miter)
				("bevel" +join_bevel)
				("round" +join_round)
				("miter-clip" +join_miter)
				("arcs" +join_miter)
				(t (throw "Unknown stroke-linejoin style !" val))))
		("fill-rule"
			(case val
				("nonzero" +winding_none_zero)
				("evenodd" +winding_odd_even)
				(t (throw "Unknown fill-rule !" val))))
		("d"
			(gen-svg-path val))
		(t  val)))

(defun push-attributes (stack keys vals)
	(push stack (defq entry (. (elem -2 stack) :copy)))
	(each (lambda (key val)
		(cond
			((eql key "style")
				(each (lambda ((k v))
						(. entry :insert k (read-attribute entry k v)))
					(map (# (split %0 ":")) (split val ";"))))
			(t (setq val (read-attribute entry key val))))
		(. entry :insert key val)) keys vals)
	entry)

(defun path-fill-and-stroke (canvas entry paths)
	(defq stroke_width (* 0.5 (. entry :find "stroke-width"))
		fill (. entry :find "fill")
		fill_rule (. entry :find "fill-rule")
		fill_opacity (. entry :find "fill-opacity")
		stroke (. entry :find "stroke")
		stroke_opacity (. entry :find "stroke-opacity")
		stroke_linecap (. entry :find "stroke-linecap")
		stroke_linejoin (. entry :find "stroke-linejoin")
		transform (. entry :find "transform"))
	(when (and (/= fill 0) (/= fill_opacity 0.0))
		(.-> canvas
			(:set_color (+ (<< (n2i (* fill_opacity 0xff.0)) 24) (logand fill 0xffffff)))
			(:fpoly 0.0 0.0 fill_rule
				(map (lambda ((f p)) (apply (const path-transform) (cat transform (list p (cat p)))))
					paths))))
	(when (and (/= stroke 0) (/= stroke_opacity 0.0))
		(.-> canvas
			(:set_color (+ (<< (n2i (* stroke_opacity 0xff.0)) 24) (logand stroke 0xffffff)))
			(:fpoly 0.0 0.0 fill_rule
				(map (lambda (p) (apply (const path-transform) (cat transform (list p p))))
					(reduce (lambda (l (f p))
						(cond
							(f  ;closed
								(bind '(p1 p2) (path-stroke-polygon p stroke_width +eps
									stroke_linejoin))
								(push l p1 p2))
							(t  ;open
								(push l (path-stroke-polyline p stroke_width +eps
									stroke_linejoin stroke_linecap stroke_linecap)))))
							paths (list)))))))

(defun SVG-Canvas (stream &optional scale)
	; (SVG-Canvas stream [scale]) -> nil | canvas
	(setd scale 1)
	(defq canvas nil stack
		(list (lmap-kv
			"stroke" 0
			"fill" +argb_black
			"stroke-width" 1.0
			"stroke-linecap" +cap_butt
			"stroke-linejoin" +join_miter
			"fill-rule" +winding_none_zero
			"fill-opacity" 1.0
			"stroke-opacity" 1.0
			"transform" (list (n2f scale) 0.0 0.0 (n2f scale) 0.0 0.0)
			"x" 0.0 "y" 0.0 "x1" 0.0 "y1" 0.0 "x2" 0.0 "y2" 0.0
			"cx" 0.0 "cy" 0.0 "r" 0.0
			"d" (path) "points" (path)
			)))
	(XML-parse stream
		(lambda (command keys vals)
;           (prin "open: " command " " keys " " vals)(print)
			(case command
				("svg"
					(defq entry (push-attributes stack keys vals) width 256 height 256)
					(if (defq val (. entry :find "viewBox")) (bind '(_ _ width height) val))
					(if (defq val (. entry :find "width")) (setq width val))
					(if (defq val (. entry :find "height")) (setq height val))
					(. (setq canvas (Canvas (n2i width) (n2i height) (n2i scale)))
						:set_canvas_flags +canvas_flag_antialias))
				("g"
					(push-attributes stack keys vals))
				("circle"
					(defq entry (push-attributes stack keys vals)
						cx (. entry :find "cx") cy (. entry :find "cy") r (. entry :find "r")
						d (path-gen-arc cx cy 0.0 +fp_2pi r +eps (path)))
					(path-fill-and-stroke canvas entry (list (list t d))))
				("ellipse"
					(defq entry (push-attributes stack keys vals)
						cx (. entry :find "cx") cy (. entry :find "cy")
						rx (. entry :find "rx") ry (. entry :find "ry"))
					(unless rx (setq rx ry))
					(unless ry (setq ry rx))
					(defq d (path-gen-ellipse cx cy rx ry +eps (path)))
					(path-fill-and-stroke canvas entry (list (list t d))))
				("line"
					(defq entry (push-attributes stack keys vals)
						x1 (. entry :find "x1") y1 (. entry :find "y1")
						x2 (. entry :find "x2") y2 (. entry :find "y2")
						d (path x1 y1 x2 y2))
					(path-fill-and-stroke canvas entry (list (list nil d))))
				("rect"
					(defq entry (push-attributes stack keys vals)
						x (. entry :find "x") y (. entry :find "y")
						rx (. entry :find "rx") ry (. entry :find "ry")
						x1 (+ x (. entry :find "width")) y1 (+ y (. entry :find "height")))
					(unless rx (setq rx ry))
					(unless ry (setq ry rx))
					(defq d (path-gen-rect x y x1 y1 rx ry +eps (path)))
					(path-fill-and-stroke canvas entry (list (list t d))))
				("polygon"
					(defq entry (push-attributes stack keys vals)
						d (. entry :find "points"))
					(path-fill-and-stroke canvas entry (list (list t d))))
				("polyline"
					(defq entry (push-attributes stack keys vals)
						d (. entry :find "points"))
					(path-fill-and-stroke canvas entry (list (list nil d))))
				("path"
					(defq entry (push-attributes stack keys vals)
						d (. entry :find "d"))
					(path-fill-and-stroke canvas entry d))
				))
		(lambda (command)
			(case command
				(("svg" "g" "circle" "ellipse" "path" "polygon" "polyline" "line" "rect")
					(pop stack))
				))
		(lambda (text)))
	;resize to final output
	(bind '(w h) (. canvas :pref_size))
	(. (Canvas w h 1) :resize canvas))

(defun SVG-info (stream)
	; (SVG-info stream) -> (width height type) | (-1 -1 -1)
	(defq stack (list (lmap)) width -1 height -1)
	(XML-parse stream
		(lambda (command keys vals)
			(when (eql command "svg")
				(defq entry (push-attributes stack keys vals))
				(when (defq val (. entry :find "viewBox"))
					(bind '(_ _ w h) val)
					(setq width w height h))
				(if (defq val (. entry :find "width")) (setq width val))
				(if (defq val (. entry :find "height")) (setq height val))))
		(lambda (command))
		(lambda (text)))
	(list width height 32))

;module
(export-symbols SVG-Canvas SVG-info)
(env-pop)
