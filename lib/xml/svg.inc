;;;;;;;;;;;;
; svg parser
;;;;;;;;;;;;

(import "./parse.inc")
(import "gui/canvas/lisp.inc")

;module
(env-push)

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
						(t	(push commands (n2f (read-num (apply cat token))))
							(setq state :text))))
			))) (cat d " "))
	commands)

(defun gen-svg-path (d)
	(defq i 0 p (path) d (parse-svg-path d)
		cx 0.0 cy 0.0 cpx 0.0 cpy 0.0 last_cmd " ")
	(while (< i (length d))
		(if (str? (defq cmd (elem i d)))
			(setq i (inc i))
			(setq cmd last_cmd))
		(case cmd
			("M"
				(bind '(cpx cpy) (slice i (setq i (+ i 2)) d))
				(push p cpx cpy))
			("m"
				(bind '(x y) (slice i (setq i (+ i 2)) d))
				(setq cpx (+ cpx x) cpy (+ cpy y))
				(push p cpx cpy))
			("L"
				(bind '(cpx cpy) (slice i (setq i (+ i 2)) d))
				(push p cpx cpy))
			("l"
				(bind '(x y) (slice i (setq i (+ i 2)) d))
				(setq cpx (+ cpx x) cpy (+ cpy y))
				(push p cpx cpy))
			("H"
				(bind '(cpx) (slice i (setq i (+ i 1)) d))
				(push p cpx cpy))
			("h"
				(bind '(x) (slice i (setq i (+ i 1)) d))
				(setq cpx (+ cpx x))
				(push p cpx cpy))
			("V"
				(bind '(cpy) (slice i (setq i (+ i 1)) d))
				(push p cpx cpy))
			("v"
				(bind '(y) (slice i (setq i (+ i 1)) d))
				(setq cpy (+ cpy y))
				(push p cpx cpy))
			("C"
				(bind '(x1 y1 x2 y2 x y) (slice i (setq i (+ i 6)) d))
				(path-gen-cubic cpx cpy x1 y1 x2 y2 x y +eps p)
				(setq cpx x cpy y cx x2 cy y2))
			("c"
				(bind '(x1 y1 x2 y2 x y) (slice i (setq i (+ i 6)) d))
				(setq x1 (+ cpx x1) y1 (+ cpy y1)
					x2 (+ cpx x2) y2 (+ cpy y2)
					x (+ cpx x) y (+ cpy y))
				(path-gen-cubic cpx cpy x1 y1 x2 y2 x y +eps p)
				(setq cpx x cpy y cx x2 cy y2))
			("S"
				(bind '(x2 y2 x y) (slice i (setq i (+ i 4)) d))
				(if (find last_cmd "CScs")
					(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
					(defq x1 cpx y1 cpy))
				(path-gen-cubic cpx cpy x1 y1 x2 y2 x y +eps p)
				(setq cpx x cpy y cx x2 cy y2))
			("s"
				(bind '(x2 y2 x y) (slice i (setq i (+ i 4)) d))
				(setq x2 (+ cpx x2) y2 (+ cpy y2)
					x (+ cpx x) y (+ cpy y))
				(if (find last_cmd "CScs")
					(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
					(defq x1 cpx y1 cpy))
				(path-gen-cubic cpx cpy x1 y1 x2 y2 x y +eps p)
				(setq cpx x cpy y cx x2 cy y2))
			("Q"
				(bind '(x1 y1 x y) (slice i (setq i (+ i 4)) d))
				(path-gen-quadratic cpx cpy x1 y1 x y +eps p)
				(setq cpx x cpy y cx x1 cy y1))
			("q"
				(bind '(x1 y1 x y) (slice i (setq i (+ i 4)) d))
				(setq x1 (+ cpx x1) y1 (+ cpy y1)
					x (+ cpx x) y (+ cpy y))
				(path-gen-quadratic cpx cpy x1 y1 x y +eps p)
				(setq cpx x cpy y cx x1 cy y1))
			("T"
				(bind '(x y) (slice i (setq i (+ i 2)) d))
				(if (find last_cmd "QTqt")
					(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
					(defq x1 cpx y1 cpy))
				(path-gen-quadratic cpx cpy x1 y1 x y +eps p)
				(setq cpx x cpy y cx x1 cy y1))
			("t"
				(bind '(x y) (slice i (setq i (+ i 2)) d))
				(setq x (+ cpx x) y (+ cpy y))
				(if (find last_cmd "QTqt")
					(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
					(defq x1 cpx y1 cpy))
				(path-gen-quadratic cpx cpy x1 y1 x y +eps p)
				(setq cpx x cpy y cx x1 cy y1))
 			("A"
			 	(throw "Not implemted path command !" cmd)
				(bind '(rx ry x_axis_rotation large_arc_flag sweep_flag x y)
					(slice i (setq i (+ i 7)) d)))
			("a"
			 	(throw "Not implemted path command !" cmd)
				(bind '(rx ry x_axis_rotation large_arc_flag sweep_flag dx dy)
					(slice i (setq i (+ i 7)) d)))
			(("Z" "z")
				(bind '(cpx cpy) (slice 0 2 p))
				(push p cpx cpy)))
			(setq last_cmd cmd))
	p)

(defun push-attributes (stack keys vals)
	(push stack (defq entry (. (elem -2 stack) :copy)))
	(each (lambda (key val)
		(setq val (case key
			(("x" "y" "x1" "y1" "x2" "y2" "rx" "ry" "cx" "cy" "r"
				"width" "height" "stroke-width")
				(n2f (read-num val)))
			("viewBox"
				(map (# (n2i (read-num %0))) (split val ", ")))
			("points"
				(apply path (map (# (n2f (read-num %0))) (split val ", "))))
			("transform"
				(map (# (n2f (read-num %0)))
					(split (slice (inc (find "(" val)) (find-rev ")" val) val) ", ")))
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
					(t	(cond
							((starts-with "#" val)
								(col-to-num val))
							(t	val)))))
			(("stroke-linecap")
				(case val
					("butt" +cap_butt)
					("round" +cap_round)
					("square" +cap_square)
					(t (throw "Unknown cap style !" val))))
			(("stroke-linejoin")
				(case val
					("miter" +join_miter)
					("bevel" +join_bevel)
					("round" +join_round)
					("miter-clip" +join_miter)
					("arcs" +join_miter)
					(t (throw "Unknown join style !" val))))
			("line-rule"
				(case val
					("nonzero" +winding_none_zero)
					("evenodd" +winding_odd_even)
					(t (throw "Unknown winding !" val))))
			("d"
				(gen-svg-path val))
			(t	val)))
		(. entry :insert key val)) keys vals)
	entry)

(defun print-attribute-stack (stack)
	(print "Attribute stack:")
	(each (lambda (entry)
		(. entry :each (# (print %0 " : " %1)))) stack)
	(print))

(defun path-fill-and-stroke (canvas entry d flag)
	(defq stroke_width (* 0.5 (. entry :find "stroke-width"))
		fill (. entry :find "fill")
		stroke (. entry :find "stroke")
		stroke_linecap (. entry :find "stroke-linecap")
		stroke_linejoin (. entry :find "stroke-linejoin")
		fill_rule (. entry :find "fill-rule")
		transform (. entry :find "transform"))
	(when (/= fill 0)
		(defq df (map (# (apply (const path-transform) (cat transform (list %0 (cat %0))))) d))
		(.-> canvas (:set_color fill) (:fpoly 0.0 0.0 fill_rule df)))
	(when (/= stroke 0)
		(cond
			(flag
				;closed
				(defq ds (path-stroke-polygons (list) stroke_width +eps stroke_linejoin d)
					df (map (# (apply (const path-transform) (cat transform (list %0 %0)))) ds)))
			(t	;open
				(defq ds (path-stroke-polylines (list) stroke_width +eps
							stroke_linejoin stroke_linecap stroke_linecap d)
					df (map (# (apply (const path-transform) (cat transform (list %0 %0)))) ds))))
		(.-> canvas (:set_color stroke) (:fpoly 0.0 0.0 fill_rule df))))

(defun SVG-Canvas (file scale flags)
	(defq canvas nil count 0 attribute_stack
		(list (fmap-kv
			"stroke" 0
			"fill" +argb_black
			"stroke-width" 1.0
			"stroke-linecap" +cap_butt
			"stroke-linejoin" +join_miter
			"fill-rule" +winding_none_zero
			"transform" '(1.0 0.0 0.0 1.0 0.0 0.0))))
	(XML-parse (file-stream file)
		(lambda (command keys vals)
;			(prin "open: " command " " keys " " vals)(print)
			(case command
				("svg"
					(defq entry (push-attributes attribute_stack keys vals) width 256 height 256)
					(if (defq val (. entry :find "viewBox")) (bind '(_ _ width height) val))
					(if (defq val (. entry :find "width")) (setq width val))
					(if (defq val (. entry :find "height")) (setq height val))
					(. (setq canvas (Canvas (/ (inc (n2i width)) scale) (/ (inc (n2i height)) scale) scale))
						:set_canvas_flags +canvas_flag_antialias))
				("g"
					(push-attributes attribute_stack keys vals))
				("circle"
					(defq entry (push-attributes attribute_stack keys vals)
						cx (. entry :find "cx") cy (. entry :find "cy") r (. entry :find "r")
						d (path-gen-arc cx cy 0.0 +fp_2pi r +eps (path)))
					(path-fill-and-stroke canvas entry (list d) t))
				("line"
					(defq entry (push-attributes attribute_stack keys vals)
						x1 (. entry :find "x1") y1 (. entry :find "y1")
						x2 (. entry :find "x2") y2 (. entry :find "y2")
						d (path x1 y1 x2 y2))
					(path-fill-and-stroke canvas entry (list d) nil))
				("rect"
					(defq entry (push-attributes attribute_stack keys vals)
						x (. entry :find "x") y (. entry :find "y")
						rx (. entry :find "rx") ry (. entry :find "ry")
						x1 (+ x (. entry :find "width")) y1 (+ y (. entry :find "height"))
						d (path x y x1 y x1 y1 x y1))
					(path-fill-and-stroke canvas entry (list d) t))
				("polygon"
					(defq entry (push-attributes attribute_stack keys vals)
						d (. entry :find "points"))
					(path-fill-and-stroke canvas entry (list d) t))
				("polyline"
					(defq entry (push-attributes attribute_stack keys vals)
						d (. entry :find "points"))
					(path-fill-and-stroke canvas entry (list d) nil))
				("path"
					(defq entry (push-attributes attribute_stack keys vals)
						d (. entry :find "d"))
					(path-fill-and-stroke canvas entry (list d) nil))
				))
		(lambda (command)
			(case command
				(("svg" "g" "circle" "path" "polygon" "polyline" "line" "rect")
					(pop attribute_stack))
				))
		(lambda (text)))
	(. canvas :swap))

;module
(export-symbols SVG-Canvas)
(env-pop)
