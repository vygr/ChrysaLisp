;lisp bindings
(ffi "gui/path/lisp_gen_arc" path-gen-arc)
(ffi "gui/path/lisp_gen_cubic" path-gen-cubic)
(ffi "gui/path/lisp_gen_quadratic" path-gen-quadratic)
(ffi "gui/path/lisp_filter" path-filter)
(ffi "gui/path/lisp_simplify" path-simplify)
(ffi "gui/path/lisp_stroke_polygon" path-stroke-polygon)
(ffi "gui/path/lisp_stroke_polyline" path-stroke-polyline)
(ffi "gui/path/lisp_transform" path-transform)
(ffi "gui/path/lisp_svg" path-svg)

(enums +join 0
	(enum miter bevel round))

(enums +cap 0
	(enum butt square tri arrow round))

(defun path-smooth (src)
	; (path-smooth src) -> dst
	(cond
		((> (length src) 4)
			(defq out (path) src (partition src 2)
				last_point (first src) last_mid_point last_point
				mid_point last_mid_point)
			(each (# (setq last_mid_point mid_point
				mid_point (nums-add last_point (nums-scale (nums-sub %0 last_point) 0.5)))
				(path-gen-quadratic
					(first last_mid_point) (second last_mid_point)
					(first last_point) (second last_point)
					(first mid_point) (second mid_point)
					out)
				(setq last_point %0)) src)
			(push out (first last_point) (second last_point)))
		(src)))

(defun path-gen-rect (x y x1 y1 rx ry dst)
	; (path-gen-rect x y x1 y1 rx ry dst) -> dst
	(cond
		((and rx ry)
			(defq x (+ x rx) y (+ y ry) x1 (- x1 rx) y1 (- y1 ry))
			(path-gen-arc x y +fp_pi +fp_hpi rx dst)
			(path-gen-arc x y1 (const (+ +fp_pi +fp_hpi)) +fp_hpi rx dst)
			(path-gen-arc x1 y1 0.0 +fp_hpi rx dst)
			(path-gen-arc x1 y +fp_hpi +fp_hpi rx dst))
		((path x y x y1 x1 y1 x1 y))))

(defun path-gen-ellipse (cx cy rx ry dst)
	; (path-gen-ellipse cx cy rx ry dst) -> dst
	(defq s 0.1 a (neg s))
	(while (< (++ a s) +fp_2pi)
		(push dst (+ cx (* rx (sin a))) (+ cy (* ry (cos a)))))
	dst)

(defun path-gen-paths (d)
	; (path-gen-paths svg_d) -> ((:nil|:t path) ...)
	; :t closed, :nil open
	(defq i 0 paths (list) d (path-svg (cat d " "))
		cx 0.0 cy 0.0 cpx 0.0 cpy 0.0 last_cmd " ")
	(until (eql (defq cmd (elem-get d i)) "x")
		(++ i)
		(case cmd
			("M"
				(push paths (list :nil (defq p (path))))
				(while (num? (elem-get d i))
					(bind '(cpx cpy) (slice d i (++ i 2)))
					(push p cpx cpy)))
			("m"
				(push paths (list :nil (defq p (path))))
				(while (num? (elem-get d i))
					(bind '(x y) (slice d i (++ i 2)))
					(setq cpx (+ cpx x) cpy (+ cpy y))
					(push p cpx cpy)))
			("L"
				(while (num? (elem-get d i))
					(bind '(cpx cpy) (slice d i (++ i 2)))
					(push p cpx cpy)))
			("l"
				(while (num? (elem-get d i))
					(bind '(x y) (slice d i (++ i 2)))
					(setq cpx (+ cpx x) cpy (+ cpy y))
					(push p cpx cpy)))
			("H"
				(while (num? (elem-get d i))
					(bind '(cpx) (slice d i (++ i 1)))
					(push p cpx cpy)))
			("h"
				(while (num? (elem-get d i))
					(bind '(x) (slice d i (++ i)))
					(++ cpx x)
					(push p cpx cpy)))
			("V"
				(while (num? (elem-get d i))
					(bind '(cpy) (slice d i (++ i)))
					(push p cpx cpy)))
			("v"
				(while (num? (elem-get d i))
					(bind '(y) (slice d i (++ i)))
					(++ cpy y)
					(push p cpx cpy)))
			("C"
				(while (num? (elem-get d i))
					(bind '(x1 y1 x2 y2 x y) (slice d i (++ i 6)))
					(path-gen-cubic cpx cpy x1 y1 x2 y2 x y p)
					(setq cpx x cpy y cx x2 cy y2)))
			("c"
				(while (num? (elem-get d i))
					(bind '(x1 y1 x2 y2 x y) (slice d i (++ i 6)))
					(setq x1 (+ cpx x1) y1 (+ cpy y1)
						x2 (+ cpx x2) y2 (+ cpy y2)
						x (+ cpx x) y (+ cpy y))
					(path-gen-cubic cpx cpy x1 y1 x2 y2 x y p)
					(setq cpx x cpy y cx x2 cy y2)))
			("S"
				(while (num? (elem-get d i))
					(bind '(x2 y2 x y) (slice d i (++ i 4)))
					(if (find last_cmd "CScs")
						(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
						(defq x1 cpx y1 cpy))
					(path-gen-cubic cpx cpy x1 y1 x2 y2 x y p)
					(setq cpx x cpy y cx x2 cy y2)))
			("s"
				(while (num? (elem-get d i))
					(bind '(x2 y2 x y) (slice d i (++ i 4)))
					(setq x2 (+ cpx x2) y2 (+ cpy y2)
						x (+ cpx x) y (+ cpy y))
					(if (find last_cmd "CScs")
						(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
						(defq x1 cpx y1 cpy))
					(path-gen-cubic cpx cpy x1 y1 x2 y2 x y p)
					(setq cpx x cpy y cx x2 cy y2)))
			("Q"
				(while (num? (elem-get d i))
					(bind '(x1 y1 x y) (slice d i (++ i 4)))
					(path-gen-quadratic cpx cpy x1 y1 x y p)
					(setq cpx x cpy y cx x1 cy y1)))
			("q"
				(while (num? (elem-get d i))
					(bind '(x1 y1 x y) (slice d i (++ i 4)))
					(setq x1 (+ cpx x1) y1 (+ cpy y1)
						x (+ cpx x) y (+ cpy y))
					(path-gen-quadratic cpx cpy x1 y1 x y p)
					(setq cpx x cpy y cx x1 cy y1)))
			("T"
				(while (num? (elem-get d i))
					(bind '(x y) (slice d i (++ i 2)))
					(if (find last_cmd "QTqt")
						(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
						(defq x1 cpx y1 cpy))
					(path-gen-quadratic cpx cpy x1 y1 x y p)
					(setq cpx x cpy y cx x1 cy y1)))
			("t"
				(while (num? (elem-get d i))
					(bind '(x y) (slice d i (++ i 2)))
					(setq x (+ cpx x) y (+ cpy y))
					(if (find last_cmd "QTqt")
						(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
						(defq x1 cpx y1 cpy))
					(path-gen-quadratic cpx cpy x1 y1 x y p)
					(setq cpx x cpy y cx x1 cy y1)))
			("A"
				(while (num? (elem-get d i))
					(throw "Not implemted path command !" cmd)
					(bind '(rx ry x_axis_rotation large_arc_flag sweep_flag x y)
						(slice d i (++ i 7)))))
			("a"
				(while (num? (elem-get d i))
					(throw "Not implemted path command !" cmd)
					(bind '(rx ry x_axis_rotation large_arc_flag sweep_flag dx dy)
						(slice d i (++ i 7)))))
			(("Z" "z")
				(bind '(cpx cpy) (slice p 0 2))
				(push p cpx cpy)
				(elem-set (last paths) 0 :t)))
		(setq last_cmd cmd))
	paths)

(defun path-stroke-polylines (dst radius joint cap1 cap2 src)
	; (path-stroke-polylines dst radius join cap1 cap2 src) -> dst
	(each (lambda (p)
		(push dst (path-stroke-polyline p radius joint cap1 cap2))) src)
	dst)

(defun path-stroke-polygons (dst radius joint src)
	; (path-stroke-polygons dst radius join src) -> dst
	(each (lambda (p)
		(bind '(p p1) (path-stroke-polygon p radius joint))
		(push dst p p1)) src)
	dst)
