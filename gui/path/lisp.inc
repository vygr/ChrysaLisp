;lisp bindings
(ffi path-gen-arc "gui/path/lisp_gen_arc" 0)
(ffi path-gen-cubic "gui/path/lisp_gen_cubic" 0)
(ffi path-gen-quadratic "gui/path/lisp_gen_quadratic" 0)
(ffi path-filter "gui/path/lisp_filter" 0)
(ffi path-simplify "gui/path/lisp_simplify" 0)
(ffi path-stroke-polygon "gui/path/lisp_stroke_polygon" 0)
(ffi path-stroke-polyline "gui/path/lisp_stroke_polyline" 0)
(ffi path-transform "gui/path/lisp_transform" 0)
(ffi path-svg "gui/path/lisp_svg" 0)

(enums +join 0
	(enum miter bevel round))

(enums +cap 0
	(enum butt square tri arrow round))

(defun path-gen-rect (x y x1 y1 rx ry tol dst)
	; (path-gen-rect x y x1 y1 rx ry tol dst) -> dst
	(cond
		((and rx ry)
			(defq x (+ x rx) y (+ y ry) x1 (- x1 rx) y1 (- y1 ry))
			(path-gen-arc x y +fp_pi +fp_hpi rx tol dst)
			(path-gen-arc x y1 (const (+ +fp_pi +fp_hpi)) +fp_hpi rx tol dst)
			(path-gen-arc x1 y1 0.0 +fp_hpi rx tol dst)
			(path-gen-arc x1 y +fp_hpi +fp_hpi rx tol dst))
		(:t (defq d (path x y x y1 x1 y1 x1 y)))))

(defun path-gen-ellipse (cx cy rx ry tol dst)
	; (path-gen-ellipse cx cy rx ry tol dst) -> dst
	(defq s 0.1 a (neg s))
	(while (< (setq a (+ a s)) +fp_2pi)
		(push dst (+ cx (* rx (sin a))) (+ cy (* ry (cos a)))))
	dst)

(defun path-gen-paths (d eps)
	; (path-gen-paths svg_d tol) -> (list (:nil|:t path) ...)
	(defq i 0 paths (list) d (path-svg (cat d " "))
		cx 0.0 cy 0.0 cpx 0.0 cpy 0.0 last_cmd " ")
	(until (eql (defq cmd (elem-get i d)) "x")
		(setq i (inc i))
		(case cmd
			("M"
				(push paths (list :nil (defq p (path))))
				(while (num? (elem-get i d))
					(bind '(cpx cpy) (slice i (setq i (+ i 2)) d))
					(push p cpx cpy)))
			("m"
				(push paths (list :nil (defq p (path))))
				(while (num? (elem-get i d))
					(bind '(x y) (slice i (setq i (+ i 2)) d))
					(setq cpx (+ cpx x) cpy (+ cpy y))
					(push p cpx cpy)))
			("L"
				(while (num? (elem-get i d))
					(bind '(cpx cpy) (slice i (setq i (+ i 2)) d))
					(push p cpx cpy)))
			("l"
				(while (num? (elem-get i d))
					(bind '(x y) (slice i (setq i (+ i 2)) d))
					(setq cpx (+ cpx x) cpy (+ cpy y))
					(push p cpx cpy)))
			("H"
				(while (num? (elem-get i d))
					(bind '(cpx) (slice i (setq i (+ i 1)) d))
					(push p cpx cpy)))
			("h"
				(while (num? (elem-get i d))
					(bind '(x) (slice i (setq i (+ i 1)) d))
					(setq cpx (+ cpx x))
					(push p cpx cpy)))
			("V"
				(while (num? (elem-get i d))
					(bind '(cpy) (slice i (setq i (+ i 1)) d))
					(push p cpx cpy)))
			("v"
				(while (num? (elem-get i d))
					(bind '(y) (slice i (setq i (+ i 1)) d))
					(setq cpy (+ cpy y))
					(push p cpx cpy)))
			("C"
				(while (num? (elem-get i d))
					(bind '(x1 y1 x2 y2 x y) (slice i (setq i (+ i 6)) d))
					(path-gen-cubic cpx cpy x1 y1 x2 y2 x y eps p)
					(setq cpx x cpy y cx x2 cy y2)))
			("c"
				(while (num? (elem-get i d))
					(bind '(x1 y1 x2 y2 x y) (slice i (setq i (+ i 6)) d))
					(setq x1 (+ cpx x1) y1 (+ cpy y1)
						x2 (+ cpx x2) y2 (+ cpy y2)
						x (+ cpx x) y (+ cpy y))
					(path-gen-cubic cpx cpy x1 y1 x2 y2 x y eps p)
					(setq cpx x cpy y cx x2 cy y2)))
			("S"
				(while (num? (elem-get i d))
					(bind '(x2 y2 x y) (slice i (setq i (+ i 4)) d))
					(if (find last_cmd "CScs")
						(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
						(defq x1 cpx y1 cpy))
					(path-gen-cubic cpx cpy x1 y1 x2 y2 x y eps p)
					(setq cpx x cpy y cx x2 cy y2)))
			("s"
				(while (num? (elem-get i d))
					(bind '(x2 y2 x y) (slice i (setq i (+ i 4)) d))
					(setq x2 (+ cpx x2) y2 (+ cpy y2)
						x (+ cpx x) y (+ cpy y))
					(if (find last_cmd "CScs")
						(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
						(defq x1 cpx y1 cpy))
					(path-gen-cubic cpx cpy x1 y1 x2 y2 x y eps p)
					(setq cpx x cpy y cx x2 cy y2)))
			("Q"
				(while (num? (elem-get i d))
					(bind '(x1 y1 x y) (slice i (setq i (+ i 4)) d))
					(path-gen-quadratic cpx cpy x1 y1 x y eps p)
					(setq cpx x cpy y cx x1 cy y1)))
			("q"
				(while (num? (elem-get i d))
					(bind '(x1 y1 x y) (slice i (setq i (+ i 4)) d))
					(setq x1 (+ cpx x1) y1 (+ cpy y1)
						x (+ cpx x) y (+ cpy y))
					(path-gen-quadratic cpx cpy x1 y1 x y eps p)
					(setq cpx x cpy y cx x1 cy y1)))
			("T"
				(while (num? (elem-get i d))
					(bind '(x y) (slice i (setq i (+ i 2)) d))
					(if (find last_cmd "QTqt")
						(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
						(defq x1 cpx y1 cpy))
					(path-gen-quadratic cpx cpy x1 y1 x y eps p)
					(setq cpx x cpy y cx x1 cy y1)))
			(":t"
				(while (num? (elem-get i d))
					(bind '(x y) (slice i (setq i (+ i 2)) d))
					(setq x (+ cpx x) y (+ cpy y))
					(if (find last_cmd "QTqt")
						(defq x1 (- (* cpx 2.0) cx) y1 (- (* cpy 2.0) cy))
						(defq x1 cpx y1 cpy))
					(path-gen-quadratic cpx cpy x1 y1 x y eps p)
					(setq cpx x cpy y cx x1 cy y1)))
			("A"
				(while (num? (elem-get i d))
					(throw "Not implemted path command !" cmd)
					(bind '(rx ry x_axis_rotation large_arc_flag sweep_flag x y)
						(slice i (setq i (+ i 7)) d))))
			("a"
				(while (num? (elem-get i d))
					(throw "Not implemted path command !" cmd)
					(bind '(rx ry x_axis_rotation large_arc_flag sweep_flag dx dy)
						(slice i (setq i (+ i 7)) d))))
			(("Z" "z")
				(bind '(cpx cpy) (slice 0 2 p))
				(push p cpx cpy)
				(elem-set 0 (elem-get -2 paths) :t)))
		(setq last_cmd cmd))
	paths)

(defun path-stroke-polylines (dst radius tol joint cap1 cap2 src)
	; (path-stroke-polylines dst radius tol join cap1 cap2 src) -> dst
	(each (lambda (p)
		(push dst (path-stroke-polyline p radius tol joint cap1 cap2))) src)
	dst)

(defun path-stroke-polygons (dst radius tol joint src)
	; (path-stroke-polygons dst radius tol join src) -> dst
	(each (lambda (p)
		(bind '(p p1) (path-stroke-polygon p radius tol joint))
		(push dst p p1)) src)
	dst)
