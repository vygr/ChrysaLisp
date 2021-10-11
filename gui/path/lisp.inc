;lisp bindings
(ffi path-gen-arc "gui/path/lisp_gen_arc" 0)
(ffi path-gen-cubic "gui/path/lisp_gen_cubic" 0)
(ffi path-gen-quadratic "gui/path/lisp_gen_quadratic" 0)
(ffi path-filter "gui/path/lisp_filter" 0)
(ffi path-simplify "gui/path/lisp_simplify" 0)
(ffi path-stroke-polygon "gui/path/lisp_stroke_polygon" 0)
(ffi path-stroke-polyline "gui/path/lisp_stroke_polyline" 0)
(ffi path-transform "gui/path/lisp_transform" 0)

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
		(t (defq d (path x y x y1 x1 y1 x1 y)))))

(defun path-gen-ellipse (cx cy rx ry tol dst)
	; (path-gen-ellipse cx cy rx ry tol dst) -> dst
	(defq s 0.1 a (neg s))
	(while (< (setq a (+ a s)) +fp_2pi)
		(push dst (+ cx (* rx (sin a))) (+ cy (* ry (cos a)))))
	dst)

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
