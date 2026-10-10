(import "./struct.inc")

;lisp bindings
(ffi "gui/path/lisp_gen_arc" path-gen-arc)
; (path-gen-arc cx cy start end radius dst) -> dst
(ffi "gui/path/lisp_gen_cubic" path-gen-cubic)
; (path-gen-cubic p1x p1y p2x p2y p3x p3y p4x p4y dst) -> dst
(ffi "gui/path/lisp_gen_quadratic" path-gen-quadratic)
; (path-gen-quadratic p1x p1y p2x p2y p3x p3y dst) -> dst
(ffi "gui/path/lisp_filter" path-filter)
; (path-filter tol src dst) -> dst
(ffi "gui/path/lisp_simplify" path-simplify)
; (path-simplify tol src dst) -> dst
(ffi "gui/path/lisp_stroke_polygon" path-stroke-polygon)
; (path-stroke-polygon path radius join) -> paths
(ffi "gui/path/lisp_stroke_polyline" path-stroke-polyline)
; (path-stroke-polyline path radius join cap1 cap2) -> path
(ffi "gui/path/lisp_transform" path-transform)
; (path-transform m3x2 src dst) -> dst
(ffi "gui/path/lisp_svg" path-svg)
; (path-svg d) -> commands

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

(defun path-angle (x y)
	; (path-angle x y) -> angle
	;the angle of a vector, from the x axis round towards the y axis, in
	;radians, more than -pi and no more than pi. 0.0 for no vector at all.
	;A first guess good to a part in 300, then stepped twice toward where
	;the vector turned back by it lies along the x axis
	(defq len (sqrt (+ (* x x) (* y y))))
	(cond
		((= len 0.0) 0.0)
		(:t (setq x (/ x len) y (/ y len))
			(defq ax (abs x) ay (abs y)
				r (if (>= ax ay) (/ ay ax) (/ ax ay))
				a (+ (* (const (/ +fp_pi 4.0)) r) (* 0.273 r (- 1.0 r))))
			(if (< ax ay) (setq a (- +fp_hpi a)))
			(if (< x 0.0) (setq a (- +fp_pi a)))
			(if (< y 0.0) (setq a (neg a)))
			(times 2 (setq a (+ a (- (* (cos a) y) (* (sin a) x)))))
			a)))

(defun path-gen-earc (x1 y1 rx ry phi large sweep x2 y2 dst)
	; (path-gen-earc x1 y1 rx ry phi large sweep x2 y2 dst) -> dst
	;the arc of an ellipse from one point to another, as the A of an SVG
	;path has it: the two radii, the turn of the ellipse in radians, the
	;larger of the two arcs if large is not 0, drawn the way the angle
	;grows if sweep is not 0. The first point is taken to be in dst, the
	;rest are added, the last is the end point exactly. Radii too small
	;to reach are made as big as is needed, and with no radius, or next
	;to no distance to go, it is a line
	(setq rx (abs rx) ry (abs ry))
	(defq cp (cos phi) sp (sin phi)
		hx (* 0.5 (- x1 x2)) hy (* 0.5 (- y1 y2))
		;the half chord, turned to the ellipse, in units of its radii
		a (if (= rx 0.0) 0.0 (/ (+ (* cp hx) (* sp hy)) rx))
		b (if (= ry 0.0) 0.0 (/ (- (* cp hy) (* sp hx)) ry))
		m (max (abs a) (abs b))
		lam (if (= m 0.0) 0.0 (* m (sqrt (+ (* (/ a m) (/ a m)) (* (/ b m) (/ b m)))))))
	(cond
		;no radius, or so little way to go for the size of it that the
		;square of it is nothing, the start of an arc as a pen sets off
		;round a protractor: a line
		((or (= rx 0.0) (= ry 0.0) (= (* lam lam) 0.0))
			(push dst x2 y2))
		(:t (when (> lam 1.0)
				;too small to reach, so as small as will
				(setq rx (* rx lam) ry (* ry lam) a (/ a lam) b (/ b lam) lam 1.0))
			(defq l2 (* lam lam)
				coef (if (>= l2 1.0) 0.0 (sqrt (/ (- 1.0 l2) l2)))
				coef (if (eql (/= large 0.0) (/= sweep 0.0)) (neg coef) coef)
				;the centre, and the two ends as seen from it, on a unit circle
				ux (- a (* coef b)) uy (+ b (* coef a))
				vx (- (neg a) (* coef b)) vy (+ (neg b) (* coef a))
				cxp (* coef rx b) cyp (neg (* coef ry a))
				cx (+ (- (* cp cxp) (* sp cyp)) (* 0.5 (+ x1 x2)))
				cy (+ (+ (* sp cxp) (* cp cyp)) (* 0.5 (+ y1 y2)))
				t1 (path-angle ux uy)
				dt (path-angle (+ (* ux vx) (* uy vy)) (- (* ux vy) (* uy vx))))
			(cond
				((and (= sweep 0.0) (> dt 0.0)) (setq dt (- dt +fp_2pi)))
				((and (/= sweep 0.0) (< dt 0.0)) (setq dt (+ dt +fp_2pi))))
			;a chord is never more than an eighth of a pixel from the curve
			(defq n (max 2 (n2i (+ 1.0 (/ (abs dt) (sqrt (/ 1.0 (max 1.0 (max rx ry))))))))
				step (/ dt (n2f n)))
			(each (lambda (i)
				(defq ang (+ t1 (* step (n2f i))) ex (* rx (cos ang)) ey (* ry (sin ang)))
				(push dst (+ cx (- (* cp ex) (* sp ey))) (+ cy (+ (* sp ex) (* cp ey)))))
				(range 1 n))
			(push dst x2 y2)))
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
				;an arc of an ellipse, its turn is in degrees
				(while (num? (elem-get d i))
					(bind '(rx ry x_axis_rotation large_arc_flag sweep_flag x y)
						(slice d i (++ i 7)))
					(path-gen-earc cpx cpy rx ry (* x_axis_rotation (const (/ +fp_pi 180.0)))
						large_arc_flag sweep_flag x y p)
					(setq cpx x cpy y)))
			("a"
				(while (num? (elem-get d i))
					(bind '(rx ry x_axis_rotation large_arc_flag sweep_flag x y)
						(slice d i (++ i 7)))
					(setq x (+ cpx x) y (+ cpy y))
					(path-gen-earc cpx cpy rx ry (* x_axis_rotation (const (/ +fp_pi 180.0)))
						large_arc_flag sweep_flag x y p)
					(setq cpx x cpy y)))
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
