;;;;;;;;;;;;
; cwb loader
;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")
(import "lib/math/vector.inc")

;module
(env-push)

(defq +margin_ratio (n2f 0.05) +margin_min (n2f 10.0))

(defun bounds (groups)
	(bind '(((min_v max_v) &ignore) &ignore) groups)
	(defq min_v (cat min_v) max_v (cat max_v))
	(each (lambda (((gmin gmax) &ignore))
		(vector-min min_v gmin min_v)
		(vector-max max_v gmax max_v)) groups)
	(list min_v max_v))

(defun CWB-info (stream)
	; (CWB-info stream) -> (width height type) | (-1 -1 -1)
	(defq out '(-1 -1 -1))
	(when (and (defq data (tree-load stream))
			(>= (. data :find :version) 2)
			(defq groups (. data :find :groups))
			(nempty? groups))
		(bind '((min_x min_y) (max_x max_y)) (bounds groups))
		(when (< min_x max_x)
			(defq w (- max_x min_x) h (- max_y min_y)
				margin (max (* (max w h) (const +margin_ratio)) (const +margin_min)))
			(setq out (list (n2i (+ w (* (const (n2f 2.0)) margin))) (n2i (+ h (* (const (n2f 2.0)) margin))) 32))))
	out)

(defun CWB-Canvas (stream &optional scale)
	; (CWB-Canvas stream [scale]) -> :nil | canvas
	(setd scale 1)
	(when (and (defq data (tree-load stream))
			(>= (. data :find :version) 2)
			(defq groups (. data :find :groups))
			(nempty? groups))
		(bind '((min_x min_y) (max_x max_y)) (bounds groups))
		(when (< min_x max_x)
			(defq w (- max_x min_x) h (- max_y min_y)
				margin (max (* (max w h) (const +margin_ratio)) (const +margin_min))
				cw (n2i (+ w (* (const (n2f 2.0)) margin))) ch (n2i (+ h (* (const (n2f 2.0)) margin)))
				canvas (Canvas cw ch scale) tx (- margin min_x) ty (- margin min_y)
				fs (n2f scale) matrix (fixeds fs 0.0 (* tx fs) 0.0 fs (* ty fs)))
			(.-> canvas (:set_canvas_flags +canvas_flag_antialias) (:fill 0))
			(each (lambda (group)
				(each (lambda ((col poly bbox))
					(defq transformed_poly (map (# (path-transform matrix %0 (cat %0))) poly))
					(.-> canvas (:set_color col) (:fpoly 0.0 0.0 +winding_none_zero transformed_poly)))
					(second group)))
				groups)
			canvas)))

;module
(export-symbols '(CWB-Canvas CWB-info))
(env-pop)