;;;;;;;;;;;;
; cwb loader
;;;;;;;;;;;;

(import "gui/canvas/lisp.inc")

;module
(env-push)

(defun CWB-info (stream)
	; (CWB-info stream) -> (width height type) | (-1 -1 -1)
	(if (and (defq data (tree-load stream))
			(>= (. data :find :version) 2)
			(defq groups (. data :find :groups))
			(nempty? groups))
		(progn
			(defq min_x (const (n2f 1000000.0)) min_y (const (n2f 1000000.0))
				max_x (const (n2f -1000000.0)) max_y (const (n2f -1000000.0)))
			(each (lambda (group)
				(bind '((gminx gminy) (gmaxx gmaxy)) (first group))
				(setq min_x (min min_x gminx) min_y (min min_y gminy)
					max_x (max max_x gmaxx) max_y (max max_y gmaxy)))
				groups)
			(if (< min_x max_x)
				(progn
					(defq w (- max_x min_x) h (- max_y min_y)
						margin (max (* (max w h) (const (n2f 0.05))) (const (n2f 10.0))))
					(list (n2i (+ w (* (const (n2f 2.0)) margin))) (n2i (+ h (* (const (n2f 2.0)) margin))) 32))
				(list -1 -1 -1)))
		(list -1 -1 -1)))

(defun CWB-Canvas (stream &optional scale)
	; (CWB-Canvas stream [scale]) -> :nil | canvas
	(setd scale 1)
	(if (and (defq data (tree-load stream))
			(>= (. data :find :version) 2)
			(defq groups (. data :find :groups))
			(nempty? groups))
		(progn
			(defq min_x (const (n2f 1000000.0)) min_y (const (n2f 1000000.0))
				max_x (const (n2f -1000000.0)) max_y (const (n2f -1000000.0)))
			(each (lambda (group)
				(bind '((gminx gminy) (gmaxx gmaxy)) (first group))
				(setq min_x (min min_x gminx) min_y (min min_y gminy)
					max_x (max max_x gmaxx) max_y (max max_y gmaxy)))
				groups)
			(if (< min_x max_x)
				(progn
					(defq w (- max_x min_x) h (- max_y min_y)
						margin (max (* (max w h) (const (n2f 0.05))) (const (n2f 10.0)))
						cw (n2i (+ w (* (const (n2f 2.0)) margin))) ch (n2i (+ h (* (const (n2f 2.0)) margin)))
						canvas (Canvas cw ch scale)
						tx (- margin min_x) ty (- margin min_y)
						fs (n2f scale)
						matrix (fixeds fs 0.0 (* tx fs) 0.0 fs (* ty fs)))
					(.-> canvas (:set_canvas_flags +canvas_flag_antialias) (:fill 0))
					(each (lambda (group)
						(each (lambda ((col poly bbox))
							(defq transformed_poly (map (# (path-transform matrix %0 (cat %0))) poly))
							(.-> canvas (:set_color col) (:fpoly 0.0 0.0 +winding_none_zero transformed_poly)))
							(second group)))
						groups)
					canvas)
				:nil))
		:nil))

;module
(export-symbols '(CWB-Canvas CWB-info))
(env-pop)