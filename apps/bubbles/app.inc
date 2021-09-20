;common imports
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

;quick debug switch
;; (import "lib/debug/debug.inc")
;; (defmacro defun (&rest _) `(defun-debug ~_))

(cond	;pick number format t/nil
	(t	;reals
		(defmacro vec (&rest _) `(reals ~_))
		(defmacro i2n (_) `(n2r ,_))
		(defmacro f2n (_) `(n2r ,_)))
	(t	;fixed point
		(defmacro vec (&rest _) `(fixeds ~_))
		(defmacro i2n (_) `(n2f ,_))
		(defmacro f2n (_) _)))

(defmacro vec-n2f (&rest _)
	`(fixeds ~(map (lambda (_) (list 'n2f _)) _)))

(defmacro vec-i2n (&rest _)
	`(vec ~(map (lambda (_) (list 'i2n _)) _)))

(defmacro vec-f2n (&rest _)
	`(vec ~(map (lambda (_) (list 'f2n _)) _)))

(enums +dlist 0
	(enum mask light_pos layer1_canvas layer1_verts))

(enums +path 0
	(enum mode color radius path))

(enums +vertex 0
	(enum p v r c))

(defq box_size 500 focal_len 1 max_vel 5 num_bubbles 50 bubble_radius 70
	light_pos (vec-i2n (neg box_size) (neg box_size) (neg (* box_size 4))))
