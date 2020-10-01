;common imports and tuples
(import "sys/lisp.inc")
(import "class/lisp.inc")
(import "gui/lisp.inc")

;quick debug switch
;; (import "lib/debug/debug.inc")
;; (defmacro defun-bind (&rest _) `(defun-debug ~_))

(cond	;pick number format t/nil
	(t	;reals
		(defmacro-bind vec (&rest _) `(reals ~_))
		(defmacro-bind i2n (_) `(i2r ,_))
		(defmacro-bind n2i (_) `(r2i ,_))
		(defmacro-bind f2n (_) `(f2r ,_))
		(defmacro-bind n2f (_) `(r2f ,_)))
	(t	;fixed point
		(defmacro-bind vec (&rest _) `(fixeds ~_))
		(defmacro-bind i2n (_) `(i2f ,_))
		(defmacro-bind n2i (_) `(f2i ,_))
		(defmacro-bind f2n (_) _)
		(defmacro-bind n2f (_) _)))

(defmacro-bind vec-n2f (&rest _)
	`(fixeds ~(map (lambda (_) (list 'n2f _)) _)))

(defmacro-bind vec-i2n (&rest _)
	`(vec ~(map (lambda (_) (list 'i2n _)) _)))

(defmacro-bind vec-f2n (&rest _)
	`(vec ~(map (lambda (_) (list 'f2n _)) _)))

(structure '+dlist 0
	(byte 'mask+ 'rate+ 'light_pos+ 'layer1_canvas+ 'layer1_verts+))

(structure '+path 0
	(byte 'mode+ 'color+ 'radius+ 'path+))

(structure '+vertex 0
	(byte 'p+ 'v+ 'r+ 'c+))

(defq box_size 500 focal_len 1 max_vel 5 num_bubbles 50 bubble_radius 70
	light_pos (vec-i2n (neg box_size) (neg box_size) (neg (* box_size 4))))
