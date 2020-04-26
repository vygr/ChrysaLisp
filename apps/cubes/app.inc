;common imports and tuples
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'dlist 0
	(byte 'mask 'rate 'layer1_canvas 'layer1_verts))

(structure 'path 0
	(byte 'mode 'color 'radius 'path))

(structure 'vertex 0
	(byte 'x 'y 'z 'r 'c))

(structure 'vertex_vel 0
	(byte 'vx 'vy 'vz))

(defq box_size 500 focal_len 1 max_vel 5)

(cond	;pick number format
	(t	;reals
		(defmacro-bind i2n (_) `(i2r ,_))
		(defmacro-bind n2i (_) `(r2i ,_))
		(defmacro-bind n2f (_) `(r2f ,_)))
	(t		;fixed point
		(defmacro-bind i2n (_) `(i2f ,_))
		(defmacro-bind n2i (_) `(f2i ,_))
		(defmacro-bind n2f (_) _)))
