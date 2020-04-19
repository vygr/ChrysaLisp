;common imports and tuples
(import 'sys/lisp.inc)
(import 'class/lisp.inc)
(import 'gui/lisp.inc)

(structure 'dlist 0
	(byte 'mask 'rate 'layer1_canvas 'layer1_verts))

(structure 'path 0
	(byte 'mode 'color 'radius 'points))

(structure 'vertex 0
	(byte 'x 'y 'z 'r 'c))

(structure 'vertex_vel 0
	(byte 'vx 'vy 'vz))

(defq box_size 500 focal_len 1 max_vel 10)

(when nil
	(defmacro-bind .= (&rest _) `(= ~_))
	(defmacro-bind .<= (&rest _) `(<= ~_))
	(defmacro-bind .>= (&rest _) `(>= ~_))
	(defmacro-bind .< (&rest _) `(< ~_))
	(defmacro-bind .> (&rest _) `(> ~_))
	(defmacro-bind .+ (&rest _) `(+ ~_))
	(defmacro-bind .- (&rest _) `(- ~_))
	(defmacro-bind ./ (&rest _) `(fdiv ~_))
	(defmacro-bind .* (&rest _) `(fmul ~_))
	(defmacro-bind i2r (_) `(* ,_ 1.0))
	(defmacro-bind f2r (_) _)
	(defmacro-bind r2f (_) _))
