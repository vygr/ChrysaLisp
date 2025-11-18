;;;;;;;;;;;;;;;;;;;;;;;;;;
; OpenGL Lisp Bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro-bind gl-init ()
	`(call 'host_gl :lisp_init (list) '(,@_)))

(defmacro-bind gl-clear (_mask)
	`(call 'host_gl :lisp_clear (list ,_mask) '(,@_)))

(defmacro-bind gl-viewport (_x _y _w _h)
	`(call 'host_gl :lisp_viewport (list ,_x ,_y ,_w ,_h) '(,@_)))

(defmacro-bind gl-begin (_mode)
	`(call 'host_gl :lisp_begin (list ,_mode) '(,@_)))

(defmacro-bind gl-end ()
	`(call 'host_gl :lisp_end (list) '(,@_)))

(defmacro-bind gl-vertex3f (_x _y _z)
	`(call 'host_gl :lisp_vertex3f (list ,_x ,_y ,_z) '(,@_)))

(defmacro-bind gl-color3f (_r _g _b)
	`(call 'host_gl :lisp_color3f (list ,_r ,_g ,_b) '(,@_)))

(defmacro-bind gl-flush ()
	`(call 'host_gl :lisp_flush (list) '(,@_)))
