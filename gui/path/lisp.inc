;lisp bindings
(ffi path-gen-arc "gui/path/lisp_gen_arc" 0)
(ffi path-gen-cubic "gui/path/lisp_gen_cubic" 0)
(ffi path-gen-quadratic "gui/path/lisp_gen_quadratic" 0)
(ffi path-filter "gui/path/lisp_filter" 0)
(ffi path-simplify "gui/path/lisp_simplify" 0)
(ffi path-stroke-polygon "gui/path/lisp_stroke_polygon" 0)
(ffi path-stroke-polyline "gui/path/lisp_stroke_polyline" 0)
(ffi path-stroke-polygons "gui/path/lisp_stroke_polygons" 0)
(ffi path-stroke-polylines "gui/path/lisp_stroke_polylines" 0)
(ffi path-transform "gui/path/lisp_transform" 0)

(enums +join 0
	(enum miter bevel round))

(enums +cap 0
	(enum butt square tri arrow round))
