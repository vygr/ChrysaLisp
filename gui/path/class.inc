(include 'class/nums/class.inc)

(def-class 'path 'nums)
(dec-method 'vtable 'gui/path/vtable)
(dec-method 'create 'gui/path/create)

(dec-method 'vcreate 'gui/path/create 'final)
(dec-method 'velement 'class/fixed/create 'final)

(dec-method 'filter_polyline 'gui/path/filter_polyline 'static '(r0 r1 r2) '(r0))
(dec-method 'filter_polygon 'gui/path/filter_polygon 'static '(r0 r1 r2) '(r0))
(dec-method 'transform 'gui/path/transform 'static '(r0 r1 r2 r3 r4 r5 r6 r7) '(r0))
(dec-method 'simplify 'gui/path/simplify 'static '(r0 r1 r2 r3) '(r0))

(dec-method 'gen_clerp 'gui/path/gen_clerp 'static '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9) '(r0))
(dec-method 'gen_arc 'gui/path/gen_arc 'static '(r0 r1 r2 r3 r4 r5 r6 r7) '(r0))
(dec-method 'gen_quadratic 'gui/path/gen_quadratic 'static '(r0 r1 r2 r3 r4 r5 r6 r7 r8) '(r0))
(dec-method 'gen_cubic 'gui/path/gen_cubic 'static '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10) '(r0))

(dec-method 'stroke_joints 'gui/path/stroke_joints 'static '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10) '(r0))
(dec-method 'stroke_polylines 'gui/path/stroke_polylines 'static '(r0 r1 r2 r3 r4 r5 r6 r7) '(r0))
(dec-method 'stroke_polygons 'gui/path/stroke_polygons 'static '(r0 r1 r2 r3 r4 r5) '(r0))

(dec-method 'lisp_transform 'gui/path/lisp_transform 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_simplify 'gui/path/lisp_simplify 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_filter 'gui/path/lisp_filter 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_gen_quadratic 'gui/path/lisp_gen_quadratic 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_gen_cubic 'gui/path/lisp_gen_cubic 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_gen_arc 'gui/path/lisp_gen_arc 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_stroke_polylines 'gui/path/lisp_stroke_polylines 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_stroke_polygons 'gui/path/lisp_stroke_polygons 'static '(r0 r1) '(r0 r1))

(def-struct 'path 'nums)
(def-struct-end)

(def-enum 'join)
	(enum 'miter 'bevel 'round)
(def-enum-end)

(def-enum 'cap)
	(enum 'butt 'square 'tri 'arrow 'round)
(def-enum-end)
