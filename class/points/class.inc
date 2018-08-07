(import 'class/array/class.inc)

(def-class 'points 'array)
(dec-method 'vtable 'class/class_points)
(dec-method 'create 'class/points/create)

(dec-method 'filter_polyline 'class/points/filter_polyline 'static '(r0 r1 r2) '(r0))
(dec-method 'filter_polygon 'class/points/filter_polygon 'static '(r0 r1 r2) '(r0))
(dec-method 'transform 'class/points/transform 'static '(r0 r1 r2 r3 r4) '(r0))
(dec-method 'simplify 'class/points/simplify 'static '(r0 r1 r2 r3) '(r0))

(dec-method 'gen_clerp 'class/points/gen_clerp 'static '(r0 r1 r2 r3 r4 r5 r6) '(r0))
(dec-method 'gen_arc 'class/points/gen_arc 'static '(r0 r1 r2 r3 r4 r5 r6) '(r0))
(dec-method 'gen_quadratic 'class/points/gen_quadratic 'static '(r0 r1 r2 r3 r4 r5) '(r0))
(dec-method 'gen_cubic 'class/points/gen_cubic 'static '(r0 r1 r2 r3 r4 r5 r6) '(r0))

(dec-method 'stroke_joints 'class/points/stroke_joints 'static '(r0 r1 r2 r3 r4 r5 r6 r7 r8) '(r0))
(dec-method 'stroke_polylines 'class/points/stroke_polylines 'static '(r0 r1 r2 r3 r4 r5 r6 r7) '(r0))
(dec-method 'stroke_polygons 'class/points/stroke_polygons 'static '(r0 r1 r2 r3 r4 r5) '(r0))

(dec-method 'lisp_transform 'class/points/lisp_transform 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_simplify 'class/points/lisp_simplify 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_gen_quadratic 'class/points/lisp_gen_quadratic 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_gen_cubic 'class/points/lisp_gen_cubic 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_gen_arc 'class/points/lisp_gen_arc 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_stroke_polylines 'class/points/lisp_stroke_polylines 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_stroke_polygons 'class/points/lisp_stroke_polygons 'static '(r0 r1) '(r0 r1))

(def-struct 'points 'array)
(def-struct-end)

(def-enum 'join)
	(enum 'miter 'bevel 'round)
(def-enum-end)

(def-enum 'cap)
	(enum 'butt 'square 'tri 'arrow 'round)
(def-enum-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;
