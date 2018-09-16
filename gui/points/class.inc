(import 'class/array/class.inc)

(def-class 'points 'array)
(dec-method 'vtable 'gui/class_points)
(dec-method 'create 'gui/points/create)

(dec-method 'filter_polyline 'gui/points/filter_polyline 'static '(r0 r1 r2) '(r0))
(dec-method 'filter_polygon 'gui/points/filter_polygon 'static '(r0 r1 r2) '(r0))
(dec-method 'transform 'gui/points/transform 'static '(r0 r1 r2 r3 r4) '(r0))
(dec-method 'simplify 'gui/points/simplify 'static '(r0 r1 r2 r3) '(r0))

(dec-method 'gen_clerp 'gui/points/gen_clerp 'static '(r0 r1 r2 r3 r4 r5 r6) '(r0))
(dec-method 'gen_arc 'gui/points/gen_arc 'static '(r0 r1 r2 r3 r4 r5 r6) '(r0))
(dec-method 'gen_quadratic 'gui/points/gen_quadratic 'static '(r0 r1 r2 r3 r4 r5) '(r0))
(dec-method 'gen_cubic 'gui/points/gen_cubic 'static '(r0 r1 r2 r3 r4 r5 r6) '(r0))

(dec-method 'stroke_joints 'gui/points/stroke_joints 'static '(r0 r1 r2 r3 r4 r5 r6 r7 r8) '(r0))
(dec-method 'stroke_polylines 'gui/points/stroke_polylines 'static '(r0 r1 r2 r3 r4 r5 r6 r7) '(r0))
(dec-method 'stroke_polygons 'gui/points/stroke_polygons 'static '(r0 r1 r2 r3 r4 r5) '(r0))

(dec-method 'lisp_transform 'gui/points/lisp_transform 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_simplify 'gui/points/lisp_simplify 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_gen_quadratic 'gui/points/lisp_gen_quadratic 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_gen_cubic 'gui/points/lisp_gen_cubic 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_gen_arc 'gui/points/lisp_gen_arc 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_stroke_polylines 'gui/points/lisp_stroke_polylines 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_stroke_polygons 'gui/points/lisp_stroke_polygons 'static '(r0 r1) '(r0 r1))

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
