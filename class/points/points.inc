(import 'class/array/array.inc)

(def-class 'points 'array)
(def-method 'create 'class/points/create 'static nil '(r0))
(def-method 'new 'class/points/new 'static nil '(r0))
(def-method 'rem_co_polyline 'class/points/rem_co_polyline 'static '(r0 r1 r2) '(r0))
(def-method 'rem_co_polygon 'class/points/rem_co_polygon 'static '(r0 r1 r2) '(r0))
(def-method 'transform 'class/points/transform 'static '(r0 r1 r2 r3 r5) '(r0))

(def-method 'gen_clerp 'class/points/gen_clerp 'static '(r0 r1 r2 r3 r5 r6 r7) '(r0))
(def-method 'gen_arc 'class/points/gen_arc 'static '(r0 r1 r2 r3 r5 r6 r7) '(r0))
(def-method 'gen_quadratic 'class/points/gen_quadratic 'static '(r0 r1 r2 r3 r5 r6) '(r0))
(def-method 'gen_cubic 'class/points/gen_cubic 'static '(r0 r1 r2 r3 r5 r6 r7) '(r0))

(def-method 'stroke_joints 'class/points/stroke_joints 'static '(r0 r1 r2 r3 r5 r6 r7 r8 r9) '(r0))
(def-method 'stroke_polylines 'class/points/stroke_polylines 'static '(r0 r1 r2 r3 r5 r6 r7 r8) '(r0))
(def-method 'stroke_polygons 'class/points/stroke_polygons 'static '(r0 r1 r2 r3 r5 r6) '(r0))

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
