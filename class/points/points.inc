(import 'class/array/array.inc)

(def-class 'points 'array)
(def-method 'create 'class/points/create 'static nil '(r0))
(def-method 'new 'class/points/new 'static nil '(r0))
(def-method 'rem_co_polyline 'class/points/rem_co_polyline 'static '(r0 r1 r2) '(r0))
(def-method 'rem_co_polygon 'class/points/rem_co_polygon 'static '(r0 r1 r2) '(r0))
(def-method 'gen_clerp 'class/points/gen_clerp 'static '(r0 r1 r2 r3 r5 r6 r7 r8 r9 r10) '(r0))
(def-method 'gen_bezier 'class/points/gen_bezier 'static '(r0 r1 r2 r3 r5 r6 r7 r8 r9 r10 r11) '(r0))

(def-struct 'points 'array)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;
