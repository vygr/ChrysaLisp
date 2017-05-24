(import 'class/array/array.inc)

(def-class 'points 'array)
(def-method 'create 'class/points/create 'static nil '(r0))
(def-method 'new 'class/points/new 'static nil '(r0))
(def-method 'rem_co_polyline 'class/points/rem_co_polyline 'static '(r0 r1 r2) '(r0))
(def-method 'rem_co_polygon 'class/points/rem_co_polygon 'static '(r0 r1 r2) '(r0))

(def-struct 'points 'array)
(def-struct-end)

;;;;;;;;;;;;;;;
;inline methods
;;;;;;;;;;;;;;;
