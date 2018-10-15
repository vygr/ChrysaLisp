(import 'class/array/class.inc)

(def-class 'points 'array)
(dec-method 'vtable 'gui/class_points)
(dec-method 'create 'gui/points/create)
(dec-method 'get_first2 'gui/points/get_first2 'static '(r0) '(r0 r1 r2))
(dec-method 'get_element2 'gui/points/get_element2 'static '(r0 r1) '(r0 r1 r2))
(dec-method 'push_back2 'gui/points/push_back2 'static '(r0 r1 r2) '(r0 r1 r2))
(dec-method 'pop_back2 'gui/points/pop_back2 'static '(r0) '(r0))
(dec-method 'add 'gui/points/add 'static '(r0 r1 r2) '(r0))
(dec-method 'sub 'gui/points/sub 'static '(r0 r1 r2) '(r0))
(dec-method 'mul 'gui/points/mul 'static '(r0 r1 r2) '(r0))
(dec-method 'div 'gui/points/div 'static '(r0 r1 r2) '(r0))
(dec-method 'mod 'gui/points/mod 'static '(r0 r1 r2) '(r0))
(dec-method 'sum 'gui/points/sum 'static '(r0) '(r0 r1))
(dec-method 'fmul 'gui/points/fmul 'static '(r0 r1 r2) '(r0))
(dec-method 'fdiv 'gui/points/fdiv 'static '(r0 r1 r2) '(r0))
(dec-method 'fmod 'gui/points/fmod 'static '(r0 r1 r2) '(r0))
(dec-method 'fscale 'gui/points/fscale 'static '(r0 r1 r2) '(r0))
(dec-method 'abs 'gui/points/abs 'static '(r0 r1) '(r0))
(dec-method 'frac 'gui/points/frac 'static '(r0 r1) '(r0))

(dec-method 'filter_polyline 'gui/points/filter_polyline 'static '(r0 r1 r2) '(r0))
(dec-method 'filter_polygon 'gui/points/filter_polygon 'static '(r0 r1 r2) '(r0))
(dec-method 'transform 'gui/points/transform 'static '(r0 r1 r2 r3 r4 r5 r6 r7) '(r0))
(dec-method 'simplify 'gui/points/simplify 'static '(r0 r1 r2 r3) '(r0))

(dec-method 'gen_clerp 'gui/points/gen_clerp 'static '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9) '(r0))
(dec-method 'gen_arc 'gui/points/gen_arc 'static '(r0 r1 r2 r3 r4 r5 r6 r7) '(r0))
(dec-method 'gen_quadratic 'gui/points/gen_quadratic 'static '(r0 r1 r2 r3 r4 r5 r6 r7 r8) '(r0))
(dec-method 'gen_cubic 'gui/points/gen_cubic 'static '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10) '(r0))

(dec-method 'stroke_joints 'gui/points/stroke_joints 'static '(r0 r1 r2 r3 r4 r5 r6 r7 r8 r9 r10) '(r0))
(dec-method 'stroke_polylines 'gui/points/stroke_polylines 'static '(r0 r1 r2 r3 r4 r5 r6 r7) '(r0))
(dec-method 'stroke_polygons 'gui/points/stroke_polygons 'static '(r0 r1 r2 r3 r4 r5) '(r0))

(dec-method 'lisp_vecop1 'gui/points/lisp_vecop1 'static '(r0 r1 r2) '(r0 r1))
(dec-method 'lisp_vecop2 'gui/points/lisp_vecop2 'static '(r0 r1 r2) '(r0 r1))

(dec-method 'lisp_add 'gui/points/lisp_add 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_sub 'gui/points/lisp_sub 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_mul 'gui/points/lisp_mul 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_div 'gui/points/lisp_div 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_mod 'gui/points/lisp_mod 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_sum 'gui/points/lisp_sum 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_fmul 'gui/points/lisp_fmul 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_fdiv 'gui/points/lisp_fdiv 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_fmod 'gui/points/lisp_fmod 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_fscale 'gui/points/lisp_fscale 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_abs 'gui/points/lisp_abs 'static '(r0 r1) '(r0 r1))
(dec-method 'lisp_frac 'gui/points/lisp_frac 'static '(r0 r1) '(r0 r1))

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

(defcfun gui/points/get_first2 ()
	;inputs
	;r0 = points object (ptr)
	;outputs
	;r0 = points object (ptr)
	;r1 = element1 (long)
	;r2 = element2 (long)
	(vp-cpy-ir r0 array_array r2)
	(vp-cpy-ir r2 0 r1)
	(vp-cpy-ir r2 long_size r2))

(defcfun gui/points/get_element2 ()
	;inputs
	;r0 = points object (ptr)
	;r1 = element index (uint)
	;outputs
	;r0 = points object (ptr)
	;r1 = element1 (long)
	;r2 = element2 (long)
	(vp-cpy-ir r0 array_array r2)
	(vp-shl-cr (log2 long_size) r1)
	(vp-add-rr r1 r2)
	(vp-cpy-ir r2 0 r1)
	(vp-cpy-ir r2 long_size r2))

(defcfun gui/points/pop_back2 ()
	;inputs
	;r0 = points object (ptr)
	;outputs
	;r0 = points object (ptr)
	;trashes
	;r1
	(vp-cpy-ir-ui r0 array_length r1)
	(vp-sub-cr 2 r1)
	(vp-cpy-ri-i r1 r0 array_length))
