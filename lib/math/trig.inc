;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; the inverse turns, of a double or a float: atan, asin and acos, and atan2
; of two of the same, the angle of the point x y from -pi to pi. Each is
; its class's method, by the :sys_math function of its width. Bound here
; and not in root.inc till a release, as a node of an older boot image
; would not start with them there
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ffi "class/double/lisp_atan" atan)
; (atan double) -> double
(ffi "class/double/lisp_asin" asin)
; (asin double) -> double
(ffi "class/double/lisp_acos" acos)
; (acos double) -> double
(ffi "class/double/lisp_atan2" atan2)
; (atan2 y x) -> double
