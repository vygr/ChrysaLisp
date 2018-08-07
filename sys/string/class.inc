(import 'sys/class.inc)

;;;;;;;;;;;;;;;;;;;;;
; string static class
;;;;;;;;;;;;;;;;;;;;;

(def-class 'sys_string)
(dec-method 'length 'sys/string/length 'static '(r0) '(r0 r1))
(dec-method 'copy 'sys/string/copy 'static '(r0 r1) '(r0 r1))
(dec-method 'compare 'sys/string/compare 'static '(r0 r1) '(r0))
(dec-method 'to_long 'sys/string/to_long 'static '(r0 r1) '(r0))
(dec-method 'from_long 'sys/string/from_long 'static '(r0 r1 r2))
