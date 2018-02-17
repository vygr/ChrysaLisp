(import 'sys/class.inc)

;;;;;;;;;;;;;;;;;;;;;
; string static class
;;;;;;;;;;;;;;;;;;;;;

(def-class 'sys_string)
(def-method 'length 'sys/string/length 'static '(r0) '(r0 r1))
(def-method 'copy 'sys/string/copy 'static '(r0 r1) '(r0 r1))
(def-method 'compare 'sys/string/compare 'static '(r0 r1) '(r0))
(def-method 'to_long 'sys/string/to_long 'static '(r0 r1) '(r0))
(def-method 'from_long 'sys/string/from_long 'static '(r0 r1 r2))
