%ifndef STRING_1234
%define STRING_1234

(import 'inc/class.inc)

;;;;;;;;;;;;;;;;;;;;;
; string static class
;;;;;;;;;;;;;;;;;;;;;

(def-class 'sys_string)
(def-method 'length 'sys/string_length 'static '(r0), '(r1))
(def-method 'copy 'sys/string_copy 'static '(r0, r1), '(r0, r1))
(def-method 'compare 'sys/string_compare 'static '(r0, r1), '(r0))
(def-method 'to_long 'sys/string_to_long 'static '(r0, r1), '(r0))
(def-method 'from_long 'sys/string_from_long 'static '(r0, r1, r2))
(def-method 'hash 'sys/string_hash 'static '(r0), '(r1))

%endif
