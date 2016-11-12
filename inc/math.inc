%ifndef MATH_1234
%define MATH_1234

(import 'inc/class.inc)

;;;;;;;;;;;;;;;;;;;
; math static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_math)
(def-method 'random 'sys/math_random 'static '(r0) '(r0))

%endif
