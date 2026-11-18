;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; what a function of sys/math may trash
;
; The functions of sys/math of a number, d_ f_ and q_, are of a value in
; a register, given back in one, and those who call them are what makes
; code, the shader compiler and the classes of numbers. So they all keep
; to one set, the most any of them trashes, and what calls one keeps what
; it wants in any other register across the call. A float register is
; its double and its single both, :d0 is :f0. lint says of any of them
; that goes outside it. Those that are not of a number, the random, and
; the sums of lines and points for the fixed vector code, are not held
; to it, what they trash is in their comments as any function's is
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defq +sys_math_trashes ''(:r0 :r1 :r2 :d0 :d1 :d2 :d3)
	+sys_math_free ''("sys/math/i_rand" "sys/math/q_intersect" "sys/math/q_dist_sqd"))
