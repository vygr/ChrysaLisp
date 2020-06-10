(include 'sys/class.inc)

;;;;;;;;;;;;;;;;;;;;;
; string static class
;;;;;;;;;;;;;;;;;;;;;

(def-class 'sys_str)
(dec-method :length 'sys/str/length :static '(r0) '(r0 r1))
(dec-method :copy 'sys/str/copy :static '(r0 r1) '(r0 r1))
(dec-method :compare 'sys/str/compare :static '(r0 r1) '(r0))
(dec-method :to_long 'sys/str/to_long :static '(r0 r1) '(r0 r4))
(dec-method :from_long 'sys/str/from_long :static '(r0 r1 r2) '(r0))
(dec-method :read_utf8 'sys/str/read_utf8 :static '(r0) '(r0 r1))
