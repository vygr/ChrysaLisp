(import 'sys/class.inc)

;;;;;;;;;;;;;;;;;;
; mem static class
;;;;;;;;;;;;;;;;;;

(def-class 'sys_mem)
(def-method 'statics 'sys/mem/statics)
(def-method 'init 'sys/mem/init)
(def-method 'deinit 'sys/mem/deinit)
(def-method 'alloc 'sys/mem/alloc 'static '(r0) '(r0 r1))
(def-method 'calloc 'sys/mem/calloc 'static '(r0) '(r0 r1))
(def-method 'free 'sys/mem/free 'static '(r0))
(def-method 'clear 'sys/mem/clear 'static '(r0 r1) '(r0))
(def-method 'fill 'sys/mem/fill 'static '(r0 r1 r2) '(r0))
(def-method 'copy 'sys/mem/copy 'static '(r0 r1 r2) '(r0 r1))
(def-method 'realloc 'sys/mem/realloc 'static '(r0 r1 r2) '(r0 r1))
(def-method 'recalloc 'sys/mem/recalloc 'static '(r0 r1 r2) '(r0 r1))
(def-method 'used 'sys/mem/used 'static nil '(r0))
