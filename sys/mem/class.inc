(include 'sys/class.inc)

;;;;;;;;;;;;;;;;;;
; mem static class
;;;;;;;;;;;;;;;;;;

(def-class 'sys_mem)
(dec-method :statics 'sys/mem/statics)
(dec-method :statics_init 'sys/mem/statics_init)
(dec-method :statics_deinit 'sys/mem/statics_deinit)
(dec-method :alloc 'sys/mem/alloc :static '(r0) '(r0 r1))
(dec-method :calloc 'sys/mem/calloc :static '(r0) '(r0 r1))
(dec-method :free 'sys/mem/free :static '(r0))
(dec-method :fill 'sys/mem/fill :static '(r0 r1 r2) '(r0))
(dec-method :copy 'sys/mem/copy :static '(r0 r1 r2) '(r0 r1))
(dec-method :realloc 'sys/mem/realloc :static '(r0 r1 r2) '(r0 r1))
(dec-method :recalloc 'sys/mem/recalloc :static '(r0 r1 r2) '(r0 r1))
(dec-method :collect 'sys/mem/collect)
(dec-method :used 'sys/mem/used :static nil '(r0))

(dec-method :lisp_stats 'sys/mem/lisp_stats :static '(r0 r1) '(r0 r1))

(def-struct 'sys_mem_statics)
	(struct 'obj_list 'lh_list)
	(long 'total)
(def-struct-end)

(def-struct 'sys_mem_header)
	(when (> *debug_mode* 1)
		(struct 'node 'ln_node))
	(ptr 'heap)
(def-struct-end)
