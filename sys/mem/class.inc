(include "sys/class.inc")

;;;;;;;;;;;;;;;;;;
; mem static class
;;;;;;;;;;;;;;;;;;

(def-class sys_mem nil
	(dec-method :statics_init sys/mem/statics_init)
	(dec-method :statics_deinit sys/mem/statics_deinit)
	(dec-method :alloc sys/mem/alloc :static (:r0) (:r0 :r1))
	(dec-method :calloc sys/mem/calloc :static (:r0) (:r0 :r1))
	(dec-method :free sys/mem/free :static (:r0))
	(dec-method :fill sys/mem/fill :static (:r0 :r1 :r2) (:r0))
	(dec-method :copy sys/mem/copy :static (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :realloc sys/mem/realloc :static (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :recalloc sys/mem/recalloc :static (:r0 :r1 :r2) (:r0 :r1))
	(dec-method :avail sys/mem/avail :static nil (:r0))
	(dec-method :collect sys/mem/collect))

(defq mem_cell_min_size 0x000020 mem_cell_max_size 0x4000000
	max_cell_count (/ (* 32 1024) mem_cell_min_size))

(def-struct sys_mem_header 0
	(ptr heap))
