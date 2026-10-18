(include "lib/asm/class.inc")
(include "./struct.inc")

;;;;;;;;;;;;;;;;;;
; mem static class
;;;;;;;;;;;;;;;;;;

(def-class :sys_mem :nil
	(dec-method :statics_init sys/mem/statics_init :static :nil :nil (:r0-:r5))
	(dec-method :statics_deinit sys/mem/statics_deinit :static :nil :nil (:r0-:r7 :f0-:f15))
	(dec-method :alloc sys/mem/alloc :static (:r0) (:r0 :r1) (:r0-:r2 :f0-:f15))
	(dec-method :calloc sys/mem/calloc :static (:r0) (:r0 :r1) (:r0-:r3 :f0-:f15))
	(dec-method :free sys/mem/free :static (:r0) :nil (:r0-:r2))
	(dec-method :fill sys/mem/fill :static (:r0 :r1 :r2) (:r0) (:r0-:r1 :r3))
	(dec-method :copy sys/mem/copy :static (:r0 :r1 :r2) (:r0 :r1) (:r0-:r3))
	(dec-method :copy_to_ring sys/mem/copy_to_ring :static (:r0 :r1 :r2 :r3 :r4) (:r0 :r1) (:r0-:r3 :r5))
	(dec-method :copy_from_ring sys/mem/copy_from_ring :static (:r0 :r1 :r2 :r3 :r4) (:r0 :r1) (:r0-:r3 :r5))
	(dec-method :copy_in_ring sys/mem/copy_in_ring :static (:r0 :r1 :r2 :r3 :r4) (:r0 :r1) (:r0-:r3 :r5-:r8))
	(dec-method :realloc sys/mem/realloc :static (:r0 :r1 :r2) (:r0 :r1) (:r0-:r5 :f0-:f15))
	(dec-method :recalloc sys/mem/recalloc :static (:r0 :r1 :r2) (:r0 :r1) (:r0-:r7 :f0-:f15))
	(dec-method :avail sys/mem/avail :static :nil (:r0) (:r0-:r4))
	(dec-method :inuse sys/mem/inuse :static :nil (:r0) (:r0-:r6))
	(dec-method :collect sys/mem/collect :static :nil :nil (:r0-:r9 :f0-:f15)))
