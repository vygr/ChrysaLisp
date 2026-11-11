(include "././list/class.inc")
(include "././mem/class.inc")
(include "./struct.inc")

;;;;;;;;;;;;;;;;;;;
; heap static class
;;;;;;;;;;;;;;;;;;;

(def-class :sys_heap :nil
	(dec-method :init sys/heap/init :static (:r0 :r1 :r2) (:r0) (:r1-:r2))
	(dec-method :deinit sys/heap/deinit :static (:r0) (:r0) (:r1-:r6 :d0-:d15))
	(dec-method :alloc sys/heap/alloc :static (:r0) (:r0 :r1) (:r1-:r2 :d0-:d15))
	(dec-method :free sys/heap/free :static (:r0 :r1) (:r0 :r1) (:r2))
	(dec-method :collect sys/heap/collect :static (:r0) (:r0) (:r1-:r8 :d0-:d15))
	(dec-method :overrun sys/heap/overrun :static :nil :nil ())
	(dec-method :double_free sys/heap/double_free :static :nil :nil ()))

;;;;;;;;;;;;;
; heap macros
;;;;;;;;;;;;;

(defun hp-freecell (%0 %1 %2)
	;inputs
	;%0 = heap (ptr)
	;%1 = cell (ptr)
	;%2 = temp (ptr)
	;outputs
	;%0 = heap (ptr)
	;%1 = cell (ptr)
	;trashes
	;%2
	;info
	;a cell back on its heap's free list, in line. How a heap's cells are
	;set out, and what a validate build keeps after each one and fills it
	;with, only its (:init), (:alloc) and (:free) know, and every boot
	;image holds all three, lib/boot/image.inc, so they are of the one
	;build, the system's, whatever build the function bound to them is. So
	;a validate build does not free in line: it calls (:free), where its
	;checks are, and a function of either build runs on a system of the
	;other.
	(cond
		((< *build_mode* 2)
			(ln-add-fnode %0 +hp_heap_free_flist %1 %2))
		(:t (vp-push :r0 :r1 :r2)
			(vp-push %0 %1)
			(vp-pop :r0 :r1)
			(call :sys_heap :free '(:r0 :r1))
			(vp-pop :r0 :r1 :r2))))
