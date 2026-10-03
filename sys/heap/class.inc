(include "././list/class.inc")
(include "././mem/class.inc")
(include "./struct.inc")

;;;;;;;;;;;;;;;;;;;
; heap static class
;;;;;;;;;;;;;;;;;;;

(def-class :sys_heap :nil
	(dec-method :init sys/heap/init :static (:r0 :r1 :r2) (:r0))
	(dec-method :deinit sys/heap/deinit :static (:r0) (:r0))
	(dec-method :alloc sys/heap/alloc :static (:r0) (:r0 :r1))
	(dec-method :free sys/heap/free :static (:r0 :r1) (:r0 :r1))
	(dec-method :collect sys/heap/collect :static (:r0) (:r0))
	(dec-method :overrun sys/heap/overrun)
	(dec-method :double_free sys/heap/double_free))

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
	;%2 = old first cell (ptr)

	(validatecase
		;the guard word after the cell must be whole, else something wrote
		;past the end of the cell.
		;fill the cell, past its free list link, with the freed pattern, so
		;any use of it after the free shows up. If it holds that pattern
		;already then it is being freed twice.
		(defq regs (filter (lambda (r) (not (find r (list %0 %1 %2))))
				'(:r3 :r4 :r5 :r6 :r7 :r8))
			end (first regs) fill (second regs) tmp (third regs))
		(vp-push end fill tmp)
		(assign `((,%0 +hp_heap_cellsize)) `(,end))
		(vp-lea-d %1 end end)
		(vp-cpy-cr +hp_cell_guard fill)
		(assign `((,end 0)) `(,tmp))
		(vpif `(,tmp /= ,fill))
			(jump :sys_heap :overrun)
		(endif)
		(vp-lea-i %1 +ptr_size %2)
		(vp-cpy-cr +hp_cell_free_fill fill)
		(vpif `(,%2 /= ,end))
			(assign `((,%2 0)) `(,tmp))
			(vpif `(,tmp = ,fill))
				(jump :sys_heap :double_free)
			(endif)
			(loop-start)
				(vp-cpy-ri fill %2 0)
				(vp-add-cr +long_size %2)
			(loop-until `(,%2 = ,end))
		(endif)
		(vp-pop end fill tmp))
	(ln-add-fnode %0 +hp_heap_free_flist %1 %2))

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defun sys/heap/free ()
	;inputs
	;:r0 = heap (ptr)
	;:r1 = cell (ptr)
	;outputs
	;:r0 = heap (ptr)
	;:r1 = cell (ptr)
	;trashes
	;:r2
	(hp-freecell :r0 :r1 :r2))
