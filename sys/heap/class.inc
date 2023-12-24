(include "././list/class.inc")
(include "././mem/class.inc")

;;;;;;;;;;;;;;;;;;;
; heap static class
;;;;;;;;;;;;;;;;;;;

(def-class sys_heap :nil
	(dec-method :init sys/heap/init :static (:r0 :r1 :r2) (:r0))
	(dec-method :deinit sys/heap/deinit :static (:r0) (:r0))
	(dec-method :alloc sys/heap/alloc :static (:r0) (:r0 :r1))
	(dec-method :free sys/heap/free :static (:r0 :r1))
	(dec-method :collect sys/heap/collect :static (:r0) (:r0)))

;;;;;;;;;;;;;;;;;
; heap structures
;;;;;;;;;;;;;;;;;

(def-struct hp_heap lh_list_size
	(ptr free_flist)
	(uint cellsize blocksize))

(def-struct hp_block ln_node_size
	(ptr free_flist free_flist_tail)
	(uint cellcnt freecnt))

(def-struct hp_cell 0
	(ptr block next))

;;;;;;;;;;;;;
; heap macros
;;;;;;;;;;;;;

(defun hp-freecell (_1 _2 _3)
	;inputs
	;_1 = heap (ptr)
	;_2 = cell (ptr)
	;_3 = temp (ptr)
	;outputs
	;_1 = heap (ptr)
	;_2 = cell (ptr)
	;_3 = old first cell (ptr)

	(ln-add-fnode _1 hp_heap_free_flist _2 _3))

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
