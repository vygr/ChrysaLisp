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
	(dec-method :free sys/heap/free :static (:r0 :r1))
	(dec-method :collect sys/heap/collect :static (:r0) (:r0)))

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
