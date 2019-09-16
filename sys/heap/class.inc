(include 'sys/list/class.inc)
(include 'sys/mem/class.inc)

;;;;;;;;;;;;;;;;;;;
; heap static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_heap)
(dec-method 'init 'sys/heap/init 'static '(r0 r1 r2) '(r0 r1))
(dec-method 'deinit 'sys/heap/deinit 'static '(r0) '(r0))
(dec-method 'alloc 'sys/heap/alloc 'static '(r0) '(r0 r1))
(dec-method 'free 'sys/heap/free 'static '(r0 r1))
(dec-method 'collect 'sys/heap/collect 'static '(r0) '(r0))

;;;;;;;;;;;;;;;;;
; heap structures
;;;;;;;;;;;;;;;;;

(def-struct 'hp_heap)
	(ptr 'free_flist 'block_flist)
	(uint 'cellsize 'blocksize)
(def-struct-end)

(def-struct 'hp_block 'ln_fnode)
	(ptr 'free_flist 'free_flist_tail)
	(uint 'blocksize 'cellcnt)
(def-struct-end)

;;;;;;;;;;;;;;;;
; inline methods
;;;;;;;;;;;;;;;;

(defcfun sys/heap/free ()
	;inputs
	;r0 = heap (ptr)
	;r1 = cell (ptr)
	;outputs
	;r0 = heap (ptr)
	;r1 = cell (ptr)
	;trashes
	;r2
	(hp-freecell r0 r1 r2))

;;;;;;;;;;;;;
; heap macros
;;;;;;;;;;;;;

(defcfun hp-freecell (_1 _2 _3)
	;inputs
	;_1 = heap (ptr)
	;_2 = cell (ptr)
	;_3 = temp (ptr)
	;outputs
	;_1 = heap (ptr)
	;_2 = cell (ptr)
	;_3 = old first cell (ptr)

	(ln-add-fnode _1 hp_heap_free_flist _2 _3))
