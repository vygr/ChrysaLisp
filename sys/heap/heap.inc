(import 'sys/class.inc)
(import 'sys/code.inc)
(import 'sys/mem/mem.inc)

;;;;;;;;;;;;;;;;;;;
; heap static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_heap)
(def-method 'init 'sys/heap/init 'static '(r0 r1 r2) '(r0 r1))
(def-method 'deinit 'sys/heap/deinit 'static '(r0) '(r0))
(def-method 'alloc 'sys/heap/alloc 'static '(r0) '(r0 r1))

;;;;;;;;;;;;;;;;;
; heap structures
;;;;;;;;;;;;;;;;;

(def-struct 'hp_heap)
	(ptr 'free_flist)
	(ptr 'block_flist)
	(uint 'cellsize)
	(uint 'blocksize)
(def-struct-end)

;;;;;;;;;;;;;
; heap macros
;;;;;;;;;;;;;

(defcfun hp-freecell (_1 _2 _3)
	;inputs
	;_1 = heap
	;_2 = cell
	;_3 = temp
	;outputs
	;_1 = heap
	;_2 = cell
	;_3 = old first cell

	(ln-add-fnode _1 hp_heap_free_flist _2 _3))
