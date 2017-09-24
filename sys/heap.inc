(import 'sys/class.inc)
(import 'sys/code.inc)

;;;;;;;;;;;;;;;;;;;
; heap static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_heap)
(def-method 'init 'sys/heap_init 'static '(r0 r1 r2) '(r0 r1))
(def-method 'deinit 'sys/heap_deinit 'static '(r0) '(r0))
(def-method 'alloc 'sys/heap_alloc 'static '(r0) '(r0 r1))

(def-class 'sys_mem)
(def-method 'statics 'sys/mem_statics)
(def-method 'init 'sys/mem_init)
(def-method 'deinit 'sys/mem_deinit)
(def-method 'alloc 'sys/mem_alloc 'static '(r0) '(r0 r1))
(def-method 'free 'sys/mem_free 'static '(r0))
(def-method 'clear 'sys/mem_clear 'static '(r0 r1) '(r0))
(def-method 'fill 'sys/mem_fill 'static '(r0 r1 r2) '(r0))
(def-method 'xor 'sys/mem_xor 'static '(r0 r1 r2) '(r0 r1))
(def-method 'copy 'sys/mem_copy 'static '(r0 r1 r2) '(r0 r1))
(def-method 'realloc 'sys/mem_realloc 'static '(r0 r1 r2) '(r0 r1))
(def-method 'recalloc 'sys/mem_recalloc 'static '(r0 r1 r2) '(r0 r1))
(def-method 'used 'sys/mem_used 'static nil '(r0))

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
