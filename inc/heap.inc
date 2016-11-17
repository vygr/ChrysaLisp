(import 'inc/class.inc)

;;;;;;;;;;;;;;;;;;;
; heap static class
;;;;;;;;;;;;;;;;;;;

(def-class 'sys_heap)
(def-method 'init 'sys/heap_init 'static '(r0 r1 r2))
(def-method 'deinit 'sys/heap_deinit 'static '(r0))
(def-method 'alloc 'sys/heap_alloc 'static '(r0) '(r1))
(def-method 'reset 'sys/heap_reset 'static '(r0))

(equate 'mem_block_min_size 0x000020)
(equate 'mem_block_max_size 0x800000)
(equate 'mem_block_blk_size (mul 16 1024))

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
(def-method 'grow 'sys/mem_grow 'static '(r0 r1 r2) '(r0 r1))
(def-method 'used 'sys/mem_used 'static '() '(r0))

;;;;;;;;;;;;;;;;;
; heap structures
;;;;;;;;;;;;;;;;;

(def-struct 'hp_heap)
	(ptr 'hp_heap_free_flist)
	(ptr 'hp_heap_block_flist)
	(ulong 'hp_heap_cellsize)
	(ulong 'hp_heap_blocksize)
(def-struct-end)

;;;;;;;;;;;;;
; heap macros
;;;;;;;;;;;;;

%macro hp_freecell 3
	;inputs
	;%1 = heap
	;%2 = cell
	;%3 = temp
	;outputs
	;%1 = heap
	;%2 = cell
	;%3 = old first cell

	ln-add-fnode %1 + hp_heap_free_flist, %2, %3
%endmacro
