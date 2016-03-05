%ifndef HEAP_1234
	%define HEAP_1234

%include 'inc/class.inc'

;;;;;;;;;;;;;;;;;;;
; heap static class
;;;;;;;;;;;;;;;;;;;

	def_class heap
	def_method init, sys/heap_init
	def_method deinit, sys/heap_deinit
	def_method alloc, sys/heap_alloccell
	def_method reset, sys/heap_reset

;;;;;;;;;;;;;;;;;
; heap structures
;;;;;;;;;;;;;;;;;

	struc hp_heap
		hp_heap_freelist:	resq 1
		hp_heap_blocklist:	resq 1
		hp_heap_cellsize:	resq 1
		hp_heap_blocksize:	resq 1
	endstruc

	struc hp_cell
		hp_cell_next:	resq 1
	endstruc

	struc hp_block
		hp_block_next:	resq 1
	endstruc

;;;;;;;;;;;;;
; heap macros
;;;;;;;;;;;;;

	%macro hp_heap_object 1
		align 8
	%1:
		times hp_heap_size db 0
	%endmacro

	%macro hp_freecell 3
		;inputs
		;%1 = heap
		;%2 = cell
		;%3 = temp
		;outputs
		;%1 = heap
		;%2 = cell
		;%3 = old first cell

		vp_cpy [%1 + hp_heap_freelist], %3
		vp_cpy %2, [%1 + hp_heap_freelist]
		vp_cpy %3, [%2 + hp_cell_next]
	%endmacro

%endif
