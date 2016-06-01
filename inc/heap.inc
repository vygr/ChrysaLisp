%ifndef HEAP_1234
%define HEAP_1234

%include 'inc/class.inc'

;;;;;;;;;;;;;;;;;;;
; heap static class
;;;;;;;;;;;;;;;;;;;

	def_class sys_heap
	def_method init, sys/heap_init, static, {r0, r1, r2}
	def_method deinit, sys/heap_deinit, static, {r0}
	def_method alloc, sys/heap_alloc, static, {r0}, {r1}
	def_method reset, sys/heap_reset, static, {r0}

	mem_block_min_size	equ 0x000040
	mem_block_max_size	equ 0x800000

	def_class sys_mem
	def_method statics, sys/mem_statics
	def_method init, sys/mem_init
	def_method deinit, sys/mem_deinit
	def_method alloc, sys/mem_alloc, static, {r0}, {r0, r1}
	def_method free, sys/mem_free, static, {r0}
	def_method clear, sys/mem_clear, static, {r0, r1}, {r0}
	def_method fill, sys/mem_fill, static, {r0, r1, r2}, {r0}
	def_method xor, sys/mem_xor, static, {r0, r1, r2}, {r0, r1}
	def_method copy, sys/mem_copy, static, {r0, r1, r2}, {r0, r1}
	def_method grow, sys/mem_grow, static, {r0, r1, r2}, {r0, r1}

;;;;;;;;;;;;;;;;;
; heap structures
;;;;;;;;;;;;;;;;;

	def_structure hp_heap
		ptr hp_heap_free_flist
		ptr hp_heap_block_flist
		ulong hp_heap_cellsize
		ulong hp_heap_blocksize
	def_structure_end

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

		ln_add_fnode %1 + hp_heap_free_flist, %2, %3
	%endmacro

%endif
