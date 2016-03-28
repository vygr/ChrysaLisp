%ifndef HEAP_1234
%define HEAP_1234

%include 'inc/class.inc'

;;;;;;;;;;;;;;;;;;;
; heap static class
;;;;;;;;;;;;;;;;;;;

	def_class sys_heap
	def_method init, sys/heap_init
	def_method deinit, sys/heap_deinit
	def_method alloc, sys/heap_alloc
	def_method reset, sys/heap_reset

	def_class sys_mem
	def_method statics, sys/mem_statics
	def_method init, sys/mem_init
	def_method deinit, sys/mem_deinit
	def_method alloc, sys/mem_alloc
	def_method free, sys/mem_free
	def_method clear, sys/mem_clear
	def_method copy, sys/mem_copy
	def_method grow, sys/mem_grow

;;;;;;;;;;;;;;;;;
; heap structures
;;;;;;;;;;;;;;;;;

	def_structure	hp_heap
		def_long	hp_heap_freelist
		def_long	hp_heap_blocklist
		def_long	hp_heap_cellsize
		def_long	hp_heap_blocksize
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

		add_fnode %1 + hp_heap_freelist, %2, %3
	%endmacro

%endif
