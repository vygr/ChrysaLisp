%include 'inc/func.inc'
%include 'inc/heap.inc'

	def_func sys/mem_statics

		dq 0	;used
		%assign blk_size mem_block_min_size
		%rep 1000
			times hp_heap_size db 0
			%assign blk_size blk_size * 2
			%if blk_size > mem_block_max_size
				%exitrep
			%endif
		%endrep

	def_func_end
