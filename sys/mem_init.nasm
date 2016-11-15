%include 'inc/func.ninc'
%include 'inc/heap.ninc'

def_func sys/mem_init
	;get statics
	f_bind sys_mem, statics, r0
	vp_add long_size, r0

	;mem_slots heaps
	vp_cpy mem_block_min_size, r1	;start object size
	vp_cpy mem_block_blk_size, r3	;start block size
	loop_start
		f_call sys_heap, init, {r0, r1, r3}
		vp_add hp_heap_size, r0
		vp_add r1, r1				;double object size
		vpif r3, <, r1
			vp_cpy r1, r3			;at least 1 object per block !
		endif
	loop_until r1, >, mem_block_max_size
	vp_ret

def_func_end
