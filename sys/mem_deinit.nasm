%include 'inc/func.inc'
%include 'inc/heap.inc'

def_func sys/mem_deinit
	;get statics
	f_bind sys_mem, statics, r0
	vp_cpy r0, r5

	;free memory heaps
	vp_cpy mem_block_min_size, r6	;start object size
	loop_start
		f_call sys_heap, deinit, {r5}
		vp_add hp_heap_size, r5
		vp_add r6, r6
	loop_until r6, >, mem_block_max_size
	vp_ret

def_func_end
