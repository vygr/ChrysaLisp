%include 'inc/func.inc'
%include 'inc/heap.inc'

	def_function sys/mem_used
		;outputs
		;r0 = amount in bytes

		s_bind sys_mem, statics, r0
		vp_cpy [r0], r0
		vp_ret

	def_function_end
