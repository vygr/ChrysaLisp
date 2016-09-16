%include 'inc/func.inc'
%include 'class/class_vector.inc'

	fn_function class/vector/clear
		;inputs
		;r0 = vector object
		;outputs
		;r0 = vector object
		;trashes
		;all but r0, r4

		def_structure local
			ptr local_inst
			ptr local_next
			ptr local_end
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0
		set_dst [r4 + local_inst]
		map_src_to_dst

		;deref elements
		vp_cpy [r0 + vector_length], r1
		vp_cpy [r0 + vector_array], r0
		vp_add r0, r1
		vp_cpy r1, [r4 + local_end]
		loop_while r0, !=, [r4 + local_end]
			vp_cpy r0, [r4 + local_next]
			s_call ref, deref, {[r0]}
			vp_cpy [r4 + local_next], r0
			vp_add ptr_size, r0
		loop_end

		;free dynamic array
		vp_cpy [r4 +local_inst], r0
		s_call sys_mem, free, {[r0 + vector_array]}

		;init myself
		vp_xor r1, r1
		vp_cpy [r4 +local_inst], r0
		vp_cpy r1, [r0 + vector_array]
		vp_cpy r1, [r0 + vector_length]
		vp_cpy r1, [r0 + vector_capacity]
		vp_add local_size, r4
		vp_ret

	fn_function_end
