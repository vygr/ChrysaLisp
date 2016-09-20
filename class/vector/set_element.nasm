%include 'inc/func.inc'
%include 'class/class_vector.inc'

	def_function class/vector/set_element
		;inputs
		;r0 = vector object
		;r1 = object
		;r2 = vector element
		;outputs
		;r0 = vector object
		;trashes
		;all but r0, r4

		def_structure local
			ptr local_inst
			ptr local_obj
			ptr local_index
		def_structure_end

		;save inputs
		vp_mul ptr_size, r2
		vp_sub local_size, r4
		set_src r0, r1, r2
		set_dst [r4 + local_inst], [r4 + local_obj], [r4 + local_index]
		map_src_to_dst

		vp_cpy [r0 + vector_array], r0
		s_call ref, deref, {[r0 + r2]}
		vp_cpy [r4 + local_inst], r0
		vp_cpy [r4 + local_obj], r1
		vp_cpy [r4 + local_index], r2
		vp_add [r0 + vector_array], r2
		vp_cpy r1, [r2]

		vp_add local_size, r4
		vp_ret

	def_function_end
