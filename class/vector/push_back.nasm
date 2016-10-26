%include 'inc/func.inc'
%include 'class/class_vector.inc'

	def_func class/vector/push_back
		;inputs
		;r0 = vector object
		;r1 = object pointer
		;outputs
		;r0 = vector object
		;trashes
		;r1-r3, r5-r8

		def_structure local
			ptr local_object
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r1
		set_dst [r4 + local_object]
		map_src_to_dst

		;increase capacity ?
		vp_cpy [r0 + vector_length], r1
		vp_add ptr_size, r1
		vp_cpy r1, [r0 + vector_length]
		if r1, >, [r0 + vector_capacity]
			;double the capacity
			vp_add r1, r1
			vp_shr 3, r1
			f_call vector, set_capacity, {r0, r1}
		endif

		;save object
		vp_cpy [r4 + local_object], r1
		vp_cpy [r0 + vector_length], r2
		vp_cpy [r0 + vector_array], r3
		vp_cpy r1, [r3 + r2 - long_size]

		vp_add local_size, r4
		vp_ret

	def_func_end
