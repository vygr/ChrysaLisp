%include 'inc/func.inc'
%include 'class/class_vector.inc'

	def_function class/vector/slice
		;inputs
		;r0 = vector object
		;r1 = vector element start
		;r2 = vector element end
		;outputs
		;r0 = vector object
		;r1 = slice vector object
		;trashes
		;r1-r3, r5-r8

		def_structure local
			ptr local_inst
			ptr local_new
			ulong local_start
			ulong local_end
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1, r2
		set_dst [r4 + local_inst], [r4 + local_start], [r4 + local_end]
		map_src_to_dst

		;create new vector
		s_call vector, create, {}, {[r4 + local_new]}
		vp_cpy [r4 + local_end], r1
		vp_sub [r4 + local_start], r1
		s_call vector, set_capacity, {r0, r1}

		;copy elements
		vp_cpy [r4 + local_start], r5
		vp_cpy [r4 + local_end], r6
		loop_while r5, !=, r6
			t_call vector, ref_element, {[r4 + local_inst], r5}, {r1}
			s_call vector, push_back, {[r4 + local_new], r1}
			vp_inc r5
		loop_end

		vp_cpy [r4 + local_inst], r0
		vp_cpy [r4 + local_new], r1
		vp_add local_size, r4
		vp_ret

	def_function_end
