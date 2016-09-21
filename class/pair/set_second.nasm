%include 'inc/func.inc'
%include 'class/class_pair.inc'

	def_function class/pair/set_second
		;inputs
		;r0 = pair object
		;r1 = object pointer
		;outputs
		;r0 = pair object
		;trashs
		;all but r0, r4

		def_structure local
			ptr local_inst
			ptr local_obj
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1
		set_dst [r4 + local_inst], [r4 + local_obj]
		map_src_to_dst

		s_call ref, deref, {[r0 + pair_second]}
		vp_cpy [r4 + local_inst], r0
		vp_cpy [r4 + local_obj], r1
		vp_cpy r1, [r0 + pair_second]

		vp_sub local_size, r4
		vp_ret

	def_function_end
