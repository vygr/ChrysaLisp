%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

	def_function class/unordered_set/erase
		;inputs
		;r0 = unordered_set object
		;r1 = iterator
		;r2 = bucket vector
		;outputs
		;r0 = unordered_set object
		;trashes
		;all but r0, r4

		def_structure local
			ptr local_inst
			ptr local_iter
			ptr local_bucket
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1, r2
		set_dst [r4 + local_inst], [r4 + local_iter], [r4 + local_bucket]
		map_src_to_dst

		;swap last entry for erased entry
		s_call ref, deref, {[r1]}
		vp_cpy [r4 + local_bucket], r0
		vp_cpy [r0 + vector_length], r1
		vp_sub ptr_size, r1
		vp_cpy r1, [r0 + vector_length]
		vp_add [r0 + vector_array], r1
		vp_cpy [r4 + local_iter], r0
		if r1, !=, r0
			vp_cpy [r1], r1
			vp_cpy r1, [r0]
		endif

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	def_function_end
