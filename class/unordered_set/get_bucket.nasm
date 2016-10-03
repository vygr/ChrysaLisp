%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

	def_function class/unordered_set/get_bucket
		;inputs
		;r0 = unordered_set object
		;r1 = key object
		;outputs
		;r0 = unordered_set object
		;r1 = bucket vector
		;trashes
		;all but r0, r4

		def_structure local
			ptr local_inst
			ptr local_key
			ptr local_length
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1
		set_dst [r4 + local_inst], [r4 + local_key]
		map_src_to_dst

		;search hash bucket
		vp_cpy [r0 + unordered_set_buckets], r2
		vp_cpy [r2 + vector_length], r2
		vp_shr 3, r2
		if r2, !=, 1
			vp_cpy r2, [r4 + local_length]
			m_call obj, hash, {r1}, {r0}
			vp_cpy [r4 + local_length], r1
			vp_xor r2, r2
			vp_div_u r1, r2, r0
			vp_cpy [r4 + local_inst], r0
		else
			vp_xor r2, r2
		endif
		s_call vector, get_element, {[r0 + unordered_set_buckets], r2}, {r1}

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	def_function_end
