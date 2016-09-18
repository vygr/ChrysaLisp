%include 'inc/func.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_vector.inc'
%include 'class/class_pair.inc'

	fn_function class/unordered_map/find
		;inputs
		;r0 = unordered_map object
		;r1 = key object
		;outputs
		;r0 = unordered_map object
		;r1 = 0, else found iterator
		;r2 = bucket vector
		;trashes
		;all but r0, r4

		def_structure local
			ptr local_inst
			ptr local_key
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1
		set_dst [r4 + local_inst], [r4 + local_key]
		map_src_to_dst

		;search hash bucket
		m_call obj, hash, {r1}, {r0}
		vp_cpy [r4 + local_inst], r1
		vp_cpy [r1 + unordered_set_num_buckets], r1
		vp_xor r2, r2
		vp_div r1, r2, r0
		vp_shl 3, r2
		vp_cpy [r4 + local_inst], r0
		vp_cpy [r0 + unordered_set_buckets], r0
		s_call vector, for_each, {[r0 + r2], $find_callback, r4}, {r1}
		vp_cpy r0, r2
		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	find_callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		vp_cpy [r0], r0
		vp_cpy [r0 + pair_first], r0
		vp_cpy [r1 + local_inst], r2
		vp_cpy [r1 + local_key], r1
		vp_jmp [r2 + unordered_set_key_callback]

	fn_function_end
