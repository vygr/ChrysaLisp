%include 'inc/func.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_vector.inc'
%include 'class/class_pair.inc'

	fn_function class/unordered_map/insert
		;inputs
		;r0 = unordered_map object
		;r1 = key object
		;r2 = value object
		;outputs
		;r0 = unordered_map object
		;trashes
		;all but r0, r4

		def_structure local
			ptr local_inst
			ptr local_key
			ptr local_value
			ptr local_obj
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1, r2
		set_dst [r4 + local_inst], [r4 + local_key], [r4 + local_value]
		map_src_to_dst

		;search hash bucket
		m_call obj, hash, {r1}, {r0}
		vp_cpy [r4 + local_inst], r1
		vp_cpy [r1 + unordered_set_num_buckets], r1
		vp_xor r2, r2
		vp_div r1, r2, r0
		vp_cpy [r4 + local_inst], r0
		s_call vector, get_element, {[r0 + unordered_set_buckets], r2}, {r1}
		s_call vector, for_each, {r1, $insert_callback, r4}, {r1}
		if r1, ==, 0
			;new key
			vp_cpy r0, [r4 + local_obj]
			s_call ref, ref, {[r4 + local_value]}
			s_call ref, ref, {[r4 + local_key]}
			s_call pair, create, {r0, [r4 + local_value]}, {r0}
			s_call vector, push_back, {[r4 + local_obj], r0}
		else
			;old key
			vp_cpy [r1], r2
			vp_cpy r2, [r4 + local_obj]
			s_call ref, ref, {[r4 + local_value]}
			s_call ref, deref, {[r2 + pair_second]}
			vp_cpy [r4 + local_value], r0
			vp_cpy [r4 + local_obj], r2
			vp_cpy r0, [r2 + pair_second]
		endif
		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	insert_callback:
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
