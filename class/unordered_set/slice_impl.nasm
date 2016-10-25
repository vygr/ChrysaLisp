%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

	def_function class/unordered_set/slice_impl
		;inputs
		;r0 = unordered_set object
		;r1 = empty set/map object
		;r2 = start element
		;r3 = end element
		;outputs
		;r0 = unordered_set object
		;r2 = full set/map object
		;trashes
		;all but r0, r4

		def_structure local
			ptr local_inst
			ptr local_obj
			ulong local_start_bucket
			ulong local_start_elem
			ulong local_end_bucket
			ulong local_end_elem
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1, r2, r3
		set_dst [r4 + local_inst], [r4 + local_obj], [r4 + local_start_elem], [r4 + local_end_elem]
		map_src_to_dst

		s_call unordered_set, get_iter, {[r4 + local_inst], [r4 + local_start_elem]}, {r6, r7}
		s_call unordered_set, get_iter, {r0, [r4 + local_end_elem]}, {r8, r9}

		vp_cpy [r7], r2
		vp_cpy [r9], r3
		vp_cpy [r2 + vector_array], r2
		vp_cpy [r3 + vector_array], r3
		vp_sub r2, r6
		vp_sub r3, r8
		vp_shr 3, r6
		vp_shr 3, r8
		vp_cpy r6, [r4 + local_start_elem]
		vp_cpy r8, [r4 + local_end_elem]

		vp_cpy [r0 + unordered_set_buckets], r2
		vp_cpy [r2 + vector_array], r2
		vp_sub r2, r7
		vp_sub r2, r9
		vp_shr 3, r7
		vp_shr 3, r9
		vp_cpy r7, [r4 + local_start_bucket]
		vp_cpy r9, [r4 + local_end_bucket]

		if r7, ==, r9
			s_call vector, get_element, {[r0 + unordered_set_buckets], r7}, {r0}
			s_call vector, for_each, {r0, [r4 + local_start_elem], [r4 + local_end_elem], $callback, r4}, {_}
		else
			s_call vector, get_element, {[r0 + unordered_set_buckets], r7}, {r0}
			t_call vector, get_length, {r0}, {r1}
			s_call vector, for_each, {r0, [r4 + local_start_elem], r1, $callback, r4}, {_}

			vp_cpy [r4 + local_start_bucket], r1
			vp_inc r1
			if r1, !=, [r4 + local_end_bucket]
				s_call vector, for_each, {[r0 + unordered_set_buckets], [r4 + local_start_bucket], [r4 + local_end_bucket], $callback1, r4}, {_}
			endif

			s_call vector, get_element, {[r0 + unordered_set_buckets], [r4 + local_end_bucket]}, {r0}
			s_call vector, for_each, {r0, 0, [r4 + local_end_elem], $callback, r4}, {_}
		endif

		vp_cpy [r4 + local_obj], r1
		vp_add local_size, r4
		vp_ret

	callback:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		vp_cpy r0, r2
		s_call ref, ref, {[r1]}
		vp_push r0
		s_call unordered_set, get_bucket, {[r2 + local_obj], r0}, {r0}
		vp_pop r1
		s_call vector, push_back, {r0, r1}
		vp_cpy 1, r1
		vp_ret

	callback1:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		vp_push r0
		t_call vector, get_length, {[r1]}, {r1}
		vp_pop r2
		s_call vector, for_each, {r0, 0, r1, $callback, r2}, {r1}
		vp_cpy 1, r1
		vp_ret

	def_function_end
