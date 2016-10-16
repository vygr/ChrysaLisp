%include 'inc/func.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_vector.inc'
%include 'class/class_pair.inc'

	def_function class/unordered_map/insert
		;inputs
		;r0 = unordered_map object
		;r1 = key object
		;r2 = value object
		;outputs
		;r0 = unordered_map object
		;r1 = iterator
		;r2 = bucket vector
		;trashes
		;all but r0, r4

		def_structure local
			ptr local_inst
			ptr local_key
			ptr local_value
			ptr local_iter
			ptr local_bucket
			ptr local_pair
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1, r2
		set_dst [r4 + local_inst], [r4 + local_key], [r4 + local_value]
		map_src_to_dst

		;search hash bucket
		s_call unordered_map, get_bucket, {r0, r1}, {r1}
		s_call vector, for_each, {r1, 0, $callback, r4}, {r1}
		vp_cpy r0, [r4 + local_bucket]
		if r1, ==, 0
			;new key
			s_call ref, ref, {[r4 + local_value]}
			s_call ref, ref, {[r4 + local_key]}
			s_call pair, create, {r0, [r4 + local_value]}, {r0}
			s_call vector, push_back, {[r4 + local_bucket], r0}
			vp_cpy r0, r2
			vp_cpy [r0 + vector_length], r1
			vp_cpy [r0 + vector_array], r0
			vp_lea [r0 + r1 - ptr_size], r1
		else
			;old key
			vp_cpy r1, [r4 + local_iter]
			vp_cpy [r1], r2
			vp_cpy r2, [r4 + local_pair]
			s_call ref, ref, {[r4 + local_value]}
			s_call ref, deref, {[r2 + pair_second]}
			vp_cpy [r4 + local_value], r0
			vp_cpy [r4 + local_pair], r2
			vp_cpy r0, [r2 + pair_second]
			vp_cpy [r4 + local_iter], r1
			vp_cpy [r4 + local_bucket], r2
		endif

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	callback:
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

	def_function_end
