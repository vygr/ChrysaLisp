%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

	def_function class/unordered_set/get_element
		;inputs
		;r0 = unordered_set object
		;r1 = element index
		;outputs
		;r0 = unordered_set object
		;r1 = element
		;trashes
		;all but r0, r4

		def_structure local
			ptr local_inst
			ptr local_elem
			ulong local_index
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1
		set_dst [r4 + local_inst], [r4 + local_index]
		map_src_to_dst

		;search hash buckets
		s_call vector, for_each, {[r0 + unordered_set_buckets], 0, $callback, r4}, {r1}

		vp_cpy [r4 + local_inst], r0
		vp_cpy [r4 + local_elem], r1
		vp_add local_size, r4
		vp_ret

	callback:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		vp_cpy r0, r5
		vp_cpy [r1], r0
		vp_cpy [r0 + vector_length], r2
		vp_shr 3, r2
		vp_cpy [r5 + local_index], r3
		if r3, >=, r2
			vp_sub r2, r3
			vp_cpy r3, [r5 + local_index]
		else
			s_call vector, get_element, {r0, r3}, {r1}
			vp_cpy r1, [r5 + local_elem]
			vp_xor r1, r1
		endif
		vp_ret

	def_function_end
