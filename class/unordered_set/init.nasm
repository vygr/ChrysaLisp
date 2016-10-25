%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

	def_function class/unordered_set/init
		;inputs
		;r0 = unordered_set object
		;r1 = vtable pointer
		;r2 = key compare callback
		;r3 = num_buckets
		;outputs
		;r1 = 0 if error, else ok
		;trashes
		;r2-r3, r5-r8

		def_structure local
			ptr local_inst
			ptr local_num_buckets
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r2, r3
		set_dst [r4 + local_inst], [r0 + unordered_set_key_callback], [r4 + local_num_buckets]
		map_src_to_dst

		;init parent
		p_call unordered_set, init, {r0, r1}, {r1}
		if r1, !=, 0
			;init myself
			s_call vector, create, {}, {r0}
			vp_cpy [r4 + local_inst], r1
			vp_cpy r0, [r1 + unordered_set_buckets]
			s_call vector, set_capacity, {r0, [r4 + local_num_buckets]}
			vp_cpy [r4 + local_num_buckets], r2
			vp_cpy r2, r1
			vp_shl 3, r2
			vp_cpy r2, [r0 + vector_length]
			s_call vector, for_each, {r0, 0, r1, $callback, 0}, {_}
			vp_cpy [r4 + local_inst], r0
			vp_cpy 1, r1
		endif

		vp_add local_size, r4
		vp_ret

	callback:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		vp_push r1
		s_call vector, create, {}, {r1}
		vp_pop r0
		vp_cpy r1, [r0]
		vp_ret

	def_function_end
