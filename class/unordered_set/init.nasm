%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

	fn_function class/unordered_set/init
		;inputs
		;r0 = unordered_set object
		;r1 = vtable pointer
		;r2 = key compare callback
		;r3 = num_buckets
		;outputs
		;r1 = 0 if error, else ok
		;trashes
		;all but r0, r4

		;save inputs
		vp_cpy r2, [r0 + unordered_set_key_callback]
		vp_cpy r3, [r0 + unordered_set_num_buckets]

		;init parent
		p_call unordered_set, init, {r0, r1}, {r1}
		if r1, !=, 0
			;init myself
			vp_push r0
			s_call vector, create, {}, {r0}
			vp_cpy [r4], r1
			vp_cpy r0, [r1 + unordered_set_buckets]
			s_call vector, set_capacity, {r0, [r1 + unordered_set_num_buckets]}
			vp_cpy [r0 + vector_capacity], r1
			vp_cpy r1, [r0 + vector_length]
			s_call vector, for_each, {r0, $create_bucket, 0}, {_}
			vp_pop r0
		endif
		vp_ret

	create_bucket:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		vp_push r0
		s_call vector, create, {}, {r1}
		vp_pop r0
		vp_cpy r1, [r0]
		vp_ret

	fn_function_end
