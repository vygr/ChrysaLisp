%include 'inc/func.ninc'
%include 'class/class_unordered_set.ninc'
%include 'class/class_vector.ninc'

def_func class/unordered_set/slice_impl
	;inputs
	;r0 = unordered_set object
	;r1 = empty set/map object
	;r2 = element callback
	;r3 = start element
	;r5 = end element
	;outputs
	;r0 = unordered_set object
	;r1 = full set/map object
	;trashes
	;all but r0, r4

	def_struct local
		ptr local_obj	;must be first !
		ptr local_inst
		ptr local_callback
		ulong local_start_bucket
		ulong local_start_elem
		ulong local_end_bucket
		ulong local_end_elem
	def_struct_end

	;save inputs
	vp_sub local_size, r4
	set_src r0, r1, r2, r5
	set_dst [r4 + local_inst], [r4 + local_obj], [r4 + local_callback], [r4 + local_end_elem]
	map_src_to_dst

	f_call unordered_set, get_iter, {r0, r3}, {r6, r7}
	f_call unordered_set, get_iter, {r0, [r4 + local_end_elem]}, {r8, r9}

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
		f_call vector, get_element, {[r0 + unordered_set_buckets], r7}, {r0}
		f_call vector, for_each, {r0, [r4 + local_start_elem], [r4 + local_end_elem], [r4 + local_callback], r4}, {_}
	else
		f_call vector, get_element, {[r0 + unordered_set_buckets], r7}, {r0}
		f_call vector, for_each, {r0, [r4 + local_start_elem], [r0 + vector_length], [r4 + local_callback], r4}, {_}

		vp_cpy [r4 + local_start_bucket], r1
		vp_inc r1
		if r1, !=, [r4 + local_end_bucket]
			vp_cpy [r4 + local_inst], r0
			f_call vector, for_each, {[r0 + unordered_set_buckets], [r4 + local_start_bucket], [r4 + local_end_bucket], $callback, r4}, {_}
		endif

		vp_cpy [r4 +local_inst], r0
		f_call vector, get_element, {[r0 + unordered_set_buckets], [r4 + local_end_bucket]}, {r0}
		f_call vector, for_each, {r0, 0, [r4 + local_end_elem], [r4 + local_callback], r4}, {_}
	endif

	vp_cpy [r4 + local_obj], r1
	vp_cpy [r4 + local_inst], r0
	vp_add local_size, r4
	vp_ret

callback:
	;inputs
	;r0 = predicate data pointer
	;r1 = element iterator
	;outputs
	;r1 = 0 if break, else not

	vp_cpy r0, r2
	vp_cpy [r1], r0
	f_call vector, for_each, {r0, 0, [r0 + vector_length], [r2 + local_callback], r2}, {_}
	vp_cpy 1, r1
	vp_ret

def_func_end
