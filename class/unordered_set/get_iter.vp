%include 'inc/func.ninc'
%include 'class/class_unordered_set.ninc'
%include 'class/class_vector.ninc'

def_func class/unordered_set/get_iter
	;inputs
	;r0 = unordered_set object
	;r1 = element index
	;outputs
	;r0 = unordered_set object
	;r1 = element iterator
	;r2 = bucket iterator
	;trashes
	;r3, r5

	def_struct local
		ptr local_inst
		ptr local_iter
		ptr local_bucket
		ulong local_index
	def_struct_end

	;save inputs
	vp_sub local_size, r4
	set_src r0, r1
	set_dst [r4 + local_inst], [r4 + local_index]
	map_src_to_dst

	;search hash buckets
	vp_cpy [r0 + unordered_set_buckets], r0
	f_call vector, for_each, {r0, 0, [r0 + vector_length], $callback, r4}, {_}

	vp_cpy [r4 + local_inst], r0
	vp_cpy [r4 + local_iter], r1
	vp_cpy [r4 + local_bucket], r2
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
	vp_cpy [r5 + local_index], r3
	if r3, >, r2
		vp_sub r2, r3
		vp_cpy r3, [r5 + local_index]
	else
		vp_cpy r1, [r5 + local_bucket]
		vp_cpy [r0 + vector_array], r1
		vp_mul ptr_size, r3
		vp_add r3, r1
		vp_cpy r1, [r5 + local_iter]
		vp_xor r1, r1
	endif
	vp_ret

def_func_end
