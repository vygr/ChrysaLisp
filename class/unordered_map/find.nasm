%include 'inc/func.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_vector.inc'
%include 'class/class_pair.inc'

	def_func class/unordered_map/find
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
		f_call unordered_map, get_bucket, {r0, r1}, {r0}
		d_call vector, get_length, {r0}, {r1}
		f_call vector, for_each, {r0, 0, r1, $callback, r4}, {r1}
		vp_cpy r0, r2
		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	callback:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		vp_cpy [r1], r1
		vp_cpy [r1 + pair_first], r1
		vp_cpy [r0 + local_inst], r2
		vp_cpy [r0 + local_key], r0
		vp_jmp [r2 + unordered_set_key_callback]

	def_func_end
