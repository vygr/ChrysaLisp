%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

def_func class/unordered_set/for_each
	;inputs
	;r0 = unordered_set object
	;r1 = predicate function pointer
	;r2 = predicate data pointer
	;outputs
	;r0 = unordered_set object
	;r1 = 0, else break iterator
	;r2 = 0, else bucket vector
	;trashes
	;all but r0, r4
		;callback predicate
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not
		;trashes
		;all but r0, r4

	def_structure local
		ptr local_inst
		ptr local_predicate
		ptr local_predicate_data
		ptr local_iter
	def_structure_end

	;save inputs
	vp_sub local_size, r4
	set_src r0, r1, r2
	set_dst [r4 + local_inst], [r4 + local_predicate], [r4 + local_predicate_data]
	map_src_to_dst

	;for all buckets
	vp_cpy [r0 + unordered_set_buckets], r0
	f_call vector, for_each, {r0, 0, [r0 + vector_length], $callback, r4}, {r2}
	if r2, !=, 0
		vp_cpy [r2], r2
	endif
	vp_cpy [r4 + local_iter], r1
	vp_cpy [r4 + local_inst], r0
	vp_add local_size, r4
	vp_ret

callback:
	;inputs
	;r0 = predicate data pointer
	;r1 = element iterator
	;outputs
	;r1 = 0 if break, else not

	vp_push r0
	vp_cpy r0, r2
	vp_cpy [r1], r0
	f_call vector, for_each, {r0, 0, [r0 + vector_length], [r2 + local_predicate], [r2 + local_predicate_data]}, {r1}
	vp_pop r0
	vp_cpy r1, [r0 + local_iter]
	if r1, ==, 0
		vp_cpy 1, r1
	else
		vp_xor r1, r1
	endif
	vp_ret

def_func_end
