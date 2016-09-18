%include 'inc/func.inc'
%include 'class/class_vector.inc'

	fn_function class/vector/clear
		;inputs
		;r0 = vector object
		;outputs
		;r0 = vector object
		;trashes
		;all but r0, r4

		;deref all elements
		s_call vector, for_each, {r0, $clear_callback, 0}, {_}

		;free dynamic array
		vp_push r0
		s_call sys_mem, free, {[r0 + vector_array]}
		vp_pop r0

		;init myself
		vp_xor r1, r1
		vp_cpy r1, [r0 + vector_array]
		vp_cpy r1, [r0 + vector_length]
		vp_cpy r1, [r0 + vector_capacity]
		vp_ret

	clear_callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		s_call ref, deref, {[r0]}
		vp_cpy 1, r1
		vp_ret

	fn_function_end
