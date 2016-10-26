%include 'inc/func.inc'
%include 'class/class_vector.inc'

	def_func class/vector/clear
		;inputs
		;r0 = vector object
		;outputs
		;r0 = vector object
		;trashes
		;all but r0, r4

		;deref all elements
		d_call vector, get_length, {r0}, {r1}
		f_call vector, for_each, {r0, 0, r1, $callback, 0}, {_}

		;free dynamic array
		vp_push r0
		f_call sys_mem, free, {[r0 + vector_array]}
		vp_pop r0

		;init myself
		vp_xor r1, r1
		vp_cpy r1, [r0 + vector_array]
		vp_cpy r1, [r0 + vector_length]
		vp_cpy r1, [r0 + vector_capacity]
		vp_ret

	callback:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		f_call ref, deref, {[r1]}
		vp_cpy 1, r1
		vp_ret

	def_func_end
