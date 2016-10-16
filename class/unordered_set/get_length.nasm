%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

	def_function class/unordered_set/get_length
		;inputs
		;r0 = unordered_set object
		;outputs
		;r0 = unordered_set object
		;r1 = length
		;trashes
		;all but r0, r4

		;count all buckets
		vp_push r0, 0
		s_call vector, for_each, {[r0 + unordered_set_buckets], 0, $callback, r4}, {_}
		vp_pop r0, r1
		vp_ret

	callback:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		vp_cpy [r1], r1
		vp_cpy [r1 + vector_length], r2
		vp_shr 3, r2
		vp_add [r0], r2
		vp_cpy r2, [r0]
		vp_ret

	def_function_end
