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
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		vp_push r1
		s_call vector, get_length, {[r0]}, {r0}
		vp_pop r1
		vp_add [r1], r0
		vp_cpy r0, [r1]
		vp_ret

	def_function_end
