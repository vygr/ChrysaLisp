%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

	fn_function class/unordered_set/clear
		;inputs
		;r0 = unordered_set object
		;outputs
		;r0 = unordered_set object
		;trashes
		;all but r0, r4

		;clear all buckets
		vp_push r0
		s_call vector, for_each, {[r0 + unordered_set_buckets], $clear_callback, 0}, {_}
		vp_pop r0
		vp_ret

	clear_callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		s_call vector, clear, {[r0]}
		vp_cpy 1, r1
		vp_ret

	fn_function_end
