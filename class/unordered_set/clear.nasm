%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

	def_function class/unordered_set/clear
		;inputs
		;r0 = unordered_set object
		;outputs
		;r0 = unordered_set object
		;trashes
		;all but r0, r4

		;clear all buckets
		vp_push r0
		s_call vector, for_each, {[r0 + unordered_set_buckets], 0, $callback, 0}, {_}
		vp_pop r0
		vp_ret

	callback:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		s_call vector, clear, {[r1]}
		vp_cpy 1, r1
		vp_ret

	def_function_end
