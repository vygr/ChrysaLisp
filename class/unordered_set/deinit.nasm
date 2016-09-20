%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

	def_function class/unordered_set/deinit
		;inputs
		;r0 = unordered_set object
		;trashes
		;all but r0, r4

		vp_push r0
		s_call vector, deref, {[r0 + unordered_set_buckets]}
		vp_pop r0
		p_jmp unordered_set, deinit, {r0}

	def_function_end
