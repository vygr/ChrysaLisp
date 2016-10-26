%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

	def_func class/unordered_set/deinit
		;inputs
		;r0 = unordered_set object
		;trashes
		;all but r0, r4

		vp_push r0
		f_call vector, deref, {[r0 + unordered_set_buckets]}
		vp_pop r0
		s_jmp unordered_set, deinit, {r0}

	def_func_end
