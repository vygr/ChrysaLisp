%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_vector.inc'

	def_func class/unordered_set/copy
		;inputs
		;r0 = unordered_set object
		;r1 = num buckets
		;outputs
		;r0 = unordered_set object
		;r1 = unordered_set copy
		;trashes
		;all but r0, r4

		vp_cpy r0, r9
		f_call unordered_set, create, {[r0 + unordered_set_key_callback], r1}, {r1}
		f_jmp unordered_set, copy_impl, {r9, r1}

	def_func_end
