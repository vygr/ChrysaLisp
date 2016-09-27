%include 'inc/func.inc'
%include 'class/class_ref.inc'

	def_function class/ref/deref_if
		;inputs
		;r0 = 0, else object
		;trashes
		;all but r4

		if r0, !=, 0
			s_jmp ref, ref, {r0}
		endif
		vp_ret

	def_function_end
