%include 'inc/func.inc'
%include 'class/class_ref.inc'

	def_func class/ref/deref_if
		;inputs
		;r0 = 0, else object
		;trashes
		;all but r4

		if r0, !=, 0
			f_jmp ref, ref, {r0}
		endif
		vp_ret

	def_func_end
