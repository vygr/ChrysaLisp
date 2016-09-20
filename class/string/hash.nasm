%include 'inc/func.inc'
%include 'inc/string.inc'
%include 'class/class_string.inc'

	def_function class/string/hash
		;inputs
		;r0 = string object
		;outputs
		;r0 = string object
		;r1 = hash code
		;trashes
		;all but r0, r4

		;save inputs
		vp_push r0
		s_call sys_string, hash, {&[r0 + string_data]}, {r1}
		vp_pop r0
		vp_ret

	def_function_end
