%include 'inc/func.inc'
%include 'inc/string.inc'
%include 'class/class_string.inc'

	fn_function class/string/hash
		;inputs
		;r0 = string object
		;outputs
		;r0 = string object
		;r1 = hash code
		;trashes
		;all but r0, r4

		;save inputs
		vp_push r0
		vp_add string_data, r0
		s_call sys_string, hash, {r0}, {r1}
		vp_pop r0
		vp_ret

	fn_function_end
