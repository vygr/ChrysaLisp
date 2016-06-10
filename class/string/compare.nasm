%include 'inc/func.inc'
%include 'inc/string.inc'
%include 'class/class_string.inc'

	fn_function class/string/compare
		;inputs
		;r0 = string object
		;r1 = string object
		;outputs
		;r0 = string object
		;r1 = 0 if not same, else same
		;trashes
		;r2-r3

		if r0, !=, r1
			vp_push r0
			s_call sys_string, compare, {&[r0 + string_data], &[r1 + string_data]}, {r1}
			vp_pop r0
		endif
		vp_ret

	fn_function_end
