%include 'inc/func.inc'
%include 'inc/string.inc'
%include 'class/class_string.inc'

	def_func class/string/compare
		;inputs
		;r0 = string object
		;r1 = string object
		;outputs
		;r0 = string object
		;r1 = 0 if same, else -, +
		;trashes
		;r2-r3

		if r0, !=, r1
			vp_push r0
			f_call sys_string, compare, {&[r0 + string_data], &[r1 + string_data]}, {r1}
			vp_pop r0
		else
			vp_xor r1, r1
		endif
		vp_ret

	def_func_end
