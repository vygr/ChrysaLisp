%include 'inc/func.inc'
%include 'class/class_string.inc'

	def_func class/string/slice
		;inputs
		;r0 = string object
		;r1 = start index
		;r2 = end index
		;outputs
		;r0 = string object
		;r1 = string slice
		;trashes
		;r2-r3, r5-r7

		vp_push r0
		vp_sub r1, r2
		vp_add string_data, r0
		vp_add r1, r0
		f_call string, create_from_buffer, {r0, r2}, {r1}
		vp_pop r0
		vp_ret

	def_func_end
