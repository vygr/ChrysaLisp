%include 'inc/func.inc'
%include 'class/class_string.inc'

	def_function class/string/ref_element
		;inputs
		;r0 = string object
		;r1 = char index
		;outputs
		;r0 = string object
		;r1 = char string

		vp_push r0
		s_call string, create_from_buffer, {&[r0 + r1 + string_data], 1}, {r1}
		vp_pop r0
		vp_ret

	def_function_end
