%include 'inc/func.inc'
%include 'inc/string.inc'
%include 'class/class_string.inc'

	def_function class/string/create_from_cstr
		;inputs
		;r0 = c string pointer
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3, r5-r7

		;get size of string
		s_call sys_string, length, {r0}, {r1}
		s_jmp string, create_from_buffer, {r0, r1}, {r0}

	def_function_end
