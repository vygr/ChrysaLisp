%include 'inc/func.inc'
%include 'inc/string.inc'
%include 'class/class_string.inc'

	def_func class/string/hash
		;inputs
		;r0 = string object
		;outputs
		;r0 = string object
		;r1 = hash code
		;trashes
		;all but r0, r4

		;save inputs
		vp_cpy [r0 + string_hashcode], r1
		if r1, ==, 0
			vp_push r0
			f_call sys_string, hash, {&[r0 + string_data]}, {r1}
			vp_pop r0
			vp_cpy r1, [r0 + string_hashcode]
		endif
		vp_ret

	def_func_end
