%include 'inc/func.inc'
%include 'class/class_string.inc'

	fn_function class/string/init
		;inputs
		;r0 = string object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;init parent
		super_call string, init
		if r1, !=, 0
			vp_push r0

			;init myself

			vp_pop r0
		endif
		vp_ret

	fn_function_end
