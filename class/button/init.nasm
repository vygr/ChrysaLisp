%include 'inc/func.inc'
%include 'class/class_button.inc'

	fn_function class/button/init
		;inputs
		;r0 = button object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;save object
		vp_push r0

		;init parent
		super_call button, init
		if r1, !=, 0
			;init myself
			vp_cpy_cl 0, [r0 + button_state]
		endif
		vp_pop r0
		vp_ret

	fn_function_end
