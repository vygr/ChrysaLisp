%include 'inc/func.inc'
%include 'class/class_button.inc'
%include 'class/class_flow.inc'

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
			vp_lea [r0 + button_pressed_signal], r1
			lh_init r1, r2

			vp_cpy [r0 + label_flow], r0
			vp_cpy flow_flag_down | flow_flag_align_hcenter, r1
			static_call flow, set_flow_flags
		endif
		vp_pop r0
		vp_ret

	fn_function_end
