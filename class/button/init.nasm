%include 'inc/func.inc'
%include 'class/class_button.inc'
%include 'class/class_flow.inc'

	fn_function class/button/init
		;inputs
		;r0 = button object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;init parent
		super_call button, init, {r0, r1}, {r1}
		if r1, !=, 0
			;init myself
			vp_cpy_cl 0, [r0 + button_state]
			vp_lea [r0 + button_pressed_signal], r1
			lh_init r1, r2
			static_call button, set_flow_flags, {r0, flow_flag_down | flow_flag_align_hcenter}
		endif
		vp_ret

	fn_function_end
