%include 'inc/func.inc'
%include 'class/class_button.inc'
%include 'class/class_flow.inc'

	fn_function class/button/layout
		;inputs
		;r0 = button object
		;trashes
		;all but r0, r4

		def_local
			def_local_long	inst
		def_local_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, .inst

		vp_cpy button_border_size, r8
		vp_cpy [r0 + view_w], r10
		vp_cpy [r0 + view_h], r11
		vp_sub button_border_size * 2, r10
		vp_sub button_border_size * 2, r11
		vp_cpy [r0 + button_state], r1
		vp_and button_state_pressed, r1
		if r1, !=, 0
			vp_add button_border_size, r8
		endif
		s_call flow, change, {[r0 + label_flow], r8, r8, r10, r11}

		vp_cpy .inst, r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
