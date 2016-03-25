%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_button.inc'
%include 'class/class_button.inc'

	fn_function class/button/mouse_move
		;inputs
		;r0 = button object
		;r1 = mouse event message
		;trashes
		;all but r4

		def_structure	local
			def_long	local_button
		def_structure_end

		;save info
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_button]

		;hit ?
		vp_cpy [r1 + (ml_msg_data + ev_data_rx)], r8
		vp_cpy [r1 + (ml_msg_data + ev_data_ry)], r9
		method_call button, hit
		vp_cpy r0, r8

		;is mouse over button ?
		vp_cpy [r4 + local_button], r0
		vp_cpy [r0 + button_state], r1
		vp_cpy r1, r2
		if r8, !=, 0
			;state pressed
			vp_or button_state_pressed, r1
		else
			;state not pressed
			vp_and ~button_state_pressed, r1
		endif
		vp_cpy r1, [r0 + button_state]

		;if state changed then dirty
		if r1, !=, r2
			static_call button, dirty
		endif

		vp_add local_size, r4
		vp_ret

	fn_function_end
