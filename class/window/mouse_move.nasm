%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_window.inc'

	fn_function class/window/mouse_move
		;inputs
		;r0 = window object
		;r1 = mouse event message
		;trashes
		;all but r4

		def_structure	move
			def_long	move_window
			def_long	move_event
			def_long	move_old_x
			def_long	move_old_y
			def_long	move_old_w
			def_long	move_old_h
		def_structure_end

		;save inputs
		vp_sub move_size, r4
		vp_cpy r0, [r4 + move_window]
		vp_cpy r1, [r4 + move_event]

		;get smallest size
		method_call window, pref_size
		vp_cpy r10, r12
		vp_cpy r11, r13

		;save old values
		vp_cpy [r4 + move_window], r0
		vp_cpy [r0 + view_x], r8
		vp_cpy [r0 + view_y], r9
		vp_cpy [r0 + view_w], r10
		vp_cpy [r0 + view_h], r11
		vp_cpy r8, [r4 + move_old_x]
		vp_cpy r9, [r4 + move_old_y]
		vp_cpy r10, [r4 + move_old_w]
		vp_cpy r11, [r4 + move_old_h]

		;get abolute cords of corners
		vp_add r8, r10
		vp_add r9, r11

		;drag edges
		vp_cpy [r4 + move_event], r1
		vp_cpy [r0 + window_drag_mode], r15
		vp_and window_drag_left, r15
		if r15, !=, 0
			;drag left edge
			vp_cpy [r1 + (ml_msg_data + ev_data_x)], r8
			vp_sub [r0 + window_offset_x], r8
			vp_cpy r10, r15
			vp_sub r8, r15
			if r15, <, r12
				vp_cpy r10, r8
				vp_sub r12, r8
			endif
		endif
		vp_cpy [r0 + window_drag_mode], r15
		vp_and window_drag_right, r15
		if r15, !=, 0
			;drag right edge
			vp_cpy [r1 + (ml_msg_data + ev_data_x)], r10
			vp_sub [r0 + window_offset_x], r10
			vp_cpy r10, r15
			vp_sub r8, r15
			if r15, <, r12
				vp_cpy r8, r10
				vp_add r12, r10
			endif
		endif
		vp_cpy [r0 + window_drag_mode], r15
		vp_and window_drag_top, r15
		if r15, !=, 0
			;drag top edge
			vp_cpy [r1 + (ml_msg_data + ev_data_y)], r9
			vp_sub [r0 + window_offset_y], r9
			vp_cpy r11, r15
			vp_sub r9, r15
			if r15, <, r13
				vp_cpy r11, r9
				vp_sub r13, r9
			endif
		endif
		vp_cpy [r0 + window_drag_mode], r15
		vp_and window_drag_bottom, r15
		if r15, !=, 0
			;drag bottom edge
			vp_cpy [r1 + (ml_msg_data + ev_data_y)], r11
			vp_sub [r0 + window_offset_y], r11
			vp_cpy r11, r15
			vp_sub r9, r15
			if r15, <, r13
				vp_cpy r9, r11
				vp_add r13, r11
			endif
		endif
		vp_sub r8, r10
		vp_sub r9, r11

		;change window size
		static_call window, change
		vp_cpy [r4 + move_window], r0
		static_call window, dirty_all

		;add dirty area to cover old region of window
		vp_cpy [r4 + move_window], r0
		vp_cpy [r4 + move_old_x], r8
		vp_cpy [r4 + move_old_y], r9
		vp_cpy [r4 + move_old_w], r10
		vp_cpy [r4 + move_old_h], r11
		vp_sub [r0 + view_x], r8
		vp_sub [r0 + view_y], r9
		static_call window, add_dirty

		vp_add move_size, r4
		vp_ret

	fn_function_end
