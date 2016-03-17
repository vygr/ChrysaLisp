%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_title.inc'
%include 'class/class_window.inc'

	fn_function class/title/mouse_move
		;inputs
		;r0 = window object
		;r1 = mouse event message
		;trashes
		;all but r4

		def_structure	move
			def_long	move_window
			def_long	move_title
			def_long	move_event
			def_long	move_old_x
			def_long	move_old_y
			def_long	move_old_w
			def_long	move_old_h
		def_structure_end

		;save old bounds
		vp_sub move_size, r4
		vp_cpy r0, [r4 + move_title]
		vp_cpy r1, [r4 + move_event]
		vp_cpy [r0 + view_parent], r0
		vp_cpy [r0 + view_parent], r0
		vp_cpy r0, [r4 + move_window]
		static_call window, get_bounds
		vp_cpy r8, [r4 + move_old_x]
		vp_cpy r9, [r4 + move_old_y]
		vp_cpy r10, [r4 + move_old_w]
		vp_cpy r11, [r4 + move_old_h]

		;get new window position
		vp_cpy r0, r1
		vp_cpy [r4 + move_title], r0
		vp_cpy [r0 + title_last_x], r8
		vp_cpy [r0 + title_last_y], r9
		static_call title, relative
		vp_cpy [r4 + move_event], r1
		vp_cpy [r1 + (ml_msg_data + ev_data_x)], r12
		vp_cpy [r1 + (ml_msg_data + ev_data_y)], r13
		vp_sub r8, r12
		vp_sub r9, r13
		vp_cpy r12, r8
		vp_cpy r13, r9

		;change window position
		vp_cpy [r4 + move_window], r0
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
		vp_add move_size, r4
		static_jmp window, add_dirty

	fn_function_end
