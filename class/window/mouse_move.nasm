%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_window.inc'

	fn_function class/window/mouse_move
		;inputs
		;r0 = window object
		;r1 = mouse event message
		;trashes
		;all but r0, r4

		def_structure local
			long local_inst
			long local_event
			long local_old_x
			long local_old_y
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1
		set_dst [r4 + local_inst], [r4 + local_event]
		map_src_to_dst

		;dirty old area
		s_call window, dirty, {r0}

		;get smallest size
		m_call window, pref_size, {r0}, {r10, r11}
		vp_cpy r10, r12
		vp_cpy r11, r13

		;save old bounds
		s_call window, get_bounds, {r0}, {[r4 + local_old_x], [r4 + local_old_y], r10, r11}

		;get abolute cords of corners
		vp_add r8, r10
		vp_add r9, r11

		;drag edges
		vp_cpy [r4 + local_event], r1
		vp_cpy [r0 + window_drag_mode], r15
		vp_and window_drag_left, r15
		if r15, !=, 0
			;drag left edge
			vp_cpy [r1 + ev_data_x], r8
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
			vp_cpy [r1 + ev_data_x], r10
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
			vp_cpy [r1 + ev_data_y], r9
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
			vp_cpy [r1 + ev_data_y], r11
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
		s_call window, change, {r0, r8, r9, r10, r11}

		;translate old dirty area and dirty all
		vp_cpy [r4 + local_old_x], r8
		vp_cpy [r4 + local_old_y], r9
		vp_sub [r0 + view_x], r8
		vp_sub [r0 + view_y], r9
		vp_lea [r0 + view_dirty_region], r1
		s_call gui_region, translate, {r1, r8, r9}

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		s_jmp window, dirty_all, {r0}

	fn_function_end
