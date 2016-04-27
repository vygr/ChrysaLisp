%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_title.inc'
%include 'class/class_window.inc'

	fn_function class/title/mouse_move
		;inputs
		;r0 = window object
		;r1 = mouse event message
		;trashes
		;all but r0, r4

		def_local
			def_local_long	inst
			def_local_long	window
			def_local_long	event
			def_local_long	old_x
			def_local_long	old_y
		def_local_end

		;save old window bounds
		vp_sub local_size, r4
		vp_cpy r0, .inst
		vp_cpy r1, .event
		vp_cpy [r0 + view_parent], r0
		vp_cpy [r0 + view_parent], r0
		vp_cpy r0, .window
		vp_cpy [r0 + view_x], r8
		vp_cpy [r0 + view_y], r9
		vp_cpy r8, .old_x
		vp_cpy r9, .old_y

		;dirty old area
		static_call window, dirty, {r0}

		;get new window position
		vp_cpy .inst, r0
		static_call title, get_relative, {r0, .window, [r0 + title_last_x], [r0 + title_last_y]}, {r8, r9}
		vp_cpy .event, r1
		vp_sub [r1 + ev_data_x], r8
		vp_sub [r1 + ev_data_y], r9
		vp_mul -1, r8
		vp_mul -1, r9

		;change window position
		vp_cpy .window, r0
		vp_cpy r8, [r0 + view_x]
		vp_cpy r9, [r0 + view_y]

		;translate old dirty area and dirty all
		vp_sub .old_x, r8
		vp_sub .old_y, r9
		vp_mul -1, r8
		vp_mul -1, r9
		vp_add view_dirty_region, r0
		static_call gui_region, translate, {r0, r8, r9}
		static_call window, dirty_all, {.window}

		vp_cpy .inst, r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
