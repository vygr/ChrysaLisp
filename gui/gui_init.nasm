%include 'inc/func.inc'
%include 'inc/gui.inc'

	fn_function gui/gui_init, no_debug_enter
		;inputs
		;r0 = sdl function table

		;init sdl function table
		static_bind gui_gui, statics, r3
		vp_cpy r0, [r3 + gui_statics_sdl_funcs]

		;init old region
		vp_xor r0, r0
		vp_cpy r0, [r3 + gui_statics_old_region]

		;init last view etc
		vp_cpy r0, [r3 + gui_statics_last_x_pos]
		vp_cpy r0, [r3 + gui_statics_last_y_pos]
		vp_cpy r0, [r3 + gui_statics_last_buttons]
		vp_cpy r0, [r3 + gui_statics_last_view]

		;init region heap
		vp_lea [r3 + gui_statics_rect_heap], r0
		vp_cpy gui_rect_size, r1
		vp_cpy gui_rect_size * 32, r2
		static_call sys_heap, init

		;init signal heap
		vp_lea [r3 + gui_statics_signal_heap], r0
		vp_cpy gui_signal_size, r1
		vp_cpy gui_signal_size * 32, r2
		static_jmp sys_heap, init

	fn_function_end
