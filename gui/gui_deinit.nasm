%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/sdl2.inc'
%include 'class/class_view.inc'

	fn_function gui/gui_deinit
		;free any screen
		static_bind gui_gui, statics, r1
		vp_cpy [r1 + gui_statics_screen], r0
		if r0, !=, 0
			vp_cpy_cl 0, [r1 + gui_statics_screen]
			static_call view, deref
		endif

		;free old region
		static_bind gui_gui, statics, r5
		vp_lea [r5 + gui_statics_rect_heap], r0
		vp_lea [r5 + gui_statics_old_region], r1
		static_call gui_region, free

		;deinit region heap
		static_call sys_heap, deinit

		;deinit signal heap
		vp_lea [r5 + gui_statics_sigslot_heap], r0
		static_call sys_heap, deinit

		;destroy any window
		vp_cpy [r5 + gui_statics_window], r14
		if r14, !=, 0
			;align stack on 16 byte boundary
			vp_cpy r4, r15
			vp_and -16, r4

			sdl_destroy_window r14
			ttf_quit
			sdl_quit

			vp_cpy r15, r4
		endif
		vp_ret

	fn_function_end
