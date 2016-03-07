%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/sdl2.inc'
%include 'class/class_view.inc'

	fn_function gui/gui_deinit
		;free any screen
		static_bind gui, statics, r1
		vp_cpy [r1 + gui_statics_screen], r0
		if r0, !=, 0
			vp_cpy 0, qword[r1 + gui_statics_screen]
			static_call view, deref
		endif

		;deinit region heap
		static_bind gui, statics, r15
		vp_lea [r15 + gui_statics_rect_heap], r0
		static_call heap, deinit

		;destroy any window
		vp_cpy [r15 + gui_statics_window], r14
		if r14, !=, 0
			;align stack on 16 byte boundary
			vp_cpy r4, r15
			vp_and -16, r4

			sdl_destroywindow r14
			sdl_quit

			vp_cpy r15, r4
		endif
		vp_ret

	fn_function_end
