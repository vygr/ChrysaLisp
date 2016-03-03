%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'inc/sdl2.inc'

	fn_function gui/gui_deinit
		;free any screen
		fn_bind gui/gui_statics, r15
		vp_cpy [r15 + gui_statics_screen], r0
		if r0, !=, 0
			vp_cpy 0, qword[r15 + gui_statics_screen]
			fn_call gui/view_free
		endif

		;deinit patch heap
		vp_lea [r15 + gui_statics_patch_heap], r0
		fn_call sys/heap_deinit

		;deinit view heap
		vp_lea [r15 + gui_statics_view_heap], r0
		fn_call sys/heap_deinit

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
