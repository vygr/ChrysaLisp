%include "func.inc"
%include "gui.inc"
%include "sdl2.inc"

	fn_function "sys/gui_deinit_gui"

		;deinit patch heap
		fn_bind sys/gui_statics, r14
		vp_lea [r14 + GUI_STATICS_PATCH_HEAP], r0
		fn_call sys/heap_deinit

		;destroy any window
		vp_cpy [r14 + GUI_STATICS_SCREEN], r13
		if r13, !=, 0
			sdl_destroywindow r13

			;deinit sdl2
			sdl_quit
		endif
		vp_ret

	fn_function_end
