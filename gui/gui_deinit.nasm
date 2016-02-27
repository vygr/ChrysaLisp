%include "func.inc"
%include "gui.inc"
%include "sdl2.inc"

	fn_function "gui/gui_deinit"

		;deinit patch heap
		fn_bind gui/gui_statics, r15
		vp_lea [r15 + GUI_STATICS_PATCH_HEAP], r0
		fn_call sys/heap_deinit

		;deinit view heap
		vp_lea [r15 + GUI_STATICS_VIEW_HEAP], r0
		fn_call sys/heap_deinit

		;destroy any window
		vp_cpy [r15 + GUI_STATICS_WINDOW], r14
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
