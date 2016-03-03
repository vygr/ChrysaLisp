%include 'inc/func.inc'
%include 'inc/gui.inc'

	fn_function "gui/gui_init"
		;inputs
		;r0 = sdl function table

		;init sdl function table
		fn_bind gui/gui_statics, r3
		vp_cpy r0, [r3 + GUI_STATICS_SDL_FUNCS]

		;init patch heap
		vp_lea [r3 + GUI_STATICS_PATCH_HEAP], r0
		vp_cpy GUI_PATCH_SIZE, r1
		vp_cpy GUI_PATCH_SIZE*32, r2
		fn_call sys/heap_init

		;init view heap
		vp_lea [r3 + GUI_STATICS_VIEW_HEAP], r0
		vp_cpy GUI_VIEW_SIZE, r1
		vp_cpy GUI_VIEW_SIZE*8, r2
		fn_call sys/heap_init

		;init view list
		vp_lea [r3 + GUI_STATICS_VIEW_LIST], r0
		lh_init r0, r1

		vp_ret

	fn_function_end
