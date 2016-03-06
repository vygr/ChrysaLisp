%include 'inc/func.inc'
%include 'inc/gui.inc'

	fn_function gui/gui_init, no_debug_enter
		;inputs
		;r0 = sdl function table

		;init sdl function table
		class_bind gui, statics, r3
		vp_cpy r0, [r3 + gui_statics_sdl_funcs]

		;init patch heap
		vp_lea [r3 + gui_statics_patch_heap], r0
		vp_cpy GUI_PATCH_SIZE, r1
		vp_cpy GUI_PATCH_SIZE*32, r2
		class_call heap, init

		;init view heap
		vp_lea [r3 + gui_statics_view_heap], r0
		vp_cpy GUI_VIEW_SIZE, r1
		vp_cpy GUI_VIEW_SIZE*8, r2
		class_call heap, init

		;init view list
		vp_lea [r3 + gui_statics_view_list], r0
		lh_init r0, r1

		vp_ret

	fn_function_end
