%include 'inc/func.inc'
%include 'inc/gui.inc'

	fn_function gui/gui_init, no_debug_enter
		;inputs
		;r0 = sdl function table

		;init sdl function table
		static_bind gui, statics, r3
		vp_cpy r0, [r3 + gui_statics_sdl_funcs]

		;init patch heap
		vp_lea [r3 + gui_statics_patch_heap], r0
		vp_cpy gui_patch_size, r1
		vp_cpy gui_patch_size*32, r2
		static_jmp heap, init
		vp_ret

	fn_function_end
