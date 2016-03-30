%include 'inc/func.inc'
%include 'inc/font.inc'
%include 'inc/sdl2.inc'

	fn_function gui/font_deinit, no_debug_enter
		;get font statics
		static_bind gui_font, statics, r15

		;free all text in the cache
		loop_list_forward r15 + ft_statics_text_list, r14, r13
			sdl_destroy_texture [r14 + ft_text_texture]
			vp_cpy r14, r0
			static_call sys_mem, free
		loop_end

		;free all fonts in the cache
		loop_list_forward r15 + ft_statics_font_list, r14, r13
			ttf_close_font [r14 + ft_font_handle]
			vp_cpy r14, r0
			static_call sys_mem, free
		loop_end
		vp_ret

	fn_function_end
