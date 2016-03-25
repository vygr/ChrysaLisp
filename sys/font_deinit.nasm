%include 'inc/func.inc'
%include 'inc/font.inc'
%include 'inc/sdl2.inc'

	fn_function sys/font_deinit, no_debug_enter
		;get font statics
		static_bind font, statics, r15

		;free all words in the cache
		loop_list_forward r15 + ft_statics_word_list, r13, r14
			sdl_destroy_texture [r14 + ft_word_texture]
			vp_cpy r14, r0
			static_call mem, free
		loop_end

		;free all fonts in the cache
		loop_list_forward r15 + ft_statics_font_list, r13, r14
			ttf_close_font [r14 + ft_font_handle]
			vp_cpy r14, r0
			static_call mem, free
		loop_end
		vp_ret

	fn_function_end
