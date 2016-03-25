%include 'inc/func.inc'
%include 'inc/font.inc'

	fn_function sys/font_init, no_debug_enter

		;get font statics
		static_bind font, statics, r3

		;init font lists
		vp_lea [r3 + ft_statics_font_list], r0
		lh_init r0, r1
		vp_lea [r3 + ft_statics_word_list], r0
		lh_init r0, r1
		vp_ret

	fn_function_end
