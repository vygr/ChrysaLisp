%include 'inc/func.inc'
%include 'inc/font.inc'

	fn_function gui/font_init

		;get font statics
		static_bind gui_font, statics, r3

		;init font lists
		vp_xor r0, r0
		vp_cpy r0, [r3 + ft_statics_font_list]
		vp_cpy r0, [r3 + ft_statics_text_list]
		vp_ret

	fn_function_end
