%include 'inc/func.inc'
%include 'inc/font.inc'

	fn_function gui/font_init

		;get font statics
		static_bind gui_font, statics, r1

		;init font lists
		vp_xor r0, r0
		vp_cpy r0, [r1 + ft_statics_font_list]

		vp_add ft_statics_text_lists, r1
		vp_lea [r1 + ft_buckets_size], r2
		loop_start
			vp_cpy r0, [r1]
			vp_add ptr_size, r1
		loop_until r1, ==, r2
		vp_ret

	fn_function_end
