%include 'inc/func.inc'
%include 'inc/font.inc'

	def_func gui/font_init

		;get font statics
		f_bind gui_font, statics, r1

		;init font lists
		vp_xor r0, r0
		vp_cpy r0, [r1 + ft_statics_font_flist]

		;init text image hash slots
		vp_add ft_statics_text_flists, r1
		vp_lea [r1 + ft_buckets_size], r2
		loop_start
			vp_cpy r0, [r1]
			vp_add ptr_size, r1
		loop_until r1, ==, r2
		vp_ret

	def_func_end
