%include 'inc/func.ninc'
%include 'inc/font.ninc'
%include 'inc/sdl2.ninc'

def_func gui/font_deinit

	def_struct local
		ptr local_old_stack
	def_struct_end

	;align stack to 16 bytes for SDl
	vp_cpy r4, r15
	vp_sub local_size, r4
	vp_and -16, r4
	vp_cpy r15, [r4 + local_old_stack]

	;get font statics
	f_bind gui_font, statics, r15

	;free all text in the cache
	vp_lea [r15 + ft_statics_text_flists], r10
	vp_lea [r10 + ft_buckets_size], r11
	loop_start
		loop_flist_forward r10, r14, r13
			vp_cpy r14, r12
			ln_remove_fnode r14, r13
			sdl_destroy_texture [r12 + ft_text_texture]
			f_call sys_mem, free, {r12}
		loop_end
		vp_add ptr_size, r10
	loop_until r10, ==, r11

	;free all fonts in the cache
	loop_flist_forward r15 + ft_statics_font_flist, r14, r13
		vp_cpy r14, r12
		ln_remove_fnode r14, r13
		ttf_close_font [r12 + ft_font_handle]
		f_call sys_mem, free, {r12}
	loop_end

	vp_cpy [r4 + local_old_stack], r4
	vp_ret

def_func_end
