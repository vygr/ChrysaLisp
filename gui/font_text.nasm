%include 'inc/func.inc'
%include 'inc/font.inc'
%include 'inc/string.inc'
%include 'inc/sdl2.inc'
%include 'class/class_string.inc'

def_func gui/font_text
	;inputs
	;r0 = font entry
	;r1 = string object
	;outputs
	;r0 = 0 if error, else text entry
	;trashes
	;all but r4

	def_struc local
		ptr local_font
		ptr local_text
		ptr local_bucket
		ulong local_handle
		long local_surface
		long local_width
		long local_height
	def_struc_end

	;save inputs
	vp_lea [r1 + string_data], r2
	vp_sub local_size, r4
	set_src r0, r2
	set_dst [r4 + local_font], [r4 + local_text]
	map_src_to_dst

	;get font statics
	f_bind gui_font, statics, r5

	;string hash to bucket
	v_call string, hash, {r1}, {r0}
	vp_cpy ft_num_buckets, r1
	vp_xor r2, r2
	vp_div r1, r2, r0
	vp_lea [r5 + r2 * ptr_size], r5
	vp_cpy r5, [r4 + local_bucket]

	;search bucket
	loop_flist_forward r5, r5, r6
		vp_cpy [r4 + local_font], r0
		continueif r0, !=, [r5 + ft_text_font]
		f_call sys_string, compare, {&[r5 + ft_text_name], [r4 + local_text]}, {r0}
	loop_until r0, ==, 0

	;did we find it ?
	vp_cpy r5, r0
	if r5, ==, 0
		;no so try create it
		f_call sys_task, callback, {$kernel_callback, r4}
		vp_cpy [r4 + local_handle], r0
	else
		;yes so LRU to front
		ln_remove_fnode r5, r6
		vp_cpy [r4 + local_bucket], r5
		ln_add_fnode r5, r0, r1
	endif

	vp_add local_size, r4
	vp_ret

kernel_callback:
	;inputs
	;r0 = user data
	;trashes
	;all but r4

	;align stack
	vp_cpy r4, r15
	vp_and -16, r4

	;save input
	vp_cpy r0, r14

	vp_cpy [r14 + local_font], r0
	ttf_render_utf8_blended [r0 + ft_font_handle], [r14 + local_text], 0xffffff
	if r0, !=, 0
		vp_cpy r0, [r14 + local_surface]
		vp_cpy_ui [r0 + sdl_surface_w], r10
		vp_cpy_ui [r0 + sdl_surface_h], r11
		vp_cpy r10, [r14 + local_width]
		vp_cpy r11, [r14 + local_height]

		;create texture
		f_bind gui_gui, statics, r0
		sdl_create_texture_from_surface [r0 + gui_statics_renderer], [r14 + local_surface]
		if r0, !=, 0
			vp_cpy r0, r5

			f_call sys_string, length, {[r14 + local_text]}, {r1}
			f_call sys_mem, alloc, {&[r1 + ft_text_size + 1]}, {r13, _}
			assert r0, !=, 0

			vp_cpy [r14 + local_font], r0
			vp_cpy r0, [r13 + ft_text_font]
			vp_cpy r5, [r13 + ft_text_texture]
			f_call sys_string, copy, {[r14 + local_text], &[r13 + ft_text_name]}, {_, _}

			;fill in width and height
			vp_cpy [r14 + local_width], r10
			vp_cpy [r14 + local_height], r11
			vp_cpy r10, [r13 + ft_text_width]
			vp_cpy r11, [r13 + ft_text_height]

			;texture blend mode
			sdl_set_texture_blend_mode [r13 + ft_text_texture], SDL_BLENDMODE_BLEND

			vp_cpy r13, r0
			vp_cpy [r14 + local_bucket], r5
			ln_add_fnode r5, r0, r1
		endif
		vp_cpy r0, [r14 + local_handle]
		sdl_free_surface [r14 + local_surface]
		vp_cpy [r14 + local_handle], r0
	endif
	vp_cpy r0, [r14 + local_handle]

	vp_cpy r15, r4
	vp_ret

def_func_end
