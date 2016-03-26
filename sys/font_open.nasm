%include 'inc/func.inc'
%include 'inc/font.inc'
%include 'inc/string.inc'
%include 'inc/sdl2.inc'
%include 'inc/task.inc'

	fn_function sys/font_open, no_debug_enter
		;inputs
		;r0 = font name
		;r1 = point size
		;outputs
		;r0 = 0 if error, else font cache entry
		;trashes
		;r1-r3, r5-r6

		def_structure	local
			def_long	local_font
			def_long	local_points
			def_long	local_handle
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_font]
		vp_cpy r1, [r4 + local_points]

		;get font statics
		static_bind sys_font, statics, r5

		;search font list
		loop_list_forward r5 + ft_statics_font_list, r6, r5
			vp_cpy [r4 + local_points], r0
			continueif r0, !=, [r5 + ft_font_points]
			vp_lea [r5 + ft_font_name], r0
			vp_cpy [r4 + local_font], r1
			static_call sys_string, compare
		loop_until r0, !=, 0

		;did we find it ?
		vp_cpy r5, r0
		if r6, ==, 0
			;no so try open it
			vp_lea [rel kernel_callback], r0
			vp_cpy r4, r1
			static_call sys_task, callback
			vp_cpy [r4 + local_handle], r0
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

		ttf_open_font [r14 + local_font], [r14 + local_points]
		if r0, !=, 0
			vp_cpy r0, r5
			vp_cpy [r14 + local_font], r0
			static_call sys_string, length
			vp_lea	[r1 + ft_font_size + 1], r0
			static_call sys_mem, alloc
			fn_assert r0, !=, 0
			vp_cpy r0, r13

			vp_cpy [r14 + local_points], r0
			vp_cpy r0, [r13 + ft_font_points]
			vp_cpy r5, [r13 + ft_font_handle]
			vp_lea [r13 + ft_font_name], r1
			vp_cpy [r14 + local_font], r0
			static_call sys_string, copy

			;fill in ascent, descent and height
			ttf_font_ascent [r13 + ft_font_handle]
			vp_cpy r0, [r13 + ft_font_ascent]
			ttf_font_descent [r13 + ft_font_handle]
			vp_cpy r0, [r13 + ft_font_descent]
			ttf_font_height [r13 + ft_font_handle]
			vp_cpy r0, [r13 + ft_font_height]

			vp_cpy r13, r0
			static_bind sys_font, statics, r5
			vp_add ft_statics_font_list, r5
			lh_add_at_tail r5, r0, r1
		endif
		vp_cpy r0, [r14 + local_handle]

		vp_cpy r15, r4
		vp_ret

	fn_function_end
