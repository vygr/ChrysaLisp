%include 'inc/func.inc'
%include 'class/class_title.inc'
%include 'class/class_flow.inc'

	fn_function class/title/layout
		;inputs
		;r0 = title object
		;trashes
		;all but r0, r4

		def_local
			def_local_long	inst
		def_local_end

		;save inputs
		vp_sub local_size, r4
		set_src r0
		set_dst .inst
		map_src_to_dst

		vp_cpy [r0 + view_w], r10
		vp_cpy [r0 + view_h], r11
		vp_sub title_border_size * 2, r10
		vp_sub title_border_size * 2, r11
		s_call flow, change, {[r0 + label_flow], title_border_size, title_border_size, r10, r11}

		vp_cpy .inst, r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
