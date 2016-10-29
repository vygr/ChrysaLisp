%include 'inc/func.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'

	def_func class/window/layout
		;inputs
		;r0 = window object
		;trashes
		;all but r0, r4

		def_struct local
			ptr local_inst
		def_struct_end

		;save inputs
		vp_sub local_size, r4
		set_src r0
		set_dst [r4 + local_inst]
		map_src_to_dst

		vp_cpy [r0 + view_w], r10
		vp_cpy [r0 + view_h], r11
		vp_sub window_border_size * 2, r10
		vp_sub window_border_size * 2, r11
		f_call flow, change, {[r0 + window_flow], window_border_size, window_border_size, r10, r11}

		f_call window, opaque, {[r4 + local_inst]}
		vp_add local_size, r4
		vp_ret

	def_func_end
