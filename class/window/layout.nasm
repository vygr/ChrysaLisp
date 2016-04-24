%include 'inc/func.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'

	fn_function class/window/layout
		;inputs
		;r0 = window object
		;trashes
		;all but r0, r4

		def_structure	local
			def_long	local_inst
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]

		vp_cpy [r0 + view_w], r10
		vp_cpy [r0 + view_h], r11
		vp_sub window_border_size * 2, r10
		vp_sub window_border_size * 2, r11
		static_call flow, change, '[r0 + window_flow], window_border_size, window_border_size, r10, r11'

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
