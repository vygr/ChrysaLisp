%include 'inc/func.inc'
%include 'class/class_title.inc'
%include 'class/class_window.inc'

	fn_function class/window/set_title
		;inputs
		;r0 = window object
		;r1 = title string

		def_structure	local
			def_long	local_inst
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]

		vp_cpy [r0 + window_title], r0
		static_call label, set_text

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
