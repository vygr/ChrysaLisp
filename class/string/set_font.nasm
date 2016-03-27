%include 'inc/func.inc'
%include 'inc/font.inc'
%include 'class/class_string.inc'

	fn_function class/string/set_font
		;inputs
		;r0 = string object
		;r1 = font name
		;r2 = point size
		;trashes
		;all but r0, r4

		def_structure	local
			def_long	local_inst
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]

		;open font
		vp_cpy r1, r0
		vp_cpy r2, r1
		static_call gui_font, open

		vp_cpy [r4 + local_inst], r1
		vp_cpy r0, [r1 + string_font]

		vp_cpy [r4 + local_inst], r1
		vp_add local_size, r4
		vp_ret

	fn_function_end
