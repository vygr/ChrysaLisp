%include 'inc/func.inc'
%include 'class/class_string.inc'
%include 'class/class_label.inc'

	fn_function class/label/set_text
		;inputs
		;r0 = label object
		;r1 = string pointer
		;trashes
		;all but r0, r4

		def_structure	local
			def_long	local_inst
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]

		vp_cpy [r0 + label_string], r0
		static_call string, set_text

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
