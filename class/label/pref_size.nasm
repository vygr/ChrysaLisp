%include 'inc/func.inc'
%include 'class/class_flow.inc'
%include 'class/class_label.inc'

	fn_function class/label/pref_size
		;inputs
		;r0 = label object
		;outputs
		;r10 = prefered width
		;r11 = prefered height
		;trashes
		;all but r0, r4

		def_structure	local
			def_long	local_inst
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]

		method_call flow, pref_size, '[r0 + label_flow]'
		vp_add label_border_size * 2, r10
		vp_add label_border_size * 2, r11

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
