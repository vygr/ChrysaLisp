%include 'inc/func.inc'
%include 'class/class_text.inc'
%include 'class/class_label.inc'

	fn_function class/label/get_text
		;inputs
		;r0 = label object
		;outputs
		;r0 = label object
		;r1 = 0, else string object

		def_structure	local
			def_long	local_inst
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]

		static_call text, get_text, {[r0 + label_text]}

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
