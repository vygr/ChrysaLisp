%include 'inc/func.inc'
%include 'class/class_text.inc'
%include 'class/class_string.inc'

	fn_function class/text/get_text
		;inputs
		;r0 = text object
		;outputs
		;r0 = text object
		;r1 = 0 else, string object
		;trashes
		;all but r0, r4

		def_structure	local
			def_long	local_inst
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]

		;reference the string
		vp_cpy [r0 + text_string], r0
		if r0, !=, 0
			static_call string, ref
		endif
		vp_cpy [r4 + local_inst], r0
		vp_cpy [r0 + text_string], r1

		vp_add local_size, r4
		vp_ret

	fn_function_end
