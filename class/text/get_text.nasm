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

		def_local
			def_local_long	inst
		def_local_end

		;save inputs
		vp_sub local_size, r4
		set_src r0
		set_dst .inst
		map_src_to_dst

		;reference the string
		vp_cpy [r0 + text_string], r0
		if r0, !=, 0
			static_call string, ref, {r0}
		endif
		vp_cpy r0, r1

		vp_cpy .inst, r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
