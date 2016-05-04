%include 'inc/func.inc'
%include 'class/class_text.inc'
%include 'class/class_string.inc'

	fn_function class/text/set_text
		;inputs
		;r0 = text object
		;r1 = 0, else string object
		;trashes
		;all but r0, r4

		def_local
			def_local_long	inst
			def_local_long	string
		def_local_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1
		set_dst .inst, .string
		map_src_to_dst

		;deref the old string
		vp_cpy [r0 + text_string], r0
		if r0, !=, 0
			s_call string, deref, {r0}
		endif

		;transfer reference to new string
		vp_cpy .inst, r0
		vp_cpy .string, r1
		vp_cpy r1, [r0 + text_string]

		vp_add local_size, r4
		vp_ret

	fn_function_end
