%include 'inc/func.inc'
%include 'class/class_string.inc'

	fn_function class/string/init
		;inputs
		;r0 = string object
		;r1 = vtable pointer
		;r2 = c string pointer
		;r3 = c string length
		;outputs
		;r1 = 0 if error, else ok

		def_local
			def_local_long	inst
			def_local_long	data
			def_local_long	length
		def_local_end

		;save inputs
		vp_sub local_size, r4
		set_src r2, r3
		set_dst .data, .length
		map_src_to_dst

		;init parent
		super_call string, init
		if r1, !=, 0
			vp_cpy r0, .inst

			;init myself
			vp_cpy .length, r2
			vp_cpy r2, [r0 + string_length]
			vp_inc r2
			static_call sys_mem, copy, {.data, &[r0 + string_data], r2}

			vp_cpy .inst, r0
		endif

		vp_add local_size, r4
		vp_ret

	fn_function_end
