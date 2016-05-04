%include 'inc/func.inc'
%include 'class/class_string.inc'

	fn_function class/string/init1
		;inputs
		;r0 = string object
		;r1 = vtable pointer
		;r2 = string object
		;r3 = string object
		;outputs
		;r1 = 0 if error, else ok

		def_local
			def_local_long	inst
			def_local_long	string1
			def_local_long	string2
		def_local_end

		;save inputs
		vp_sub local_size, r4
		set_src r2, r3
		set_dst .string1, .string2
		map_src_to_dst

		;init parent
		p_call string, init, {r0, r1, r2, r3}, {r1}
		if r1, !=, 0
			vp_cpy r0, .inst

			;init myself
			vp_cpy .string1, r6
			vp_cpy .string2, r7
			vp_cpy [r6 + string_length], r1
			vp_add [r7 + string_length], r1
			vp_cpy r1, [r0 + string_length]
			s_call sys_mem, copy, {:[r6 + string_data], :[r0 + string_data], [r6 + string_length]}, {_, r1}
			vp_cpy [r7 + string_length], r2
			vp_inc r2
			s_call sys_mem, copy, {:[r7 + string_data], r1, r2}, {_, _}

			vp_cpy .inst, r0
		endif

		vp_add local_size, r4
		vp_ret

	fn_function_end
