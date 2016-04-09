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

		def_structure	local
			def_long	local_inst
			def_long	local_string1
			def_long	local_string2
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r2, [r4 + local_string1]
		vp_cpy r3, [r4 + local_string2]

		;init parent
		super_call string, init
		if r1, !=, 0
			vp_cpy r0, [r4 + local_inst]

			;init myself
			vp_cpy [r4 + local_string1], r6
			vp_cpy [r4 + local_string2], r7
			vp_cpy [r6 + string_length], r1
			vp_add [r7 + string_length], r1
			vp_cpy r1, [r0 + string_length]
			vp_lea [r0 + string_data], r1
			vp_lea [r6 + string_data], r0
			vp_cpy [r6 + string_length], r2
			static_call sys_mem, copy
			vp_lea [r7 + string_data], r0
			vp_cpy [r7 + string_length], r2
			vp_inc r2
			static_call sys_mem, copy

			vp_cpy [r4 + local_inst], r0
		endif

		vp_add local_size, r4
		vp_ret

	fn_function_end
