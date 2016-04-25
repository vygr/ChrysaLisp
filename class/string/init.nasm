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

		def_structure	local
			def_long	local_inst
			def_long	local_data
			def_long	local_length
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r2, [r4 + local_data]
		vp_cpy r3, [r4 + local_length]

		;init parent
		super_call string, init
		if r1, !=, 0
			vp_cpy r0, [r4 + local_inst]

			;init myself
			vp_cpy [r4 + local_length], r2
			vp_cpy r2, [r0 + string_length]
			vp_inc r2
			static_call sys_mem, copy, {[r4 + local_data], &[r0 + string_data], r2}

			vp_cpy [r4 + local_inst], r0
		endif

		vp_add local_size, r4
		vp_ret

	fn_function_end
