%include 'inc/func.inc'
%include 'inc/syscall.inc'
%include 'inc/string.inc'

	fn_function sys/write_debug_str, no_debug_enter
		;inputs
		;r0 = function name
		;r1 = line number
		;r2 = debug string
		;r3 = debug string
		;trashes
		;r0-r3, r5

		def_structure	local
			def_long	local_name
			def_long	local_line
			def_long	local_string
			def_long	local_str
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_name]
		vp_cpy r1, [r4 + local_line]
		vp_cpy r2, [r4 + local_string]
		vp_cpy r3, [r4 + local_str]

		;tab in by stack depth
		static_call sys_task, stack_depth
		if r0, >, 0
			vp_lea [r0 - (11*8)], r2
			vp_cpy ' ', r0
			vp_cpy 2, r1
			loop_while r2, >, 0
				static_call sys_io, char
				vp_sub 8, r2
			loop_end
		endif
		vp_cpy [r4 + local_name], r0
		vp_cpy 2, r1
		static_call sys_io, string
		fn_string '< ', r0
		vp_cpy 2, r1
		static_call sys_io, string
		vp_cpy [r4 + local_line], r0
		vp_cpy 2, r1
		vp_cpy 10, r2
		static_call sys_io, number
		fn_string ' >: ', r0
		static_call sys_io, string
		vp_cpy [r4 + local_string], r0
		vp_cpy 2, r1
		static_call sys_io, string
		fn_string ' :-> ', r0
		vp_cpy 2, r1
		static_call sys_io, string
		vp_cpy [r4 + local_str], r0
		vp_cpy 2, r1
		static_call sys_io, string
		vp_cpy 10, r0
		vp_cpy 2, r1
		static_call sys_io, char

		vp_add local_size, r4
		vp_ret

	fn_function_end
