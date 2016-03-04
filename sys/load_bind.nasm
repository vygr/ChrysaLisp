%include 'inc/func.inc'
%include 'inc/load.inc'

	fn_function sys/load_bind, no_debug_enter
		;input
		;r0 = function path name
		;output
		;r0 = 0 else, function entry pointer
		;trashes
		;r1-r3, r5-r8

	ld_load_function:
		;save pathname
		vp_cpy r0, r7

		;get loader statics !
		vp_lea [rel _func_start], r8
		vp_add [r8 + fn_header_length], r8
		vp_add [r8 + fn_header_entry], r8

		;check if function already present !
		vp_xor r5, r5
		vp_cpy [r8 + ld_statics_function_list], r6
		loop_while r6, !=, 0
			vp_cpy r7, r0
			vp_lea [r6 + fn_header_pathname], r1
			vp_call string_compare
			if r0, !=, 0
				vp_cpy r6, r5
				vp_add [r6 + fn_header_entry], r5
			endif
			vp_cpy [r6], r6
		loop_until r5, !=, 0
		if r5, !=, 0
			;found function already loaded
			vp_cpy r5, r0
			vp_ret
		endif

		;get length of function on disk
		vp_lea [r8 + ld_statics_stat_buffer], r0
		sys_stat r7, r0
		if r0, !=, 0
			xor r0, r0
			vp_ret
		endif

		;ensure space for new function
		vp_cpy [r8 + ld_statics_block_start], r1
		vp_cpy [r8 + ld_statics_block_end], r2
		vp_sub r1, r2
		vp_lea [r8 + ld_statics_stat_buffer], r0
		vp_cpy [r0 + stat_fsize], r0
		if r2, <, r0
			;not enough so allocate new function buffer
			sys_mmap 0, ld_block_size, prot_read|prot_write|prot_exec, map_private|map_anon, -1, 0

			;add to block list for freeing
			vp_cpy [r8 + ld_statics_block_list], r3
			vp_cpy r3, [r0]
			vp_cpy r0, [r8 + ld_statics_block_list]

			;set block pointers for loading
			vp_add 8, r0
			vp_cpy r0, [r8 + ld_statics_block_start]
			vp_add ld_block_size - 8, r0
			vp_cpy r0, [r8 + ld_statics_block_end]
		endif

		;open function file
		sys_open r7, o_rdonly, 0
		vp_cpy r0, r12

		;read into buffer
		vp_cpy [r8 + ld_statics_block_start], r3
		vp_lea [r8 + ld_statics_stat_buffer], r2
		sys_read r12, r3, [r2 + stat_fsize]

		;close function file
		sys_close r12

		;add to function list
		vp_cpy [r8 + ld_statics_function_list], r0
		vp_cpy r0, [r3]
		vp_cpy r3, [r8 + ld_statics_function_list]

		;adjust block start
		vp_cpy r3, r0
		vp_add [r2 + stat_fsize], r0
		vp_cpy r0, [r8 + ld_statics_block_start]

		;load and link function references
		vp_cpy r3, r0
		vp_add [r3 + fn_header_links], r0
		loop_start
			vp_cpy [r0], r1
			breakif r1, ==, 0
			vp_lea [r0 + 8], r0
			vp_push r0, r3
			vp_call ld_load_function
			if r0, ==, 0
				;no such file
				vp_lea [rel bind_error], r0
				sys_write_string 1, r0, bind_error_end-bind_error
				vp_cpy [r4 + 8], r0
				vp_call string_skip
				vp_lea [r0 - 1], r1
				vp_cpy [r4 + 8], r0
				vp_sub r0, r1
				sys_write_string 1, r0, r1
				sys_write_char 1, 10
				sys_exit 1
			endif
			vp_cpy r0, r1
			vp_pop r0, r3
			vp_cpy r1, [r0 - 8]
			vp_call string_skip
			vp_add 7, r0
			vp_and -8, r0
		loop_end

		;return function address
		vp_cpy r3, r0
		vp_add [r3 + fn_header_entry], r0
		vp_ret

	string_compare:
		loop_start
			vp_cpy byte[r0], r2l
			vp_cpy byte[r1], r3l
			vp_and 0xff, r2
			vp_and 0xff, r3
			breakif r2, !=, r3
			if r2, ==, 0
				vp_cpy 1, r0
				vp_ret
			endif
			vp_inc r0
			vp_inc r1
		loop_end
		vp_xor r0, r0
		vp_ret

	string_skip:
		loop_start
			vp_cpy byte[r0], r1l
			vp_inc r0
			vp_and 0xff, r1
		loop_until r1, ==, 0
		vp_ret

	bind_error:
		db 'Bind error: '
	bind_error_end:

	fn_function_end
