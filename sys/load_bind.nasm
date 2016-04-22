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
		vp_rel _func_start, r8
		vp_xor r0, r0
		vp_cpy_i [r8 + fn_header_length], r0
		vp_add r0, r8
		vp_cpy_i [r8 + fn_header_entry], r0
		vp_add r0, r8

		;check if function already present !
		loop_flist_forward r8 + ld_statics_function_list, r6, r6
			vp_cpy r7, r0
			vp_lea [r6 + fn_header_pathname], r1
			vp_call string_compare
		loop_until r0, !=, 0
		if r6, !=, 0
			;found function already loaded
			vp_xor r0, r0
			vp_cpy_i [r6 + fn_header_entry], r0
			vp_add r6, r0
			vp_ret
		endif

		;get length of function on disk
		vp_lea [r8 + ld_statics_stat_buffer], r0
		sys_stat r7, r0
		if r0, !=, 0
			vp_xor r0, r0
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
			assert r0, !=, 0

			;add to block list for freeing
			ln_add_fnode r8 + ld_statics_block_list, r0, r1

			;set block pointers for loading
			vp_add 8, r0
			vp_cpy r0, [r8 + ld_statics_block_start]
			vp_add ld_block_size - 8, r0
			vp_cpy r0, [r8 + ld_statics_block_end]
		endif

		;open function file
		sys_open r7, o_rdonly, 0
		vp_cpy r0, r7

		;read into buffer
		vp_cpy [r8 + ld_statics_block_start], r3
		vp_lea [r8 + ld_statics_stat_buffer], r2
		sys_read r7, r3, [r2 + stat_fsize]

		;close function file
		sys_close r7

		;check loaded length equals file size
		vp_xor r0, r0
		vp_cpy_i [r3 + fn_header_length], r0
		if r0, !=, [r2 + stat_fsize]
			vp_rel size_error, r0
			sys_write_string 2, r0, size_error_end-size_error
			sys_exit 1
		endif

		;add to function list
		ln_add_fnode r8 + ld_statics_function_list, r3, r0

		;relocate vtable so we can discard paths
		vp_cpy [r8 + ld_statics_reloc_stack], r1
		vp_cpy r1, r6

		;copy paths to reloc buffer
		vp_xor r0, r0
		vp_xor r2, r2
		vp_cpy_i [r3 + fn_header_paths], r0
		vp_cpy_i [r3 + fn_header_length], r2
		vp_add r3, r0
		vp_add r3, r2
		loop_while r0, <, r2
		 	vp_cpy [r0], r5
			vp_cpy r5, [r1]
			vp_add 8, r0
			vp_add 8, r1
		loop_end

		;push reloc buffer entry
		vp_add 8, r1
		vp_cpy r6, [r1 - 8]
		vp_cpy r1, [r8 + ld_statics_reloc_stack]

		;overflow check
		vp_lea [r8 + ld_statics_size], r2
		if r1, >, r2
			vp_rel reloc_error, r0
			sys_write_string 2, r0, reloc_error_end-reloc_error
			sys_exit 1
		endif

		;bind links to paths in reloc buffer
		vp_xor r0, r0
		vp_xor r2, r2
		vp_cpy_i [r3 + fn_header_links], r0
		vp_cpy_i [r3 + fn_header_paths], r2
		vp_add r3, r0
		vp_add r3, r2
		vp_sub r2, r6
		loop_start
		 	vp_cpy [r0], r2
			breakif r2, ==, 0
			vp_add r0, r2
			vp_add r6, r2
			vp_cpy r2, [r0]
			vp_add 8, r0
		loop_end

		;adjust block start
		vp_add 8, r0
		vp_cpy r0, [r8 + ld_statics_block_start]

		;load and link function references
		;now actual addresses of strings in the reloc buffer
		vp_xor r0, r0
		vp_cpy_i [r3 + fn_header_links], r0
		vp_add r3, r0
		vp_push r3
		loop_start
			vp_cpy [r0], r1
			breakif r1, ==, 0
			vp_push r0
			vp_cpy r1, r0
			vp_call ld_load_function
			if r0, ==, 0
				;no such file
				vp_rel bind_error, r0
				sys_write_string 2, r0, bind_error_end-bind_error
				vp_cpy [r4], r0
				vp_cpy [r0], r0
				vp_call string_skip
				vp_lea [r0 - 1], r1
				vp_cpy [r4], r0
				vp_cpy [r0], r0
				vp_sub r0, r1
				sys_write_string 2, r0, r1
				sys_write_char 2, 10
				sys_exit 1
			endif
			vp_cpy r0, r1
			vp_pop r0
			vp_cpy r1, [r0]
			vp_add 8, r0
		loop_end
		vp_pop r3

		;get loader statics !
		vp_rel _func_start, r8
		vp_xor r0, r0
		vp_cpy_i [r8 + fn_header_length], r0
		vp_add r0, r8
		vp_cpy_i [r8 + fn_header_entry], r0
		vp_add r0, r8

		;pop reloc buffer
		vp_cpy [r8 + ld_statics_reloc_stack], r0
		vp_cpy [r0 - 8], r0
		vp_cpy r0, [r8 + ld_statics_reloc_stack]

		;return function address
		vp_xor r0, r0
		vp_cpy_i [r3 + fn_header_entry], r0
		vp_add r3, r0
		vp_ret

	string_compare:
		vp_xor r2, r2
		vp_xor r3, r3
		loop_start
			vp_cpy_b [r0], r2
			vp_cpy_b [r1], r3
			breakif r2, !=, r3
			if r2, ==, 0
				vp_ret
			endif
			vp_inc r0
			vp_inc r1
		loop_end
		vp_xor r0, r0
		vp_ret

	string_skip:
		vp_xor r1, r1
		loop_start
			vp_cpy_b [r0], r1
			vp_inc r0
		loop_until r1, ==, 0
		ret

	bind_error:
		db 'Bind error: '
	bind_error_end:

	reloc_error:
		db 'Reloc buffer overflow !', 10
	reloc_error_end:

	size_error:
		db 'Length field error !', 10
	size_error_end:

	fn_function_end
