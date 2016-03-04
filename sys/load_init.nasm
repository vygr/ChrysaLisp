%include 'inc/func.inc'
%include 'inc/load.inc'

	fn_function sys/load_init, no_debug_enter
		;set prebound functions as read/write/executable
		vp_lea [rel _func_start], r0
		vp_cpy r0, r1
		loop_start
		 	vp_cpy [r1 + fn_header_length], r2
			breakif r2, ==, 0
			vp_add r2, r1
		loop_end
		vp_and -ld_page_size, r0
		vp_sub r0, r1
		sys_mprotect r0, r1, prot_read|prot_write|prot_exec

		;get loader statics and load function !
		vp_lea [rel _func_start], r6
		vp_add [r6 + fn_header_length], r6
		vp_cpy r6, r5
		vp_add [r6 + fn_header_length], r6
		vp_add [r6 + fn_header_entry], r6
		vp_add [r5 + fn_header_entry], r5

		;add all prebound functions to function list
		vp_lea [rel _func_start], r1
		loop_start
		 	vp_cpy [r1 + fn_header_length], r2
			breakif r2, ==, 0
			vp_cpy [r6 + ld_statics_function_list], r0
			vp_cpy r0, [r1]
			vp_cpy r1, [r6 + ld_statics_function_list]
			vp_add r2, r1
		loop_end

		;bind all prebound function intra references
		vp_lea [rel _func_start], r2
		loop_start
			vp_cpy [r2 + fn_header_length], r1
			breakif r1, ==, 0
			vp_cpy r2, r0
			vp_add r1, r2
			vp_add [r0 + fn_header_links], r0
			loop_start
				vp_cpy [r0], r1
				breakif r1, ==, 0
				vp_push r0, r2, r5
				vp_add r1, r0
				vp_call r5		;sys/load_function
				if r0, ==, 0
					;no such function
					vp_lea [rel bind_error], r0
					sys_write_string 1, r0, bind_error_end-bind_error
					vp_cpy [r4 + 16], r0
					vp_add [r0], r0
					loop_start
						vp_cpy byte[r0], r1l
						vp_inc r0
						vp_and 0xff, r1
					loop_until r1, ==, 0
					vp_lea [r0 - 1], r1
					vp_cpy [r4 + 16], r0
					vp_add [r0], r0
					vp_sub r0, r1
					sys_write_string 1, r0, r1
					sys_write_char 1, 10
					sys_exit 1
				endif
				vp_cpy r0, r1
				vp_pop r0, r2, r5
				vp_cpy r1, [r0]
				vp_add 8, r0
			loop_end
		loop_end
		vp_ret

	bind_error:
		db 'Prebind error: '
	bind_error_end:

	fn_function_end
