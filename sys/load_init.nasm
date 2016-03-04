%include 'inc/func.inc'
%include 'inc/load.inc'

	fn_function sys/load_init, no_debug_enter
		;get loader statics and load function !
		vp_lea [rel _func_start], r6
		vp_add [r6 + fn_header_length], r6
		vp_cpy r6, r5
		vp_add [r6 + fn_header_length], r6
		vp_add [r6 + fn_header_entry], r6
		vp_add [r5 + fn_header_entry], r5

		;add all to function list
		vp_lea [rel _func_start], r1
		loop_start
			vp_cpy [r1], r0
			breakif r0, ==, 0
			vp_cpy [r6 + ld_statics_function_list], r0
			vp_cpy r0, [r1]
			vp_cpy r1, [r6 + ld_statics_function_list]
			vp_add [r1 + fn_header_length], r1
		loop_end

		;bind all function intra references
		vp_cpy [r6 + ld_statics_function_list], r2
		loop_start
			breakif r2, ==, 0
			vp_cpy r2, r0
			vp_add [r2 + fn_header_links], r0
			loop_start
				vp_cpy [r0], r1
				breakif r1, ==, 0
				vp_lea [r0 + 8], r0
				vp_push r0
				vp_push r2
				vp_push r5
				vp_call r5		;sys/load_function
				vp_cpy r0, r1
				vp_pop r5
				vp_pop r2
				vp_pop r0
				vp_cpy r1, [r0 - 8]
				loop_start
					vp_cpy byte[r0], r1l
					vp_inc r0
					vp_and 0xff, r1
				loop_until r1, ==, 0
				vp_add 7, r0
				vp_and -8, r0
			loop_end
			vp_cpy [r2], r2
		loop_end
		vp_ret

	fn_function_end
