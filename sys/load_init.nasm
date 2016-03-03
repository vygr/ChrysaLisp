%include 'inc/func.inc'
%include 'inc/load.inc'

	fn_function sys/load_init
		;get loader statics and load function !
		vp_lea [rel _func_start], r6
		vp_add [r6 + FN_HEADER_LENGTH], r6
		vp_cpy r6, r5
		vp_add [r6 + FN_HEADER_LENGTH], r6
		vp_add [r6 + FN_HEADER_ENTRY], r6
		vp_add [r5 + FN_HEADER_ENTRY], r5

		;add all to function list
		vp_lea [rel _func_start], r1
		loop_start
			vp_cpy [r1], r0
			breakif r0, ==, 0
			vp_cpy [r6 + LD_STATICS_FUNCTION_LIST], r0
			vp_cpy r0, [r1]
			vp_cpy r1, [r6 + LD_STATICS_FUNCTION_LIST]
			vp_add [r1 + FN_HEADER_LENGTH], r1
		loop_end

		;bind all function intra references
		vp_cpy [r6 + LD_STATICS_FUNCTION_LIST], r2
		loop_start
			breakif r2, ==, 0
			vp_cpy r2, r0
			vp_add [r2 + FN_HEADER_LINKS], r0
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
