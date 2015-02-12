%include "func.inc"
%include "load.inc"

	fn_function "sys/load_init_loader"
		;get loader statics and load function !
		vp_lea [rel __func_start], r0
		vp_add [r0 + FN_HEADER_LENGTH], r0
		vp_cpy r0, r5
		vp_add [r0 + FN_HEADER_LENGTH], r0
		vp_add [r0 + FN_HEADER_ENTRY], r0
		vp_call r0
		vp_add [r5 + FN_HEADER_ENTRY], r5
		vp_cpy r0, r6

		;add all to function list
		vp_lea [rel __func_start], r1
		loopstart
			vp_cpy [r1], r0
			breakif r0, ==, 0
			vp_cpy [r6 + LD_STATICS_FUNCTION_LIST], r0
			vp_cpy r0, [r1]
			vp_cpy r1, [r6 + LD_STATICS_FUNCTION_LIST]
			vp_add [r1 + FN_HEADER_LENGTH], r1
		loopend

		;bind all function intra references
		vp_cpy [r6 + LD_STATICS_FUNCTION_LIST], r2
		loopstart
			breakif r2, ==, 0
			vp_cpy r2, r0
			vp_add [r2 + FN_HEADER_LINKS], r0
			loopstart
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
				repeat
					vp_cpy byte[r0], r1l
					vp_inc r0
					vp_and 0xff, r1
				until r1, ==, 0
				vp_add 7, r0
				vp_and -8, r0
			loopend
			vp_cpy [r2], r2
		loopend
		vp_ret

	fn_function_end
