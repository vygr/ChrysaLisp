%include "func.inc"
%include "load.inc"

	fn_function "sys/load_deinit_loader"
		;get statics
		fn_call sys/load_get_statics

		;free all function blocks
		vp_cpy [r0 + LD_STATICS_BLOCK_LIST], r1
		loopstart
			breakif r1, ==, 0
			vp_cpy [r1], r3
			sys_munmap r1, LD_BLOCK_SIZE
			vp_cpy r3, r1
		loopend
		vp_ret

	fn_function_end
