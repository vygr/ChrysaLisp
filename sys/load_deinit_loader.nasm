%include "func.inc"
%include "load.inc"

	fn_function "sys/load_deinit_loader"
		;get statics
		fn_bind sys/load_statics, r0

		;free all function blocks
		vp_cpy [r0 + LD_STATICS_BLOCK_LIST], r1
		loop_start
			breakif r1, ==, 0
			vp_cpy [r1], r3
			sys_munmap r1, LD_BLOCK_SIZE
			vp_cpy r3, r1
		loop_end
		vp_ret

	fn_function_end
