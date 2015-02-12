%include "func.inc"
%include "load.inc"

	fn_function "sys/load_get_statics"
		;outputs
		;r0 = statics

		vp_lea [rel statics], r0
		vp_ret

		align 8, db 0
	statics:
		times LD_STATICS_SIZE db 0

	fn_function_end
