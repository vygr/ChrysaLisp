%include "func.inc"
%include "heap.inc"

	fn_function "sys/mail_get_statics"
		;outputs
		;r0 = heap

		vp_lea [rel statics], r0
		vp_ret

		align 8, db 0
	statics:
		times HP_HEAP_SIZE db 0

	fn_function_end
