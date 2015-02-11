%include "func.inc"
%include "heap.inc"

	fn_function "sys/mail_mailheap"
		;outputs
		;r0 = heap

		vp_lea [rel mailheap], r0
		vp_ret

		align 8, db 0
	mailheap:
		times HP_HEAP_SIZE db 0

	fn_function_end
