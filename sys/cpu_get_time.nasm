%include "inc/func.inc"
%include "inc/syscall.inc"

	fn_function "sys/cpu_get_time"
		;outputs
		;r0 = time in usec

		;calculate wake time
		vp_sub TIMEVAL_SIZE, r4
		vp_cpy r4, r0
		sys_gettimeofday r0, 0
		vp_cpy [r4 + TIMEVAL_SEC], r0
		vp_mul 1000000, r0
		vp_add [r4 + TIMEVAL_USEC], r0
		vp_add TIMEVAL_SIZE, r4
		vp_ret

	fn_function_end
