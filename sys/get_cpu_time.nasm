%include "func.inc"
%include "syscall.inc"

	fn_function "sys/get_cpu_time"
		;outputs
		;r0 = time in usec
		;trashes
		;r2

		;calculate wake time
		vp_sub TIMEVAL_SIZE, r4
		vp_cpy r4, r0
		sys_gettimeofday r0, 0
		vp_mul 1000000, r0
		vp_add r2, r0
		vp_add TIMEVAL_SIZE, r4
		vp_ret

	fn_function_end
