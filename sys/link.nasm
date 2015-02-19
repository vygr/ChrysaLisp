%include "func.inc"
%include "mail.inc"
%include "task.inc"

	LINK_BUFFER_SIZE	equ ML_MSG_SIZE * 2

	fn_function "sys/link"
		;started by kernel for each link

		;read params msg from kernel
		vp_lea [r15 + TK_NODE_MAILBOX], r0
		fn_call sys/mail_read
		vp_cpy r1, r14

		;open shared memory region
		vp_lea [r14 + ML_MSG_DATA], r0
		sys_shmopen r0, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR
		vp_cpy r0, r13

		;set size of region
		sys_ftruncate r13, LINK_BUFFER_SIZE

		;map shared object
		sys_mmap 0, LINK_BUFFER_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, r13, 0
		vp_cpy r0, r12

		;clear region and sleep small amount of time for other end to sync up
		for r0, 0, LINK_BUFFER_SIZE, 8
			vp_cpy 0, long[r12 + r0]
		next
		vp_cpy 1000000, r0
		fn_call sys/task_sleep

		;read and write messages through the shared buffer in r12

		;unmap object
		sys_munmap r12, LINK_BUFFER_SIZE

		;unlink shared object
		vp_lea [r14 + ML_MSG_DATA], r0
		sys_shmunlink r0

		;free params msg
		vp_cpy r14, r1
		fn_call sys/mail_free

		;stop this task
		fn_jmp sys/task_stop

	fn_function_end
