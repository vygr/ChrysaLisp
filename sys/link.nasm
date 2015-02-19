%include "func.inc"
%include "mail.inc"
%include "task.inc"
%include "link.inc"

	fn_function "sys/link"
		;started by kernel for each link

		;allocate link node on stack and link to links list
		vp_sub LK_NODE_SIZE, r4
		vp_lea [r4 + LK_NODE_NODE], r0
		fn_bind sys/link_statics, r1
		vp_lea [r1 + LK_STATICS_LINKS_LIST], r1
		lh_add_at_head r1, r0, r2

		;read params msg from kernel
		vp_lea [r15 + TK_NODE_MAILBOX], r0
		fn_call sys/mail_read
		vp_cpy r1, r14

		;init link node cpu id and task count
		vp_cpy byte[r14 + ML_MSG_DATA + 1], r1l
		vp_cpy byte[r14 + ML_MSG_DATA + 2], r2l
		vp_and 0xff, r1
		vp_and 0xff, r2
		vp_sub '0', r1
		vp_sub '0', r2
		vp_mul 10, r1
		vp_add r2, r1
		vp_cpy byte[r14 + ML_MSG_DATA + 4], r2l
		vp_cpy byte[r14 + ML_MSG_DATA + 5], r3l
		vp_and 0xff, r2
		vp_and 0xff, r3
		vp_sub '0', r2
		vp_sub '0', r3
		vp_mul 10, r2
		vp_add r3, r2
		fn_call sys/get_cpu_id
		if r1, ==, r0
			vp_cpy r2, r1
		endif
		vp_cpy r1, [r4 + LK_NODE_CPU_ID]
		vp_cpy 0, long[r4 + LK_NODE_TASK_COUNT]

		;open shared memory region
		vp_lea [r14 + ML_MSG_DATA], r0
		sys_shmopen r0, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR
		vp_cpy r0, r13

		;set size of region
		sys_ftruncate r13, LK_BUFFER_SIZE

		;map shared object
		sys_mmap 0, LK_BUFFER_SIZE, PROT_READ | PROT_WRITE, MAP_SHARED, r13, 0
		vp_cpy r0, r12

		;clear region and sleep small amount of time for other end to sync up
		for r0, 0, LK_BUFFER_SIZE, 8
			vp_cpy 0, long[r12 + r0]
		next
		vp_cpy 1000000, r0
		fn_call sys/task_sleep

		;read and write messages through the shared buffer in r12
		;allways read task count from neibour
		;allways write task count to a neibour

		;unmap object
		sys_munmap r12, LK_BUFFER_SIZE

		;unlink shared object
		vp_lea [r14 + ML_MSG_DATA], r0
		sys_shmunlink r0

		;free params msg
		vp_cpy r14, r1
		fn_call sys/mail_free

		;remove from links list and deallocate link node on stack
		vp_lea [r4 + LK_NODE_NODE], r0
		ln_remove_node r0, r1
		vp_add LK_NODE_SIZE, r4

		;stop this task
		fn_jmp sys/task_stop

	fn_function_end
