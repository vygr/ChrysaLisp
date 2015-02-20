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
			vp_cpy LK_BUFFER_CHAN_1, r10
			vp_cpy LK_BUFFER_CHAN_2, r11
		else
			vp_cpy LK_BUFFER_CHAN_2, r10
			vp_cpy LK_BUFFER_CHAN_1, r11
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
		;allways write task count to neibour
		;r10 is input channel, r11 is output channel
		vp_add r12, r10
		vp_add r12, r11
		vp_xor r9, r9
		loopstart
			;exchange task counts
			fn_bind sys/task_statics, r0
			vp_cpy [r0 + TK_STATICS_TASK_COUNT], r0
			vp_cpy r0, [r10 + LK_CHAN_TASK_COUNT]
			vp_cpy [r11 + LK_CHAN_TASK_COUNT], r0
			vp_cpy r0, [r4 + LK_NODE_TASK_COUNT]

			;check if we need to grab a new message
			if r9, ==, 0
				;no outgoing message so see if any offchip mail for me
				fn_call sys/get_cpu_id
				fn_bind sys/mail_statics, r8
				vp_lea [r8 + ML_STATICS_OFFCHIP_LIST], r8
				lh_get_head r8, r8
				loopstart
					vp_cpy r8, r7
					ln_get_succ r8, r8
					breakif r8, ==, 0
					continueif r0, !=, [r7 + (ML_MSG_DEST + 8)]
					vp_cpy r7, r0
					vp_cpy r7, r9
					ln_remove_node r0, r1
					break
				loopend
			endif

			;if we have a message to send then see if we can send it
			if r9, !=, 0
				vp_cpy [r10 + LK_CHAN_STATUS], r0
				if r0, ==, LK_CHAN_STATUS_READY
					;copy message data
					vp_cpy r9, r0
					vp_lea [r10 + LK_CHAN_MSG], r1
					vp_cpy [r9 + ML_MSG_LENGTH], r2
					fn_call sys/mem_copy

					;free message
					vp_cpy r9, r1
					fn_call sys/mail_free

					;busy status
					vp_cpy LK_CHAN_STATUS_BUSY, long[r10 + LK_CHAN_STATUS]
					vp_xor r9, r9
				endif
			endif

			;check for recived message
			vp_cpy [r11 + LK_CHAN_STATUS], r0
			if r0, ==, LK_CHAN_STATUS_BUSY
				;allocate msg, copy over data
				fn_call sys/mail_alloc
				vp_cpy r0, r8
				vp_cpy r0, r1
				vp_lea [r11 + LK_CHAN_MSG], r0
				vp_cpy [r0 + ML_MSG_LENGTH], r2
				fn_call sys/mem_copy

				;send onwards
				vp_cpy r8, r0
				fn_call sys/mail_send

				;clear status
				vp_cpy LK_CHAN_STATUS_READY, long[r11 + LK_CHAN_STATUS]
			endif

			;exit if signaled by kernel, else deshedule and loop
			vp_cpy [r4 + LK_NODE_CPU_ID], r0
			breakif r0, ==, -1
			fn_call sys/task_deshedule
		loopend

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
