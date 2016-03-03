%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/task.inc'
%include 'inc/link.inc'

	fn_function sys/link
		;started by kernel for each link

		;allocate link node on stack and link to links list
		vp_sub lk_node_size, r4
		vp_cpy 0, qword[r4 + lk_node_table + lk_table_array]
		vp_cpy 0, qword[r4 + lk_node_table + lk_table_array_size]
		vp_lea [r4 + lk_node_node], r0
		fn_bind sys/link_statics, r1
		vp_lea [r1 + lk_statics_links_list], r1
		lh_add_at_head r1, r0, r2

		;read params msg from kernel
		fn_call sys/mail_read_mymail
		vp_cpy r0, r14

		;init link node cpu id and task count
		vp_cpy byte[r14 + ML_MSG_DATA + 5], r0l
		vp_cpy byte[r14 + ML_MSG_DATA + 6], r1l
		vp_cpy byte[r14 + ML_MSG_DATA + 7], r2l
		vp_and 0xff, r0
		vp_and 0xff, r1
		vp_and 0xff, r2
		vp_sub '0', r0
		vp_sub '0', r1
		vp_sub '0', r2
		vp_mul 100, r0
		vp_mul 10, r1
		vp_add r0, r1
		vp_add r2, r1
		vp_cpy byte[r14 + ML_MSG_DATA + 9], r0l
		vp_cpy byte[r14 + ML_MSG_DATA + 10], r2l
		vp_cpy byte[r14 + ML_MSG_DATA + 11], r3l
		vp_and 0xff, r0
		vp_and 0xff, r2
		vp_and 0xff, r3
		vp_sub '0', r0
		vp_sub '0', r2
		vp_sub '0', r3
		vp_mul 100, r0
		vp_mul 10, r2
		vp_add r0, r2
		vp_add r3, r2
		fn_call sys/cpu_get_id
		if r1, ==, r0
			vp_cpy r2, r1
			vp_cpy lk_buffer_chan_1, r10
			vp_cpy lk_buffer_chan_2, r11
		else
			vp_cpy lk_buffer_chan_2, r10
			vp_cpy lk_buffer_chan_1, r11
		endif
		vp_cpy r1, [r4 + lk_node_cpu_id]
		vp_cpy 0, qword[r4 + lk_node_task_count]

		;send link routing message to neighbor kernel
		vp_cpy r0, r8
		vp_cpy r1, r9
		fn_call sys/mail_alloc
		vp_cpy 0, qword[r0 + ML_MSG_DEST]
		vp_cpy r9, [r0 + (ML_MSG_DEST + 8)]
		vp_cpy 0, qword[r0 + ML_MSG_DATA + kn_data_kernel_user]
		vp_cpy 0, qword[r0 + ML_MSG_DATA + kn_data_kernel_reply]
		vp_cpy 0, qword[r0 + (ML_MSG_DATA + kn_data_kernel_reply + 8)]
		vp_cpy fn_call_task_route, qword[r0 + ML_MSG_DATA + kn_data_kernel_function]
		vp_cpy r8, [r0 + ML_MSG_DATA + kn_data_link_route_origin]
		vp_cpy r8, [r0 + ML_MSG_DATA + kn_data_link_route_via]
		vp_cpy 1, qword[r0 + ML_MSG_DATA + kn_data_link_route_hops]
		vp_cpy ML_MSG_DATA + kn_data_link_route_size, qword[r0 + ML_MSG_LENGTH]
		fn_call sys/mail_send

		;open shared memory file
		vp_lea [r14 + ML_MSG_DATA], r0
		sys_open r0, O_CREAT | O_RDWR, S_IRUSR | S_IWUSR
		vp_cpy r0, r13

		;set size of region
		sys_ftruncate r13, lk_buffer_size

		;map shared object
		sys_mmap 0, lk_buffer_size, PROT_READ | PROT_WRITE, MAP_SHARED, r13, 0
		vp_cpy r0, r12

		;r10 is tx channel, r11 is rx channel
		vp_add r12, r10
		vp_add r12, r11

		;clear tx channel
		vp_cpy r10, r0
		vp_cpy lk_chan_size, r1
		fn_call sys/mem_clear

		;read and write messages through the shared buffer in r12
		vp_xor r9, r9
		loop_start
			;exchange task counts
			fn_bind sys/task_statics, r0
			vp_cpy [r0 + TK_STATICS_TASK_COUNT], r0
			vp_cpy r0, [r10 + lk_chan_task_count]
			vp_cpy [r11 + lk_chan_task_count], r0
			vp_cpy r0, [r4 + lk_node_task_count]

			;check if we need to grab a new message
			if r9, ==, 0
			more_output:
				;no outgoing message so see if any off chip mail for me
				vp_cpy [r4 + lk_node_cpu_id], r0
				fn_bind sys/mail_statics, r8
				loop_list_forwards r8 + ML_STATICS_OFFCHIP_LIST, r8, r7
					vp_cpy [r7 + (ML_MSG_DEST + 8)], r2
					breakif r0, ==, r2
					vp_cpy [r4 + lk_node_table + lk_table_array], r1
					continueif r1, ==, 0
					vp_mul lk_route_size, r2
					vp_cpy [r1 + r2 + lk_route_hops], r1
				loop_until r1, !=, 0
				if r8, !=, 0
					vp_cpy r7, r9
					ln_remove_node r7, r1
				endif
			endif

			;if we have a message to send then see if we can send it
			if r9, !=, 0
				vp_cpy [r10 + lk_chan_status], r0
				if r0, ==, lk_chan_status_ready
					;copy message data
					;round up to next 8 byte boundary for speed
					vp_cpy r9, r0
					vp_lea [r10 + lk_chan_msg], r1
					vp_cpy [r9 + ML_MSG_LENGTH], r2
					vp_add 7, r2
					vp_and -8, r2
					fn_call sys/mem_copy

					;free message
					vp_cpy r9, r0
					fn_call sys/mem_free

					;busy status, check for more output
					vp_cpy lk_chan_status_busy, qword[r10 + lk_chan_status]
					vp_xor r9, r9
					vp_jmp more_output
				endif
			endif

			;check for received message
			vp_cpy [r11 + lk_chan_status], r0
			if r0, ==, lk_chan_status_busy
				;allocate msg, copy over data
				;round up to next 8 byte boundary for speed
				fn_call sys/mail_alloc
				vp_cpy r0, r8
				vp_cpy r0, r1
				vp_lea [r11 + lk_chan_msg], r0
				vp_cpy [r0 + ML_MSG_LENGTH], r2
				vp_add 7, r2
				vp_and -8, r2
				fn_call sys/mem_copy

				;send onwards
				vp_cpy r8, r0
				fn_call sys/mail_send

				;clear status
				vp_cpy lk_chan_status_ready, qword[r11 + lk_chan_status]
			endif

			;let other links run
			fn_call sys/task_yield

			;are we in a quite period
			vp_cpy [r11 + lk_chan_status], r0
			continueif r0, ==, lk_chan_status_busy
			vp_cpy 0, r1
			vp_cpy [r10 + lk_chan_status], r0
			if r0, ==, lk_chan_status_ready
				if r9, !=, 0
					vp_cpy 1, r1
				endif
			endif
			continueif r1, !=, 0

			;small sleep if so
			vp_cpy 1000, r0
			fn_call sys/task_sleep

			;exit if signaled by kernel
			vp_cpy [r4 + lk_node_cpu_id], r0
		loop_until r0, ==, -1

		;unmap object
		sys_munmap r12, lk_buffer_size

		;close it
		sys_close r13

		;unlink shared object
		vp_lea [r14 + ML_MSG_DATA], r0
		sys_unlink r0

		;free params msg
		vp_cpy r14, r0
		fn_call sys/mem_free

		;remove from links list and deallocate link node on stack
		vp_cpy [r4 + lk_node_table], r0
		fn_call sys/mem_free
		vp_lea [r4 + lk_node_node], r0
		ln_remove_node r0, r1
		vp_add lk_node_size, r4
		vp_ret

	fn_function_end
