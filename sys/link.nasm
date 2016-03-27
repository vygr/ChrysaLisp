%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/task.inc'
%include 'inc/link.inc'

	fn_function sys/link, no_debug_enter
		;started by kernel for each link

		;allocate link node on stack and link to links list
		vp_sub lk_node_size, r4
		vp_xor r1, r1
		vp_cpy r1, [r4 + lk_node_table + lk_table_array]
		vp_cpy r1, [r4 + lk_node_table + lk_table_array_size]
		vp_lea [r4 + lk_node_node], r0
		static_bind sys_link, statics, r1
		vp_lea [r1 + lk_statics_links_list], r1
		lh_add_at_head r1, r0, r2

		;read params msg from kernel
		static_call sys_mail, mymail
		vp_cpy r0, r14

		;init link node cpu id and task count
		vp_cpy_b [r14 + ml_msg_data + 5], r0
		vp_cpy_b [r14 + ml_msg_data + 6], r1
		vp_cpy_b [r14 + ml_msg_data + 7], r2
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
		vp_cpy_b [r14 + ml_msg_data + 9], r0
		vp_cpy_b [r14 + ml_msg_data + 10], r2
		vp_cpy_b [r14 + ml_msg_data + 11], r3
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
		static_call sys_cpu, id
		if r1, ==, r0
			vp_cpy r2, r1
			vp_cpy lk_buffer_chan_1, r10
			vp_cpy lk_buffer_chan_2, r11
		else
			vp_cpy lk_buffer_chan_2, r10
			vp_cpy lk_buffer_chan_1, r11
		endif
		vp_cpy r1, [r4 + lk_node_cpu_id]
		vp_cpy_cl 0, [r4 + lk_node_task_count]

		;send link routing message to neighbor kernel
		vp_cpy r0, r8
		vp_cpy r1, r9
		static_call sys_mail, alloc
		fn_assert r0, !=, 0
		vp_xor r1, r1
		vp_cpy r1, [r0 + ml_msg_dest]
		vp_cpy r9, [r0 + (ml_msg_dest + 8)]
		vp_cpy r1, [r0 + ml_msg_data + kn_data_kernel_user]
		vp_cpy r1, [r0 + ml_msg_data + kn_data_kernel_reply]
		vp_cpy r1, [r0 + (ml_msg_data + kn_data_kernel_reply + 8)]
		vp_cpy_cl kn_call_task_route, [r0 + ml_msg_data + kn_data_kernel_function]
		vp_cpy r8, [r0 + ml_msg_data + kn_data_link_route_origin]
		vp_cpy r8, [r0 + ml_msg_data + kn_data_link_route_via]
		vp_cpy_cl 1, [r0 + ml_msg_data + kn_data_link_route_hops]
		vp_cpy_cl ml_msg_data + kn_data_link_route_size, [r0 + ml_msg_length]
		static_call sys_mail, send

		;open shared memory file
		vp_lea [r14 + ml_msg_data], r0
		sys_open r0, o_creat | o_rdwr, s_irusr | s_iwusr
		vp_cpy r0, r13

		;set size of region
		sys_ftruncate r13, lk_buffer_size

		;map shared object
		sys_mmap 0, lk_buffer_size, prot_read | prot_write, map_shared, r13, 0
		vp_cpy r0, r12
		fn_assert r0, !=, 0

		;r10 is tx channel, r11 is rx channel
		vp_add r12, r10
		vp_add r12, r11

		;clear tx channel
		vp_cpy r10, r0
		vp_cpy lk_chan_size, r1
		static_call sys_mem, clear

		;read and write messages through the shared buffer in r12
		vp_xor r9, r9
		loop_start
			;exchange task counts
			static_bind sys_task, statics, r0
			vp_cpy [r0 + tk_statics_task_count], r0
			vp_cpy r0, [r10 + lk_chan_task_count]
			vp_cpy [r11 + lk_chan_task_count], r0
			vp_cpy r0, [r4 + lk_node_task_count]

			;check if we need to grab a new message
			if r9, ==, 0
			more_output:
				;no outgoing message so see if any off chip mail for me
				vp_cpy [r4 + lk_node_cpu_id], r0
				static_bind sys_mail, statics, r8
				loop_list_forward r8 + ml_statics_offchip_list, r8, r7
					vp_cpy [r7 + (ml_msg_dest + 8)], r2
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
					vp_cpy [r9 + ml_msg_length], r2
					vp_add 7, r2
					vp_and -8, r2
					static_call sys_mem, copy

					;free message
					vp_cpy r9, r0
					static_call sys_mem, free

					;busy status, check for more output
					vp_cpy_cl lk_chan_status_busy, [r10 + lk_chan_status]
					vp_xor r9, r9
					vp_jmp more_output
				endif
			endif

			;check for received message
			vp_cpy [r11 + lk_chan_status], r0
			if r0, ==, lk_chan_status_busy
				;allocate msg, copy over data
				;round up to next 8 byte boundary for speed
				static_call sys_mail, alloc
				fn_assert r0, !=, 0
				vp_cpy r0, r8
				vp_cpy r0, r1
				vp_lea [r11 + lk_chan_msg], r0
				vp_cpy [r0 + ml_msg_length], r2
				vp_add 7, r2
				vp_and -8, r2
				static_call sys_mem, copy

				;send onwards
				vp_cpy r8, r0
				static_call sys_mail, send

				;clear status
				vp_cpy_cl lk_chan_status_ready, [r11 + lk_chan_status]
			endif

			;let other links run
			static_call sys_task, yield

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
			static_call sys_task, sleep

			;exit if signaled by kernel
			vp_cpy [r4 + lk_node_cpu_id], r0
		loop_until r0, ==, -1

		;unmap object
		sys_munmap r12, lk_buffer_size

		;close it
		sys_close r13

		;unlink shared object
		vp_lea [r14 + ml_msg_data], r0
		sys_unlink r0

		;free params msg
		vp_cpy r14, r0
		static_call sys_mem, free

		;remove from links list and deallocate link node on stack
		vp_cpy [r4 + lk_node_table], r0
		static_call sys_mem, free
		vp_lea [r4 + lk_node_node], r0
		ln_remove_node r0, r1
		vp_add lk_node_size, r4
		vp_ret

	fn_function_end
