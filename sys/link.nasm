%include 'inc/func.ninc'
%include 'inc/mail.ninc'
%include 'inc/task.ninc'
%include 'inc/link.ninc'

def_func sys/link
	;started by kernel for each link

	;decrement task count
	;don't count links in load balencing
	f_bind sys_task, statics, r0
	vp_cpy [r0 + tk_statics_task_count], r1
	vp_dec r1
	vp_cpy r1, [r0 + tk_statics_task_count]

	;allocate link node on stack and link to links list
	vp_sub lk_node_size, r4
	vp_xor r1, r1
	vp_cpy r1, [r4 + lk_node_table + lk_table_array]
	vp_cpy r1, [r4 + lk_node_table + lk_table_array_size]
	f_bind sys_link, statics, r1
	vp_lea [r1 + lk_statics_links_list], r1
	lh_add_at_head r1, r4, r2

	;read params msg from kernel
	f_call sys_mail, mymail, {}, {r14}

	;init link node cpu id and task count
	vp_cpy_ub [r14 + msg_data + 5], r0
	vp_cpy_ub [r14 + msg_data + 6], r1
	vp_cpy_ub [r14 + msg_data + 7], r2
	vp_sub '0', r0
	vp_sub '0', r1
	vp_sub '0', r2
	vp_mul 100, r0
	vp_mul 10, r1
	vp_add r0, r1
	vp_add r2, r1
	vp_cpy_ub [r14 + msg_data + 9], r0
	vp_cpy_ub [r14 + msg_data + 10], r2
	vp_cpy_ub [r14 + msg_data + 11], r3
	vp_sub '0', r0
	vp_sub '0', r2
	vp_sub '0', r3
	vp_mul 100, r0
	vp_mul 10, r2
	vp_add r0, r2
	vp_add r3, r2
	f_call sys_cpu, id, {}, {r0}
	vpif r1, ==, r0
		vp_cpy r2, r1
		vp_cpy lk_buffer_chan_1, r10
		vp_cpy lk_buffer_chan_2, r11
	else
		vp_cpy lk_buffer_chan_2, r10
		vp_cpy lk_buffer_chan_1, r11
	endif
	vp_cpy r1, [r4 + lk_node_cpu_id]
	vp_xor r8, r8
	vp_cpy r8, [r4 + lk_node_task_count]

	;send link routing message to neighbor kernel
	vp_cpy r0, r8
	vp_cpy r1, r9
	f_call sys_mail, alloc, {}, {r0}
	assert r0, !=, 0
	vp_xor r1, r1
	vp_cpy r1, [r0 + msg_dest + id_mbox]
	vp_cpy r9, [r0 + msg_dest + id_cpu]
	vp_cpy r1, [r0 + kn_msg_user]
	vp_cpy r1, [r0 + kn_msg_reply_id + id_mbox]
	vp_cpy r1, [r0 + kn_msg_reply_id + id_cpu]
	vp_cpy kn_call_task_route, r1
	vp_cpy r1, [r0 + kn_msg_function]
	vp_cpy r8, [r0 + kn_msg_link_route_origin]
	vp_cpy r8, [r0 + kn_msg_link_route_via]
	vp_cpy 1, r1
	vp_cpy r1, [r0 + kn_msg_link_route_hops]
	vp_cpy kn_msg_link_route_size, r1
	vp_cpy r1, [r0 + msg_length]
	f_call sys_mail, send, {r0}

	;open shared memory file
	vp_lea [r14 + msg_data], r0
	sys_open r0, o_creat | o_rdwr, s_irusr | s_iwusr
	vp_cpy r0, r13

	;set size of region
	sys_ftruncate r13, lk_buffer_size

	;map shared object
	sys_mmap 0, lk_buffer_size, prot_read | prot_write, map_shared, r13, 0
	vp_cpy r0, r12
	assert r0, !=, 0

	;r10 is tx channel, r11 is rx channel
	vp_add r12, r10
	vp_add r12, r11

	;clear tx channel
	f_call sys_mem, clear, {r10, lk_chan_size}, {_}

	;read and write messages through the shared buffer in r12
	vp_xor r9, r9
	loop_start
		;exchange task counts
		f_bind sys_task, statics, r0
		vp_cpy [r0 + tk_statics_task_count], r0
		vp_cpy r0, [r10 + lk_chan_task_count]
		vp_cpy [r11 + lk_chan_task_count], r0
		vp_cpy r0, [r4 + lk_node_task_count]

		;check if we need to grab a new message
		vpif r9, ==, 0
		more_output:
			;no outgoing message so see if any off chip mail for me
			vp_cpy [r4 + lk_node_cpu_id], r0
			f_bind sys_mail, statics, r8
			loop_list_forward r8 + ml_statics_offchip_list, r7, r8
				vp_cpy [r7 + msg_dest + id_cpu], r2
				breakif r0, ==, r2
				vp_cpy [r4 + lk_node_table + lk_table_array], r1
				continueif r1, ==, 0
				vp_mul lk_route_size, r2
				vp_add lk_route_hops, r1
				vp_cpy [r1 + r2], r1
			loop_until r1, !=, 0
			vpif r8, !=, 0
				vp_cpy r7, r9
				ln_remove_node r7, r1
			endif
		endif

		;if we have a message to send then see if we can send it
		vpif r9, !=, 0
			vp_cpy [r10 + lk_chan_status], r0
			vpif r0, ==, lk_chan_status_ready
				;copy message data
				;round up to next 8 byte boundary for speed
				vp_lea [r10 + lk_chan_msg], r1
				vp_cpy [r9 + msg_length], r2
				vp_add ptr_size - 1, r2
				vp_and -ptr_size, r2
				f_call sys_mem, copy, {r9, r1, r2}, {_, _}

				;free message
				f_call sys_mem, free, {r9}

				;busy status, check for more output
				vp_cpy lk_chan_status_busy, r1
				vp_cpy r1, [r10 + lk_chan_status]
				vp_xor r9, r9
				vp_jmp more_output
			endif
		endif

		;check for received message
		vp_cpy [r11 + lk_chan_status], r0
		vpif r0, ==, lk_chan_status_busy
			;allocate msg, copy over data
			;round up to next 8 byte boundary for speed
			f_call sys_mail, alloc, {}, {r0}
			assert r0, !=, 0
			vp_cpy r0, r8
			vp_cpy r0, r1
			vp_lea [r11 + lk_chan_msg], r0
			vp_cpy [r0 + msg_length], r2
			vp_add ptr_size - 1, r2
			vp_and -ptr_size, r2
			f_call sys_mem, copy, {r0, r1, r2}, {_, _}

			;send onwards
			f_call sys_mail, send, {r8}

			;clear status
			vp_cpy lk_chan_status_ready, r1
			vp_cpy r1, [r11 + lk_chan_status]
		endif

		;let other links run
		f_call sys_task, yield

		;are we in a quite period
		vp_cpy [r11 + lk_chan_status], r0
		continueif r0, ==, lk_chan_status_busy
		vp_cpy 0, r1
		vp_cpy [r10 + lk_chan_status], r0
		vpif r0, ==, lk_chan_status_ready
			vpif r9, !=, 0
				vp_cpy 1, r1
			endif
		endif
		continueif r1, !=, 0

		;small sleep if so
		f_call sys_task, sleep, {1000}

		;exit if signaled by kernel
		vp_cpy [r4 + lk_node_cpu_id], r0
	loop_until r0, ==, -1

	;unmap object
	sys_munmap r12, lk_buffer_size

	;close it
	sys_close r13

	;unlink shared object
	vp_lea [r14 + msg_data], r0
	sys_unlink r0

	;free params msg
	f_call sys_mem, free, {r14}

	;remove from links list and deallocate link node on stack
	f_call sys_mem, free, {[r4 + lk_node_table]}
	vp_cpy r4, r0
	ln_remove_node r0, r1
	vp_add lk_node_size, r4

	;restore task count
	f_bind sys_task, statics, r0
	vp_cpy [r0 + tk_statics_task_count], r1
	vp_inc r1
	vp_cpy r1, [r0 + tk_statics_task_count]
	vp_ret

def_func_end
