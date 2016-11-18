%include 'inc/func.ninc'
%include 'inc/mail.ninc'
%include 'inc/task.ninc'
%include 'inc/link.ninc'
%include 'inc/load.ninc'
%include 'inc/font.ninc'
%include 'inc/sdl2.ninc'

;;;;;;;;;;;;;
; kernel task
;;;;;;;;;;;;;

def_func sys/kernel
	;loader is already initialized when we get here !
	;inputs
	;r0 = argv pointer

	;save argv on stack
	vp_push r0

	;init allocator
	f_call sys_mem, init

	;init tasks
	f_call sys_task, init

	;init linker
	f_call sys_link, init

	;init font
	f_call gui_font, init

	;start kernel task
	f_call sys_task, start, {$_func_entry}, {r0, r1}
	f_bind sys_task, statics, r2
	vp_cpy r0, [r2 + tk_statics_current_tcb]
	vp_cpy r0, [r2 + tk_statics_kernel_tcb]

	;init mailer, r1 = kernel mailbox !
	f_call sys_mail, init

	;process command options
	f_call sys_cpu, opts, {[r4]}

	;fill in num cpu's with at least mine + 1
	f_call sys_cpu, id, {}, {r0}
	vp_inc r0
	f_bind sys_task, statics, r1
	vp_cpy r0, [r1 + tk_statics_cpu_total]

	;allocate for kernel routing table
	vp_sub lk_table_size, r4
	vp_xor r1, r1
	vp_cpy r1, [r4 + lk_table_array]
	vp_cpy r1, [r4 + lk_table_array_size]

;;;;;;;;;;;;;;;;;;;;;;;
; main kernel task loop
;;;;;;;;;;;;;;;;;;;;;;;

	;loop till no other tasks running
	loop_start
		;allow all other tasks to run
		f_call sys_task, yield

		;service all kernel mail
		loop_start
			;check if any mail
			f_call sys_task, mailbox, {}, {r0, r1}
			f_call sys_mail, try_read, {r0}, {r0}
			breakif r0, ==, 0
			vp_cpy r0, r15

			;switch on kernel call number
			vp_cpy [r15 + kn_msg_function], r1
			switch
			case r1, ==, kn_call_task_open
			run_here:
				;fill in reply ID, user field is left alone !
				vp_cpy [r15 + kn_msg_reply_id + id_mbox], r1
				vp_cpy [r15 + kn_msg_reply_id + id_cpu], r2
				vp_cpy r1, [r15 + msg_dest + id_mbox]
				vp_cpy r2, [r15 + msg_dest + id_cpu]

				;open single task and return mailbox ID
				f_call sys_load, bind, {&[r15 + kn_msg_open_pathname]}, {r0}
				vpif r0, ==, 0
					;error
					vp_cpy r0, r1
				else
					;start this function as task
					f_call sys_task, start, {r0}, {_, r1}
					f_call sys_cpu, id, {}, {r0}
				endif
				vp_cpy r1, [r15 + kn_msg_reply_id + id_mbox]
				vp_cpy r0, [r15 + kn_msg_reply_id + id_cpu]
				vp_cpy kn_msg_reply_size, r0
				vp_cpy r0, [r15 + msg_length]
				f_call sys_mail, send, {r15}
				break
			case r1, ==, kn_call_task_child
				;find best cpu to run task
				f_call sys_cpu, id, {}, {r5}
				f_bind sys_task, statics, r1
				vp_cpy [r1 + tk_statics_task_count], r1
				f_bind sys_link, statics, r2
				loop_list_forward r2, lk_statics_links_list, r3, r2
					vpif r1, >, [r3 + lk_node_task_count]
						vp_cpy [r3 + lk_node_cpu_id], r0
						vp_cpy [r3 + lk_node_task_count], r1
					endif
				loop_end
				vp_jmpif r0, ==, r5, run_here

				;send to better kernel
				vp_cpy r0, [r15 + msg_dest + id_cpu]
				f_call sys_mail, send, {r15}
				break
			case r1, ==, kn_call_task_route
				;increase size of network ?
				f_bind sys_task, statics, r0
				vp_cpy [r15 + kn_msg_link_route_origin], r1
				vp_inc r1
				vpif r1, >, [r0 + tk_statics_cpu_total]
					vp_cpy r1, [r0 + tk_statics_cpu_total]
				endif

				;new kernel routing table ?
				vp_cpy [r15 + kn_msg_link_route_origin], r11
				vp_mul lk_route_size, r11
				f_call sys_mem, grow, {[r4 + lk_table_array], [r4 + lk_table_array_size], &[r11 + lk_route_size]}, \
											{[r4 + lk_table_array], [r4 + lk_table_array_size]}

				;compare hop counts
				vp_add lk_route_hops, r0
				vp_cpy [r15 + kn_msg_link_route_hops], r2
				vp_cpy [r0 + r11], r3
				switch
				case r3, ==, 0
					;never seen, so better route
					vp_jmp better_route
				case r2, <, r3
				better_route:
					;new hops is less, so better route
					vp_cpy r2, [r0 + r11]

					;fill in via route and remove other routes
					vp_cpy [r15 + kn_msg_link_route_via], r13
					f_bind sys_link, statics, r14
					loop_list_forward r14, lk_statics_links_list, r12, r14
						;new link route table ?
						f_call sys_mem, grow, {[r12 + lk_node_table + lk_table_array], [r12 + lk_node_table + lk_table_array_size], &[r11 + lk_route_size]}, \
													{[r12 + lk_node_table + lk_table_array], [r12 + lk_node_table + lk_table_array_size]}

						vp_add lk_route_hops, r0
						vpif [r12 + lk_node_cpu_id], ==, r13
							;via route
							vp_cpy [r15 + kn_msg_link_route_hops], r2
							vp_cpy r2, [r0 + r11]
						else
							;none via route
							vp_xor r1,r1
							vp_cpy r1, [r0 + r11]
						endif
					loop_end
					break
				case r2, ==, r3
					;new hops is equal, so additional route
					vp_cpy [r15 + kn_msg_link_route_via], r13
					f_bind sys_link, statics, r14
					loop_list_forward r14, lk_statics_links_list, r12, r14
						;new link route table ?
						f_call sys_mem, grow, {[r12 + lk_node_table + lk_table_array], [r12 + lk_node_table + lk_table_array_size], &[r11 + lk_route_size]}, \
													{[r12 + lk_node_table + lk_table_array], [r12 + lk_node_table + lk_table_array_size]}

						vpif [r12 + lk_node_cpu_id], ==, r13
							;via route
							vp_cpy [r15 + kn_msg_link_route_hops], r2
							vp_add lk_route_hops, r0
							vp_cpy r2, [r0 + r11]
						endif
					loop_end
					;drop through to discard message !
				default
					;new hops is greater, so worse route
					vp_jmp drop_msg
				endswitch

				;increment hop count
				vp_cpy [r15 + kn_msg_link_route_hops], r1
				vp_inc r1
				vp_cpy r1, [r15 + kn_msg_link_route_hops]

				;get current via, set via to my cpu id
				vp_cpy [r15 + kn_msg_link_route_via], r14
				f_call sys_cpu, id, {}, {[r15 + kn_msg_link_route_via]}

				;copy and send to all neighbors apart from old via
				f_bind sys_link, statics, r13
				loop_list_forward r13, lk_statics_links_list, r12, r13
					vp_cpy [r12 + lk_node_cpu_id], r11
					continueif r11, ==, r14
					f_call sys_mail, alloc, {}, {r0}
					assert r0, !=, 0
					vp_cpy r0, r5
					vp_cpy r0, r1
					vp_cpy r15, r0
					vp_cpy [r15 + msg_length], r2
					vp_add ptr_size - 1, r2
					vp_and -ptr_size, r2
					f_call sys_mem, copy, {r0, r1, r2}, {_, _}
					vp_cpy r11, [r5 + msg_dest + id_cpu]
					f_call sys_mail, send, {r5}
				loop_end
			drop_msg:
				f_call sys_mem, free, {r15}
				break
			case r1, ==, kn_call_callback
				;call callback with this thread/stack
				vp_push r15
				vp_cpy [r15 + kn_msg_user], r0
				vp_call [r15 + kn_msg_callback_addr]

				;reply to originator
				vp_pop r0
				vp_cpy [r0 + kn_msg_reply_id + id_mbox], r1
				vp_cpy [r0 + kn_msg_reply_id + id_cpu], r2
				vp_cpy r1, [r0 + msg_dest + id_mbox]
				vp_cpy r2, [r0 + msg_dest + id_cpu]
				f_call sys_mail, send, {r0}
			endswitch
		loop_end

		;get time
		f_call sys_cpu, time, {}, {r0}

		;start any tasks ready to restart
		f_bind sys_task, statics, r3
		vp_cpy [r3 + tk_statics_current_tcb], r15
		vp_cpy [r3 + tk_statics_timer_list + lh_list_head], r2
		ln_get_succ r2, 0, r2
		vpif r2, !=, 0
			loop_list_forward r3, tk_statics_timer_list, r1, r2
				vp_cpy [r1 + tk_node_time], r5
				breakif r5, >, r0

				;task ready, remove from timer list and place on ready list
				vp_cpy r1, r5
				ln_remove_node r5, r6
				ln_add_node_before r15, r1, r6
			loop_end
		endif

		;next task if other ready tasks
		vp_cpy [r3 + tk_statics_task_list + lh_list_head], r2
		vp_cpy [r3 + tk_statics_task_list + lh_list_tailpred], r1
		continueif r2, !=, r1

		;exit if no task waiting for timer
		vp_cpy [r3 + tk_statics_timer_list + lh_list_head], r2
		ln_get_succ r2, 0, r1
		breakif r1, ==, 0

		;sleep till next wake time
		vp_xchg r0, r2
		vp_cpy [r0 + tk_node_time], r0
		vp_sub r2, r0
		vp_cpy 1000, r3
		vp_xor r2, r2
		vp_div r3, r2, r0
		vpif r0, <, 1
			vp_cpy 1, r0
		endif
		sdl_delay r0
	loop_end

	;free any kernel routing table
	f_call sys_mem, free, {[r4 + lk_table_array]}
	vp_add lk_table_size, r4

	;deinit font
	f_call gui_font, deinit

	;deinit mailer
	f_call sys_mail, deinit

	;deinit tasks
	f_call sys_task, deinit

	;deinit allocator
	f_call sys_mem, deinit

	;deinit loader
	f_call sys_load, deinit

	;pop argv and exit !
	vp_add ptr_size, r4
	sys_exit 0

def_func_end
