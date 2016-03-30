%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/task.inc'
%include 'inc/link.inc'
%include 'inc/gui.inc'
%include 'inc/load.inc'
%include 'inc/font.inc'
%include 'inc/sdl2.inc'

;;;;;;;;;;;;;
; kernel task
;;;;;;;;;;;;;

	fn_function sys/kernel, no_debug_enter
		;loader is already initialized when we get here !
		;inputs
		;r0 = argv pointer

		;save argv on stack
		vp_push r0

		;init tasks
		static_call sys_task, init

		;init allocator
		static_call sys_mem, init

		;init linker
		static_call sys_link, init

		;init font
		static_call gui_font, init

		;start kernel task
		static_call sys_task, start
		static_bind sys_task, statics, r2
		vp_cpy r0, [r2 + tk_statics_current_tcb]
		vp_cpy r0, [r2 + tk_statics_kernel_tcb]

		;init mailer, r1 = kernel mailbox !
		static_call sys_mail, init

		;process command options
		vp_cpy [r4], r0
		static_call sys_cpu, opts

		;fill in num cpu's with at least mine + 1
		static_call sys_cpu, id
		vp_inc r0
		static_bind sys_task, statics, r1
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
			static_call sys_task, yield

			;service all kernel mail
			loop_start
				;check if any mail
				static_call sys_task, mailbox
				static_call sys_mail, try_read
				breakif r0, ==, 0
				vp_cpy r0, r15

				;switch on kernel call number
				vp_cpy [r15 + kn_data_kernel_function], r1
				switch
				case r1, ==, kn_call_task_open
				run_here:
					;fill in reply ID, user field is left alone !
					vp_cpy [r15 + kn_data_kernel_reply], r1
					vp_cpy [r15 + kn_data_kernel_reply + 8], r2
					vp_cpy r1, [r15 + ml_msg_dest]
					vp_cpy r2, [r15 + (ml_msg_dest + 8)]

					;open single task and return mailbox ID
					vp_lea [r15 + kn_data_task_open_pathname], r0
					static_call sys_load, bind
					static_call sys_task, start
					static_call sys_cpu, id
					vp_cpy r1, [r15 + kn_data_task_open_reply_mailboxid]
					vp_cpy r0, [r15 + kn_data_task_open_reply_mailboxid + 8]
					vp_cpy_cl kn_data_task_open_reply_size, [r15 + ml_msg_length]
					vp_cpy r15, r0
					static_call sys_mail, send
					break
				case r1, ==, kn_call_task_child
					;find best cpu to run task
					static_call sys_cpu, id
					vp_cpy r0, r5
					static_bind sys_task, statics, r1
					vp_cpy [r1 + tk_statics_task_count], r1
					static_bind sys_link, statics, r2
					loop_list_forward r2 + lk_statics_links_list, r3, r2
						if r1, >, [r3 + lk_node_task_count]
							vp_cpy [r3 + lk_node_cpu_id], r0
							vp_cpy [r3 + lk_node_task_count], r1
						endif
					loop_end
					jmpif r0, ==, r5, run_here

					;send to better kernel
					vp_cpy r0, [r15 + (ml_msg_dest + 8)]
					vp_cpy r15, r0
					static_call sys_mail, send
					break
				case r1, ==, kn_call_task_route
					;increase size of network ?
					static_bind sys_task, statics, r0
					vp_cpy [r15 + kn_data_link_route_origin], r1
					vp_inc r1
					if r1, >, [r0 + tk_statics_cpu_total]
						vp_cpy r1, [r0 + tk_statics_cpu_total]
					endif

					;new kernel routing table ?
					vp_cpy [r4 + lk_table_array], r0
					vp_cpy [r4 + lk_table_array_size], r1
					vp_cpy [r15 + kn_data_link_route_origin], r11
					vp_mul lk_route_size, r11
					vp_lea [r11 + lk_route_size], r2
					static_call sys_mem, grow
					vp_cpy r0, [r4 + lk_table_array]
					vp_cpy r1, [r4 + lk_table_array_size]

					;compare hop counts
					vp_cpy [r15 + kn_data_link_route_hops], r2
					vp_cpy [r0 + r11 + lk_route_hops], r3
					switch
					case r3, ==, 0
						;never seen, so better route
						vp_jmp better_route
					case r2, <, r3
					better_route:
						;new hops is less, so better route
						vp_cpy r2, [r0 + r11]

						;fill in via route and remove other routes
						vp_cpy [r15 + kn_data_link_route_via], r13
						static_bind sys_link, statics, r14
						loop_list_forward r14 + lk_statics_links_list, r12, r14
							;new link route table ?
							vp_cpy [r12 + lk_node_table + lk_table_array], r0
							vp_cpy [r12 + lk_node_table + lk_table_array_size], r1
							vp_lea [r11 + lk_route_size], r2
							static_call sys_mem, grow
							vp_cpy r0, [r12 + lk_node_table + lk_table_array]
							vp_cpy r1, [r12 + lk_node_table + lk_table_array_size]

							if [r12 + lk_node_cpu_id], ==, r13
								;via route
								vp_cpy [r15 + kn_data_link_route_hops], r2
								vp_cpy r2, [r0 + r11 + lk_route_hops]
							else
								;none via route
								vp_cpy_cl 0, [r0 + r11 + lk_route_hops]
							endif
						loop_end
						break
					case r2, ==, r3
						;new hops is equal, so additional route
						vp_cpy [r15 + kn_data_link_route_via], r13
						static_bind sys_link, statics, r14
						loop_list_forward r14 + lk_statics_links_list, r12, r14
							;new link route table ?
							vp_cpy [r12 + lk_node_table + lk_table_array], r0
							vp_cpy [r12 + lk_node_table + lk_table_array_size], r1
							vp_lea [r11 + lk_route_size], r2
							static_call sys_mem, grow
							vp_cpy r0, [r12 + lk_node_table + lk_table_array]
							vp_cpy r1, [r12 + lk_node_table + lk_table_array_size]

							if [r12 + lk_node_cpu_id], ==, r13
								;via route
								vp_cpy [r15 + kn_data_link_route_hops], r2
								vp_cpy r2, [r0 + r11 + lk_route_hops]
							endif
						loop_end
						;drop through to discard message !
					default
						;new hops is greater, so worse route
						vp_jmp drop_msg
					endswitch

					;increment hop count
					vp_cpy [r15 + kn_data_link_route_hops], r1
					vp_inc r1
					vp_cpy r1, [r15 + kn_data_link_route_hops]

					;get current via, set via to my cpu id
					vp_cpy [r15 + kn_data_link_route_via], r14
					static_call sys_cpu, id
					vp_cpy r0, [r15 + kn_data_link_route_via]

					;copy and send to all neighbors apart from old via
					static_bind sys_link, statics, r13
					loop_list_forward r13 + lk_statics_links_list, r12, r13
						vp_cpy [r12 + lk_node_cpu_id], r11
						continueif r11, ==, r14
						static_call sys_mail, alloc
						fn_assert r0, !=, 0
						vp_cpy r0, r5
						vp_cpy r0, r1
						vp_cpy r15, r0
						vp_cpy [r15 + ml_msg_length], r2
						vp_add 7, r2
						vp_and -8, r2
						static_call sys_mem, copy
						vp_cpy r11, [r5 + ml_msg_dest + 8]
						vp_cpy r5, r0
						static_call sys_mail, send
					loop_end
				drop_msg:
					vp_cpy r15, r0
					static_call sys_mem, free
					break
				case r1, ==, kn_call_callback
					;call callback with this thread/stack
					vp_push r15
					vp_cpy [r15 + kn_data_kernel_user], r0
					vp_call [r15 + kn_data_callback_addr]

					;reply to originator
					vp_pop r0
					vp_cpy [r0 + kn_data_kernel_reply], r1
					vp_cpy [r0 + kn_data_kernel_reply + 8], r2
					vp_cpy r1, [r0 + ml_msg_dest]
					vp_cpy r2, [r0 + ml_msg_dest + 8]
					static_call sys_mail, send
					break
				default
				endswitch
			loop_end

			;get time
			static_call sys_cpu, time

			;start any tasks ready to restart
			static_bind sys_task, statics, r3
			vp_cpy [r3 + tk_statics_current_tcb], r15
			vp_cpy [r3 + tk_statics_timer_list + lh_list_head], r2
			ln_get_succ r2, r2
			if r2, !=, 0
				loop_list_forward r3 + tk_statics_timer_list, r1, r2
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
			ln_get_succ r2, r1
			breakif r1, ==, 0

			;sleep till next wake time
			vp_xchg r0, r2
			vp_cpy [r0 + tk_node_time], r0
			vp_sub r2, r0
			vp_cpy 1000, r3
			vp_xor r2, r2
			vp_div r3, r2, r0
			if r0, <, 1
				vp_cpy 1, r0
			endif
			sdl_delay r0
		loop_end

		;deinit font
		static_call gui_font, deinit

		;deinit gui
		static_call gui_gui, deinit

		;free any kernel routing table
		vp_cpy [r4 + lk_table_array], r0
		static_call sys_mem, free
		vp_add lk_table_size, r4

		;deinit allocator
		static_call sys_mem, deinit

		;deinit mailer
		static_call sys_mail, deinit

		;deinit tasks
		static_call sys_task, deinit

		;deinit loader
		static_call sys_load, deinit

		;pop argv and exit !
		vp_add 8, r4
		sys_exit 0

	fn_function_end
