%include "func.inc"
%include "mail.inc"
%include "task.inc"
%include "link.inc"
%include "gui.inc"
%include "sdl2.inc"

;;;;;;;;;;;;;
; kernel task
;;;;;;;;;;;;;

	fn_function "sys/kernel"
		;loader is already initialized when we get here !
		;inputs
		;r0 = argv pointer

		;save argv on stack
		vp_push r0

		;init tasker
		fn_call sys/task_init_tasker

		;init allocator
		fn_call sys/mem_init_allocator

		;init linker
		fn_call sys/link_init_linker

		;start kernel task
		fn_call sys/task_start
		fn_bind sys/task_statics, r2
		vp_cpy r1, [r2 + TK_STATICS_CURRENT_TCB]
		vp_cpy r1, r15

		;init mailer, r0 = kernel mailbox !
		fn_call sys/mail_init_mailer

		;process command options
		vp_cpy [r4], r0
		fn_call sys/opt_process

		;fill in num cpu's with at least mine + 1
		fn_call sys/get_cpu_id
		vp_inc r0
		fn_bind sys/task_statics, r1
		vp_cpy r0, [r1 + TK_STATICS_CPU_TOTAL]

		;allocate for kernel routing table
		vp_sub LK_TABLE_SIZE, r4
		vp_cpy 0, qword[r4 + LK_TABLE_ARRAY]
		vp_cpy 0, qword[r4 + LK_TABLE_ARRAY_SIZE]

;;;;;;;;;;;;;;;;;;;;;;;
; main kernel task loop
;;;;;;;;;;;;;;;;;;;;;;;

		;loop till no other tasks running
		loopstart
			;allow all other tasks to run
			fn_call sys/task_deshedule

			;service all kernel mail
			loopstart
				;check if any mail
				vp_lea [r15 + TK_NODE_MAILBOX], r0
				ml_check r0, r1
				breakif r1, ==, 0

				;read waiting mail
				fn_call sys/mail_read
				vp_cpy r1, r14

				;switch on kernel call number
				vp_cpy [r14 + (ML_MSG_DATA + KN_DATA_KERNEL_FUNCTION)], r1
				switch
				case r1, ==, KN_CALL_TASK_OPEN
				run_here:
					;fill in reply ID, user field is left alone !
					vp_cpy [r14 + (ML_MSG_DATA + KN_DATA_KERNEL_REPLY)], r1
					vp_cpy [r14 + (ML_MSG_DATA + KN_DATA_KERNEL_REPLY + 8)], r2
					vp_cpy r1, [r14 + ML_MSG_DEST]
					vp_cpy r2, [r14 + (ML_MSG_DEST + 8)]

					;open single task and return mailbox ID
					vp_lea [r14 + (ML_MSG_DATA + KN_DATA_TASK_OPEN_PATHNAME)], r0
					fn_call sys/load_function_load
					fn_call sys/task_start
					vp_cpy r0, [r14 + (ML_MSG_DATA + KN_DATA_TASK_OPEN_REPLY_MAILBOXID)]
					fn_call sys/get_cpu_id
					vp_cpy r0, [r14 + (ML_MSG_DATA + KN_DATA_TASK_OPEN_REPLY_MAILBOXID + 8)]
					vp_cpy ML_MSG_DATA + KN_DATA_TASK_OPEN_REPLY_SIZE, qword[r14 + ML_MSG_LENGTH]
					vp_cpy r14, r0
					fn_call sys/mail_send
					break
				case r1, ==, KN_CALL_TASK_CHILD
					;find best cpu to run task
					fn_call sys/get_cpu_id
					vp_cpy r0, r5
					fn_bind sys/task_statics, r1
					vp_cpy [r1 + TK_STATICS_TASK_COUNT], r1
					fn_bind sys/link_statics, r2
					loopstart_list_forwards r2 + LK_STATICS_LINKS_LIST, r2, r3
						if r1, >, [r3 + LK_NODE_TASK_COUNT]
							vp_cpy [r3 + LK_NODE_CPU_ID], r0
							vp_cpy [r3 + LK_NODE_TASK_COUNT], r1
						endif
					loopend
					jmpif r0, ==, r5, run_here

					;send to better kernel
					vp_cpy r0, [r14 + (ML_MSG_DEST + 8)]
					vp_cpy r14, r0
					fn_call sys/mail_send
					break
				case r1, ==, KN_CALL_LINK_ROUTE
					;increase size of network ?
					fn_bind sys/task_statics, r0
					vp_cpy [r14 + ML_MSG_DATA + KN_DATA_LINK_ROUTE_ORIGIN], r1
					vp_inc r1
					if r1, >, [r0 + TK_STATICS_CPU_TOTAL]
						vp_cpy r1, [r0 + TK_STATICS_CPU_TOTAL]
					endif

					;new kernel routing table ?
					vp_cpy [r4 + LK_TABLE_ARRAY], r0
					vp_cpy [r4 + LK_TABLE_ARRAY_SIZE], r1
					vp_cpy [r14 + ML_MSG_DATA + KN_DATA_LINK_ROUTE_ORIGIN], r10
					vp_mul LK_ROUTE_SIZE, r10
					vp_lea [r10 + LK_ROUTE_SIZE], r2
					fn_call sys/mem_grow
					vp_cpy r0, [r4 + LK_TABLE_ARRAY]
					vp_cpy r1, [r4 + LK_TABLE_ARRAY_SIZE]

					;compare hop counts
					vp_cpy [r14 + ML_MSG_DATA + KN_DATA_LINK_ROUTE_HOPS], r2
					vp_cpy [r0 + r10 + LK_ROUTE_HOPS], r3
					switch
					case r3, ==, 0
						;never seen, so better route
						vp_jmp better_route
					case r2, <, r3
					better_route:
						;new hops is less, so better route
						vp_cpy r2, [r0 + r10]

						;fill in via route and remove other routes
						vp_cpy [r14 + ML_MSG_DATA + KN_DATA_LINK_ROUTE_VIA], r12
						fn_bind sys/link_statics, r13
						loopstart_list_forwards r13 + LK_STATICS_LINKS_LIST, r13, r11
							;new link route table ?
							vp_cpy [r11 + LK_NODE_TABLE + LK_TABLE_ARRAY], r0
							vp_cpy [r11 + LK_NODE_TABLE + LK_TABLE_ARRAY_SIZE], r1
							vp_lea [r10 + LK_ROUTE_SIZE], r2
							fn_call sys/mem_grow
							vp_cpy r0, [r11 + LK_NODE_TABLE + LK_TABLE_ARRAY]
							vp_cpy r1, [r11 + LK_NODE_TABLE + LK_TABLE_ARRAY_SIZE]

							if [r11 + LK_NODE_CPU_ID], ==, r12
								;via route
								vp_cpy [r14 + ML_MSG_DATA + KN_DATA_LINK_ROUTE_HOPS], r2
								vp_cpy r2, [r0 + r10 + LK_ROUTE_HOPS]
							else
								;none via route
								vp_cpy 0, qword[r0 + r10 + LK_ROUTE_HOPS]
							endif
						loopend
						break
					case r2, ==, r3
						;new hops is equal, so additional route
						vp_cpy [r14 + ML_MSG_DATA + KN_DATA_LINK_ROUTE_VIA], r12
						fn_bind sys/link_statics, r13
						loopstart_list_forwards r13 + LK_STATICS_LINKS_LIST, r13, r11
							;new link route table ?
							vp_cpy [r11 + LK_NODE_TABLE + LK_TABLE_ARRAY], r0
							vp_cpy [r11 + LK_NODE_TABLE + LK_TABLE_ARRAY_SIZE], r1
							vp_lea [r10 + LK_ROUTE_SIZE], r2
							fn_call sys/mem_grow
							vp_cpy r0, [r11 + LK_NODE_TABLE + LK_TABLE_ARRAY]
							vp_cpy r1, [r11 + LK_NODE_TABLE + LK_TABLE_ARRAY_SIZE]

							if [r11 + LK_NODE_CPU_ID], ==, r12
								;via route
								vp_cpy [r14 + ML_MSG_DATA + KN_DATA_LINK_ROUTE_HOPS], r2
								vp_cpy r2, [r0 + r10 + LK_ROUTE_HOPS]
							endif
						loopend
						;drop through to discard message !
					default
						;new hops is greater, so worse route
						vp_jmp drop_msg
					endswitch

					;increment hop count
					vp_cpy [r14 + ML_MSG_DATA + KN_DATA_LINK_ROUTE_HOPS], r1
					vp_inc r1
					vp_cpy r1, [r14 + ML_MSG_DATA + KN_DATA_LINK_ROUTE_HOPS]

					;get current via, set via to my cpu id
					vp_cpy [r14 + ML_MSG_DATA + KN_DATA_LINK_ROUTE_VIA], r13
					fn_call sys/get_cpu_id
					vp_cpy r0, [r14 + ML_MSG_DATA + KN_DATA_LINK_ROUTE_VIA]

					;copy and send to all neighbors apart from old via
					fn_bind sys/link_statics, r12
					loopstart_list_forwards r12 + LK_STATICS_LINKS_LIST, r12, r11
						vp_cpy [r11 + LK_NODE_CPU_ID], r10
						continueif r10, ==, r13
						fn_call sys/mail_alloc
						vp_cpy r0, r5
						vp_cpy r0, r1
						vp_cpy r14, r0
						vp_cpy [r14 + ML_MSG_LENGTH], r2
						fn_call sys/mem_copy
						vp_cpy r10, [r5 + (ML_MSG_DEST + 8)]
						vp_cpy r5, r0
						fn_call sys/mail_send
					loopend
				drop_msg:
					vp_cpy r14, r1
					fn_call sys/mail_free
					break
				case r1, ==, KN_CALL_GUI_UPDATE
					;free update message
					vp_cpy r14, r1
					fn_call sys/mail_free

					;create screen window ?
					fn_bind gui/gui_statics, r14
					vp_cpy [r14 + GUI_STATICS_WINDOW], r13
					if r13, ==, 0
						;init sdl2
						sdl_setmainready
						sdl_init SDL_INIT_VIDEO

						;create window
						vp_lea [rel title], r13
						sdl_createwindow r13, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED, 1024, 768, SDL_WINDOW_OPENGL
						vp_cpy r0, [r14 + GUI_STATICS_WINDOW]

						;create renderer
						sdl_createrenderer r0, -1, SDL_RENDERER_ACCELERATED
						vp_cpy r0, [r14 + GUI_STATICS_RENDERER]
					endif

					;update screen
					vp_cpy [r14 + GUI_STATICS_SCREEN], r1
					if r1, !=, 0
						vp_cpy [r14 + GUI_STATICS_RENDERER], r0
						vp_call draw_view

						fn_bind gui/gui_statics, r0
						sdl_renderpresent [r0 + GUI_STATICS_RENDERER]
					endif
					break
				default
				endswitch
			loopend

			;get time
			fn_call sys/get_cpu_time

			;start any tasks ready to restart
			fn_bind sys/task_statics, r3
			vp_cpy [r3 + TK_STATICS_TIMER_LIST + LH_LIST_HEAD], r2
			ln_get_succ r2, r2
			if r2, !=, 0
				loopstart_list_forwards r3 + TK_STATICS_TIMER_LIST, r2, r1
					vp_cpy [r1 + TK_NODE_TIME], r5
					breakif r5, >, r0

					;task ready, remove from timer list and place on ready list
					vp_cpy r1, r5
					ln_remove_node r5, r6
					ln_add_node_before r15, r1, r6
				loopend
			endif

			;next task if other ready tasks
			vp_cpy [r3 + TK_STATICS_TASK_LIST + LH_LIST_HEAD], r2
			vp_cpy [r3 + TK_STATICS_TASK_LIST + LH_LIST_TAILPRED], r1
			continueif r2, !=, r1

			;exit if no task waiting for timer
			vp_cpy [r3 + TK_STATICS_TIMER_LIST + LH_LIST_HEAD], r2
			ln_get_succ r2, r1
			breakif r1, ==, 0

			;sleep till next wake time
			vp_xchg r0, r2
			vp_cpy [r0 + TK_NODE_TIME], r0
			vp_sub r2, r0
			vp_cpy 1000, r3
			vp_xor r2, r2
			vp_div r3
			if r0, <, 1
				vp_cpy 1, r0
			endif
			sdl_delay r0
		loopend

		;deinit gui
		fn_call gui/gui_deinit_gui

		;free any kernel routing table
		vp_cpy [r4 + LK_TABLE_ARRAY], r0
		fn_call sys/mem_free
		vp_add LK_TABLE_SIZE, r4

		;deinit allocator
		fn_call sys/mem_deinit_allocator

		;deinit mailer
		fn_call sys/mail_deinit_mailer

		;deinit tasker
		fn_call sys/task_deinit_tasker

		;deinit loader
		fn_call sys/load_deinit_loader

		;pop argv and exit !
		vp_add 8, r4
		sys_exit 0

	draw_view:
		;inputs
		;r0 = ctx
		;r1 = view object

		;draw myself
		vp_push r0
		vp_push r1
		vp_call [r1 + GUI_VIEW_DRAW]
		vp_pop r1
		vp_pop r0

		;draw child views
		loopstart_list_forwards r1 + GUI_VIEW_LIST, r2, r1
			vp_push r0
			vp_push r2
			vp_call draw_view
			vp_pop r2
			vp_pop r0
		loopend
		vp_ret

	title:
		db "Asm Kernel GUI Window", 0

	fn_function_end
