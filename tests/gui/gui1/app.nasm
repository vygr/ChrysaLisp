%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/gui.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_button.inc'
%include 'class/class_progress.inc'
%include 'class/class_string.inc'
%include 'tests/gui/gui1/app.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui1/app

		def_local
			def_local_long		last_event
			def_local_long		window
			def_local_long		window_panel
			def_local_long		panel
			def_local_long		cpu_total
			def_local_long		cpu_count
			def_local_long		task_mailboxes
			def_local_long		task_progress
			def_local_struct	task_mailbox, ml_mailbox
			def_local_long		select1
			def_local_long		select2
		def_local_end

		;init app vars
		vp_sub local_size, r4

		;create my window
		static_call window, create, {}, {.window}
		assert r0, !=, 0
		static_call window, get_panel, {r0}, {.window_panel}
		static_call string, create, {"Network Task Monitor"}, {r0}
		assert r0, !=, 0
		static_call window, set_title, {.window, r0}
		static_call string, create, {"Status Text"}, {r0}
		assert r0, !=, 0
		static_call window, set_status, {.window, r0}

		;add my panel
		static_call flow, create, {}, {.panel}
		assert r0, !=, 0
		static_call flow, set_flow_flags, {r0, flow_flag_down | flow_flag_fillw}
		static_call flow, set_color, {r0, 0}
		static_call flow, add, {r0, .window_panel}

		;allocate array for progress bars
		static_call sys_cpu, total, {}, {.cpu_total}
		vp_mul 8, r0
		static_call sys_mem, alloc, {r0}, {.task_progress, _}
		assert r0, !=, 0

		;add num cpus progress bars to my app panel
		vp_xor r1, r1
		vp_cpy r1, .cpu_count
		loop_start
			static_call progress, create, {}, {r0}
			assert r0, !=, 0
			static_call progress, set_max, {r0, 48}
			static_call progress, set_color, {r0, 0xff00ff00}
			static_call progress, add, {r0, .panel}

			;save progress bar for this cpu
			vp_cpy .cpu_count, r1
			vp_cpy .task_progress, r2
			vp_cpy r0, [r2 + r1 * 8]

			vp_inc r1
			vp_cpy r1, .cpu_count
		loop_until r1, ==, .cpu_total

		;set to pref size
		method_call window, pref_size, {.window}, {r10, r11}
		static_call window, change, {r0, 32, 32, r10, r11}

		;set owner
		static_call sys_task, tcb, {}, {r0}
		static_call window, set_owner, {.window, r0}

		;add to screen and dirty
		static_call gui_gui, add, {r0}
		static_call window, dirty_all, {r0}

		;allocate array for child mailbox ID's
		vp_cpy .cpu_total, r0
		vp_mul mailbox_id_size, r0
		static_call sys_mem, alloc, {r0}, {.task_mailboxes, _}
		assert r0, !=, 0

		;open global farm
		static_call sys_task, open_global, {"tests/gui/gui1/child", r0, .cpu_total}

		;init task mailbox
		vp_lea .task_mailbox, r0
		ml_init r0, r1, r2

		;set up mailbox select array
		vp_cpy r0, .select2
		static_call sys_task, mailbox, {}, {.select1, r1}

		;app event loop
		loop_start
			;new round of samples ?
			vp_cpy .cpu_count, r0
			if r0, ==, .cpu_total
				;send out sample commands
				loop_start
					static_call sys_mail, alloc, {}, {r5}
					assert r0, !=, 0

					;child task num
					vp_cpy .cpu_count, r0
					vp_dec r0
					vp_cpy r0, .cpu_count

					vp_cpy_cl 1, [r5 + sample_mail_command]
					vp_cpy_cl sample_mail_size, [r5 + ml_msg_length]

					vp_cpy .task_progress, r1
					vp_cpy [r1 + r0 * 8], r1
					vp_cpy r1, [r5 + sample_mail_progress]

					vp_cpy .task_mailboxes, r1
					vp_mul mailbox_id_size, r0
					vp_cpy [r1 + r0], r2
					vp_cpy [r1 + r0 + 8], r3
					vp_cpy r2, [r5 + ml_msg_dest]
					vp_cpy r3, [r5 + ml_msg_dest + 8]

					static_call sys_cpu, id, {}, {[r5 + sample_mail_reply_id + 8]}
					vp_cpy .select2, r0
					vp_cpy r0, [r5 + sample_mail_reply_id]

					;send command
					static_call sys_mail, send, {r5}

					vp_cpy .cpu_count, r0
				loop_until r0, ==, 0
			endif

			;select on 2 mailboxes
			static_call sys_mail, select, {&.select1, 2}, {r0}

			;which mailbox has mail ?
			if r0, ==, .select1
				;main mailbox
				static_call sys_mail, read, {r0}, {.last_event}

				;dispatch event to view
				method_call view, event, {[r0 + ev_data_view], r0}
			else
				;task mailbox
				static_call sys_mail, read, {r0}, {.last_event}

				;update progress bar
				static_call progress, set_val, {[r0 + sample_mail_progress], [r0 + sample_mail_task_count]}
				static_call progress, dirty, {r0}

				;count up replies
				vp_cpy .cpu_count, r0
				vp_inc r0
				vp_cpy r0, .cpu_count
			endif

			;free event message
			static_call sys_mem, free, {.last_event}

			;be friendly
			static_call sys_task, yield
		loop_end

		;wait for outstanding replys
		vp_cpy .cpu_count, r5
		loop_while r5, !=, .cpu_total
			static_call sys_mail, read, {.select2}, {r0}
			static_call sys_mem, free, {r0}
			vp_inc r5
		loop_end

		;send out exit commands
		vp_xor r5, r5
		loop_start
			static_call sys_mail, alloc, {}, {r0}
			assert r0, !=, 0
			vp_cpy_cl 0, [r0 + sample_mail_command]
			vp_cpy_cl sample_mail_size, [r0 + ml_msg_length]

			vp_cpy .task_mailboxes, r2
			vp_cpy r5, r1
			vp_mul mailbox_id_size, r1
			vp_cpy [r2 + r1 + 8], r3
			vp_cpy [r2 + r1], r2
			vp_cpy r2, [r0 + ml_msg_dest]
			vp_cpy r3, [r0 + ml_msg_dest + 8]

			;send command
			static_call sys_mail, send, {r0}

			vp_inc r5
		loop_until r5, ==, .cpu_total

		;free arrays
		static_call sys_mem, free, {.task_mailboxes}
		static_call sys_mem, free, {.task_progress}

		;deref window
		static_call window, deref, {.window}

		vp_add local_size, r4
		vp_ret

	fn_function_end
