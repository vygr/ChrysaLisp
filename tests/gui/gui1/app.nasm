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

		def_structure local
			long local_last_event
			long local_window
			long local_window_panel
			long local_panel
			long local_cpu_total
			long local_cpu_count
			long local_task_mailboxes
			long local_task_progress
			struct local_task_mailbox, ml_mailbox
			long local_select1
			long local_select2
		def_structure_end

		;init app vars
		vp_sub local_size, r4

		;create my window
		s_call window, create, {}, {[r4 + local_window]}
		assert r0, !=, 0
		s_call window, get_panel, {r0}, {[r4 + local_window_panel]}
		s_call string, create, {"Network Task Monitor"}, {r0}
		assert r0, !=, 0
		s_call window, set_title, {[r4 + local_window], r0}
		s_call string, create, {"Status Text"}, {r0}
		assert r0, !=, 0
		s_call window, set_status, {[r4 + local_window], r0}

		;add my panel
		s_call flow, create, {}, {[r4 + local_panel]}
		assert r0, !=, 0
		s_call flow, set_flow_flags, {r0, flow_flag_down | flow_flag_fillw}
		s_call flow, set_color, {r0, 0}
		s_call flow, add, {r0, [r4 + local_window_panel]}

		;allocate array for progress bars
		s_call sys_cpu, total, {}, {[r4 + local_cpu_total]}
		vp_mul 8, r0
		s_call sys_mem, alloc, {r0}, {[r4 + local_task_progress], _}
		assert r0, !=, 0

		;add num cpus progress bars to my app panel
		vp_xor r1, r1
		vp_cpy r1, [r4 + local_cpu_count]
		loop_start
			s_call progress, create, {}, {r0}
			assert r0, !=, 0
			s_call progress, set_max, {r0, 48}
			s_call progress, set_color, {r0, 0xff00ff00}
			s_call progress, add, {r0, [r4 + local_panel]}

			;save progress bar for this cpu
			vp_cpy [r4 + local_cpu_count], r1
			vp_cpy [r4 + local_task_progress], r2
			vp_cpy r0, [r2 + r1 * 8]

			vp_inc r1
			vp_cpy r1, [r4 + local_cpu_count]
		loop_until r1, ==, [r4 + local_cpu_total]

		;set to pref size
		m_call window, pref_size, {[r4 + local_window]}, {r10, r11}
		s_call window, change, {r0, 32, 32, r10, r11}

		;set owner
		s_call sys_task, tcb, {}, {r0}
		s_call window, set_owner, {[r4 + local_window], r0}

		;add to screen and dirty
		s_call gui_gui, add, {r0}
		s_call window, dirty_all, {r0}

		;allocate array for child mailbox ID's
		vp_cpy [r4 + local_cpu_total], r0
		vp_mul mailbox_id_size, r0
		s_call sys_mem, alloc, {r0}, {[r4 + local_task_mailboxes], _}
		assert r0, !=, 0

		;open global farm
		s_call sys_task, open_global, {"tests/gui/gui1/child", r0, [r4 + local_cpu_total]}

		;init task mailbox
		vp_lea [r4 + local_task_mailbox], r0
		ml_init r0, r1, r2

		;set up mailbox select array
		vp_cpy r0, [r4 + local_select2]
		s_call sys_task, mailbox, {}, {[r4 + local_select1], r1}

		;app event loop
		loop_start
			;new round of samples ?
			vp_cpy [r4 + local_cpu_count], r0
			if r0, ==, [r4 + local_cpu_total]
				;send out sample commands
				loop_start
					s_call sys_mail, alloc, {}, {r5}
					assert r0, !=, 0

					;child task num
					vp_cpy [r4 + local_cpu_count], r0
					vp_dec r0
					vp_cpy r0, [r4 + local_cpu_count]

					vp_cpy_cl 1, [r5 + sample_mail_command]
					vp_cpy_cl sample_mail_size, [r5 + ml_msg_length]

					vp_cpy [r4 + local_task_progress], r1
					vp_cpy [r1 + r0 * 8], r1
					vp_cpy r1, [r5 + sample_mail_progress]

					vp_cpy [r4 + local_task_mailboxes], r1
					vp_mul mailbox_id_size, r0
					vp_cpy [r1 + r0], r2
					vp_cpy [r1 + r0 + 8], r3
					vp_cpy r2, [r5 + ml_msg_dest]
					vp_cpy r3, [r5 + ml_msg_dest + 8]

					s_call sys_cpu, id, {}, {[r5 + sample_mail_reply_id + 8]}
					vp_cpy [r4 + local_select2], r0
					vp_cpy r0, [r5 + sample_mail_reply_id]

					;send command
					s_call sys_mail, send, {r5}

					vp_cpy [r4 + local_cpu_count], r0
				loop_until r0, ==, 0
			endif

			;select on 2 mailboxes
			s_call sys_mail, select, {:[r4 + local_select1], 2}, {r0}

			;which mailbox has mail ?
			if r0, ==, [r4 + local_select1]
				;main mailbox
				s_call sys_mail, read, {r0}, {[r4 + local_last_event]}

				;dispatch event to view
				m_call view, event, {[r0 + ev_data_view], r0}
			else
				;task mailbox
				s_call sys_mail, read, {r0}, {[r4 + local_last_event]}

				;update progress bar
				s_call progress, set_val, {[r0 + sample_mail_progress], [r0 + sample_mail_task_count]}
				s_call progress, dirty, {r0}

				;count up replies
				vp_cpy [r4 + local_cpu_count], r0
				vp_inc r0
				vp_cpy r0, [r4 + local_cpu_count]
			endif

			;free event message
			s_call sys_mem, free, {[r4 + local_last_event]}

			;be friendly
			s_call sys_task, yield
		loop_end

		;wait for outstanding replys
		vp_cpy [r4 + local_cpu_count], r5
		loop_while r5, !=, [r4 + local_cpu_total]
			s_call sys_mail, read, {[r4 + local_select2]}, {r0}
			s_call sys_mem, free, {r0}
			vp_inc r5
		loop_end

		;send out exit commands
		vp_xor r5, r5
		loop_start
			s_call sys_mail, alloc, {}, {r0}
			assert r0, !=, 0
			vp_cpy_cl 0, [r0 + sample_mail_command]
			vp_cpy_cl sample_mail_size, [r0 + ml_msg_length]

			vp_cpy [r4 + local_task_mailboxes], r2
			vp_cpy r5, r1
			vp_mul mailbox_id_size, r1
			vp_cpy [r2 + r1 + 8], r3
			vp_cpy [r2 + r1], r2
			vp_cpy r2, [r0 + ml_msg_dest]
			vp_cpy r3, [r0 + ml_msg_dest + 8]

			;send command
			s_call sys_mail, send, {r0}

			vp_inc r5
		loop_until r5, ==, [r4 + local_cpu_total]

		;free arrays
		s_call sys_mem, free, {[r4 + local_task_mailboxes]}
		s_call sys_mem, free, {[r4 + local_task_progress]}

		;deref window
		s_call window, deref, {[r4 + local_window]}

		vp_add local_size, r4
		vp_ret

	fn_function_end
