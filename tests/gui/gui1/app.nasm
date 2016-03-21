%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/gui.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_button.inc'
%include 'class/class_progress.inc'
%include 'tests/gui/gui1/app.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui1/app

		def_structure	app
			def_long	app_last_event
			def_long	app_window
			def_long	app_window_panel
			def_long	app_panel
			def_long	app_cpu_total
			def_long	app_cpu_count
			def_long	app_task_mailboxes
			def_long	app_task_progress
			def_struct	app_task_mailbox, ml_mailbox
			def_long	app_select1
			def_long	app_select2
		def_structure_end

		;init app vars
		vp_sub app_size, r4

		;create my window
		static_call window, create
		fn_assert r0, !=, 0
		static_call window, panel
		vp_cpy r0, [r4 + app_window]
		vp_cpy r1, [r4 + app_window_panel]

		;set owner
		static_call task, tcb
		vp_cpy r0, r1
		vp_cpy [r4 + app_window], r0
		static_call window, set_owner

		;add my panel
		static_call flow, create
		fn_assert r0, !=, 0
		vp_cpy r0, [r4 + app_panel]
		vp_cpy flow_flag_down | flow_flag_fillw, r8
		static_call flow, set_flags
		vp_xor r8, r8
		vp_xor r9, r9
		vp_xor r10, r10
		vp_xor r11, r11
		static_call flow, set_color
		vp_cpy [r4 + app_window_panel], r1
		static_call flow, add

		;allocate array for progress bars
		static_call cpu, total
		vp_cpy r0, [r4 + app_cpu_total]
		vp_mul 8, r0
		static_call mem, alloc
		fn_assert r0, !=, 0
		vp_cpy r0, [r4 + app_task_progress]

		;add num cpus progress bars to my app panel
		vp_xor r1, r1
		vp_cpy r1, [r4 + app_cpu_count]
		loop_start
			static_call progress, create
			fn_assert r0, !=, 0
			vp_xor r1, r1
			static_call progress, set_percent
			vp_cpy 0, r8
			vp_cpy 255, r9
			vp_cpy 0, r10
			vp_cpy 255, r11
			static_call progress, set_color
			vp_cpy [r4 + app_panel], r1
			static_call progress, add

			;save progress bar for this cpu
			vp_cpy [r4 + app_cpu_count], r1
			vp_cpy [r4 + app_task_progress], r2
			vp_cpy r0, [r2 + r1 * 8]

			vp_inc r1
			vp_cpy r1, [r4 + app_cpu_count]
		loop_until r1, ==, [r4 + app_cpu_total]

		;set to pref size
		vp_cpy [r4 + app_window], r0
		method_call window, pref_size
		vp_cpy [r4 + app_window], r0
		vp_cpy 32, r8
		vp_cpy 32, r9
		static_call window, change

		;add to screen and dirty
		vp_cpy [r4 + app_window], r0
		static_call gui, add
		static_call window, dirty_all

		;allocate array for child mailbox ID's
		vp_cpy [r4 + app_cpu_total], r0
		vp_mul mailbox_id_size, r0
		static_call mem, alloc
		fn_assert r0, !=, 0
		vp_cpy r0, [r4 + app_task_mailboxes]

		;open global farm
		vp_cpy r0, r1
		vp_lea [rel child_task], r0
		vp_cpy [r4 + app_cpu_total], r2
		static_call task, global

		;init task mailbox
		vp_lea [r4 + app_task_mailbox], r0
		ml_init r0, r1, r2

		;set up mailbox select array
		vp_cpy r0, [r4 + app_select2]
		static_call task, mailbox
		vp_cpy r0, [r4 + app_select1]

		;app event loop
		loop_start
			;new round of samples ?
			vp_cpy [r4 + app_cpu_count], r0
			if r0, ==, [r4 + app_cpu_total]
				;send out sample commands
				loop_start
					static_call mail, alloc
					fn_assert r0, !=, 0
					vp_cpy r0, r5

					;child task num
					vp_cpy [r4 + app_cpu_count], r0
					vp_dec r0
					vp_cpy r0, [r4 + app_cpu_count]

					vp_cpy 1, qword[r5 + ml_msg_data + app_mail_command]
					vp_cpy ml_msg_data + app_mail_size, qword[r5 + ml_msg_length]

					vp_cpy [r4 + app_task_progress], r1
					vp_cpy [r1 + r0 * 8], r1
					vp_cpy r1, [r5 + ml_msg_data + app_mail_progress]

					vp_cpy [r4 + app_task_mailboxes], r1
					vp_mul mailbox_id_size, r0
					vp_cpy [r1 + r0], r2
					vp_cpy [r1 + r0 + 8], r3
					vp_cpy r2, [r5 + ml_msg_dest]
					vp_cpy r3, [r5 + ml_msg_dest + 8]

					static_call task, mailbox
					vp_cpy [r4 + app_select2], r0
					vp_cpy r0, [r5 + ml_msg_data + app_mail_reply_id]
					vp_cpy r1, [r5 + ml_msg_data + app_mail_reply_id + 8]

					;send command
					vp_cpy r5, r0
					static_call mail, send

					vp_cpy [r4 + app_cpu_count], r0
				loop_until r0, ==, 0
			endif

			;select on 2 mailboxes
			vp_lea [r4 + app_select1], r0
			vp_cpy 2, r1
			static_call mail, select

			;which mailbox has mail ?
			if r0, ==, [r4 + app_select1]
				;main mailbox
				static_call mail, read
				vp_cpy r0, [r4 + app_last_event]

				;dispatch event to view
				vp_cpy r0, r1
				vp_cpy [r1 + (ml_msg_data + ev_data_view)], r0
				method_call view, event
			else
				;task mailbox
				static_call mail, read
				vp_cpy r0, [r4 + app_last_event]

				;update progress bar
				vp_cpy [r0 + ml_msg_data + app_mail_task_count], r1
				vp_cpy [r0 + ml_msg_data + app_mail_progress], r0
				vp_shl progress_fixed_point - 5, r1
				static_call progress, set_percent
				static_call progress, dirty

				;count up replies
				vp_cpy [r4 + app_cpu_count], r0
				vp_inc r0
				vp_cpy r0, [r4 + app_cpu_count]
			endif

			;free event message
			vp_cpy [r4 + app_last_event], r0
			static_call mem, free

			;be friendly
			static_call task, yield
		loop_end

		;wait for outstanding replys
		vp_cpy [r4 + app_cpu_count], r5
		loop_while r5, !=, [r4 + app_cpu_total]
			vp_cpy [r4 + app_select2], r0
			static_call mail, read
			static_call mem, free
			vp_inc r5
		loop_end

		;send out exit commands
		vp_xor r5, r5
		loop_start
			static_call mail, alloc
			fn_assert r0, !=, 0
			vp_cpy 0, qword[r0 + ml_msg_data + app_mail_command]
			vp_cpy ml_msg_data + app_mail_size, qword[r0 + ml_msg_length]

			vp_cpy [r4 + app_task_mailboxes], r2
			vp_cpy r5, r1
			vp_mul mailbox_id_size, r1
			vp_cpy [r2 + r1 + 8], r3
			vp_cpy [r2 + r1], r2
			vp_cpy r2, [r0 + ml_msg_dest]
			vp_cpy r3, [r0 + ml_msg_dest + 8]

			;send command
			static_call mail, send

			vp_inc r5
		loop_until r5, ==, [r4 + app_cpu_total]

		;free arrays
		vp_cpy [r4 + app_task_mailboxes], r0
		static_call mem, free
		vp_cpy [r4 + app_task_progress], r0
		static_call mem, free

		;deref window
		vp_cpy [r4 + app_window], r0
		static_call window, deref

		vp_add app_size, r4
		vp_ret

	child_task:
		db 'tests/gui/gui1/child', 0

	fn_function_end
