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

		def_structure	local
			def_long	local_last_event
			def_long	local_window
			def_long	local_window_panel
			def_long	local_panel
			def_long	local_cpu_total
			def_long	local_cpu_count
			def_long	local_task_mailboxes
			def_long	local_task_progress
			def_struct	local_task_mailbox, ml_mailbox
			def_long	local_select1
			def_long	local_select2
		def_structure_end

		;init app vars
		vp_sub local_size, r4

		;create my window
		static_call window, create, '', '[r4 + local_window]'
		fn_assert r0, !=, 0
		static_call window, get_panel, 'r0', '[r4 + local_window_panel]'
		static_call string, create, '"Network Task Monitor"'
		fn_assert r0, !=, 0
		static_call window, set_title, '[r4 + local_window], r0'
		static_call string, create, '"Status Text"'
		fn_assert r0, !=, 0
		static_call window, set_status, '[r4 + local_window], r0'

		;add my panel
		static_call flow, create, '', '[r4 + local_panel]'
		fn_assert r0, !=, 0
		static_call flow, set_flow_flags, 'r0, flow_flag_down | flow_flag_fillw'
		static_call flow, set_color, 'r0, 0'
		static_call flow, add, 'r0, [r4 + local_window_panel]'

		;allocate array for progress bars
		static_call sys_cpu, total, '', '[r4 + local_cpu_total]'
		vp_mul 8, r0
		static_call sys_mem, alloc, 'r0', '[r4 + local_task_progress], r1'
		fn_assert r0, !=, 0

		;add num cpus progress bars to my app panel
		vp_xor r1, r1
		vp_cpy r1, [r4 + local_cpu_count]
		loop_start
			static_call progress, create
			fn_assert r0, !=, 0
			static_call progress, set_max, 'r0, 48'
			static_call progress, set_color, 'r0, 0xff00ff00'
			static_call progress, add, 'r0, [r4 + local_panel]'

			;save progress bar for this cpu
			vp_cpy [r4 + local_cpu_count], r1
			vp_cpy [r4 + local_task_progress], r2
			vp_cpy r0, [r2 + r1 * 8]

			vp_inc r1
			vp_cpy r1, [r4 + local_cpu_count]
		loop_until r1, ==, [r4 + local_cpu_total]

		;set to pref size
		vp_cpy [r4 + local_window], r0
		method_call window, pref_size
		static_call window, change, 'r0, 32, 32, r10, r11'

		;set owner
		static_call sys_task, tcb
		static_call window, set_owner, '[r4 + local_window], r0'

		;add to screen and dirty
		static_call gui_gui, add
		static_call window, dirty_all

		;allocate array for child mailbox ID's
		vp_cpy [r4 + local_cpu_total], r0
		vp_mul mailbox_id_size, r0
		static_call sys_mem, alloc, '', '[r4 + local_task_mailboxes], r1'
		fn_assert r0, !=, 0

		;open global farm
		static_call sys_task, open_global, '"tests/gui/gui1/child", r0, [r4 + local_cpu_total]'

		;init task mailbox
		vp_lea [r4 + local_task_mailbox], r0
		ml_init r0, r1, r2

		;set up mailbox select array
		vp_cpy r0, [r4 + local_select2]
		static_call sys_task, mailbox, '', '[r4 + local_select1], r1'

		;app event loop
		loop_start
			;new round of samples ?
			vp_cpy [r4 + local_cpu_count], r0
			if r0, ==, [r4 + local_cpu_total]
				;send out sample commands
				loop_start
					static_call sys_mail, alloc, '', 'r5'
					fn_assert r0, !=, 0

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

					static_call sys_cpu, id, '', '[r5 + sample_mail_reply_id + 8]'
					vp_cpy [r4 + local_select2], r0
					vp_cpy r0, [r5 + sample_mail_reply_id]

					;send command
					static_call sys_mail, send, 'r5'

					vp_cpy [r4 + local_cpu_count], r0
				loop_until r0, ==, 0
			endif

			;select on 2 mailboxes
			vp_lea [r4 + local_select1], r0
			static_call sys_mail, select, 'r0, 2'

			;which mailbox has mail ?
			if r0, ==, [r4 + local_select1]
				;main mailbox
				static_call sys_mail, read, '', '[r4 + local_last_event]'

				;dispatch event to view
				vp_cpy r0, r1
				vp_cpy [r1 + ev_data_view], r0
				method_call view, event
			else
				;task mailbox
				static_call sys_mail, read, '', '[r4 + local_last_event]'

				;update progress bar
				vp_cpy [r0 + sample_mail_task_count], r1
				vp_cpy [r0 + sample_mail_progress], r0
				static_call progress, set_val
				static_call progress, dirty

				;count up replies
				vp_cpy [r4 + local_cpu_count], r0
				vp_inc r0
				vp_cpy r0, [r4 + local_cpu_count]
			endif

			;free event message
			static_call sys_mem, free, '[r4 + local_last_event]'

			;be friendly
			static_call sys_task, yield
		loop_end

		;wait for outstanding replys
		vp_cpy [r4 + local_cpu_count], r5
		loop_while r5, !=, [r4 + local_cpu_total]
			static_call sys_mail, read, '[r4 + local_select2]', 'r0'
			static_call sys_mem, free, 'r0'
			vp_inc r5
		loop_end

		;send out exit commands
		vp_xor r5, r5
		loop_start
			static_call sys_mail, alloc
			fn_assert r0, !=, 0
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
			static_call sys_mail, send

			vp_inc r5
		loop_until r5, ==, [r4 + local_cpu_total]

		;free arrays
		static_call sys_mem, free, '[r4 + local_task_mailboxes]'
		static_call sys_mem, free, '[r4 + local_task_progress]'

		;deref window
		static_call window, deref, '[r4 + local_window]'

		vp_add local_size, r4
		vp_ret

	fn_function_end
