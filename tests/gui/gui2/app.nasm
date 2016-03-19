%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/gui.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_button.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui2/app

		def_structure	app
			def_long	app_last_event
			def_long	app_window
			def_long	app_window_panel
			def_long	app_panel
			def_long	app_button1
			def_long	app_button2
			def_long	app_button3
			def_long	app_button4
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
		static_call view, set_color
		vp_cpy [r4 + app_window_panel], r1
		static_call flow, add

		;add launch buttons to my app panel
		static_call button, create
		fn_assert r0, !=, 0
		vp_cpy 255, r8
		vp_cpy 255, r9
		vp_cpy 0, r10
		vp_cpy 255, r11
		static_call button, set_color
		vp_cpy [r4 + app_panel], r1
		static_call button, add
		vp_cpy r0, [r4 + app_button1]

		static_call button, create
		fn_assert r0, !=, 0
		vp_cpy 255, r8
		vp_cpy 255, r9
		vp_cpy 0, r10
		vp_cpy 255, r11
		static_call button, set_color
		vp_cpy [r4 + app_panel], r1
		static_call button, add
		vp_cpy r0, [r4 + app_button2]

		static_call button, create
		fn_assert r0, !=, 0
		vp_cpy 255, r8
		vp_cpy 255, r9
		vp_cpy 0, r10
		vp_cpy 255, r11
		static_call button, set_color
		vp_cpy [r4 + app_panel], r1
		static_call button, add
		vp_cpy r0, [r4 + app_button3]

		static_call button, create
		fn_assert r0, !=, 0
		vp_cpy 255, r8
		vp_cpy 255, r9
		vp_cpy 0, r10
		vp_cpy 255, r11
		static_call button, set_color
		vp_cpy [r4 + app_panel], r1
		static_call button, add
		vp_cpy r0, [r4 + app_button4]

		;set to pref size
		vp_cpy [r4 + app_window], r0
		method_call window, pref_size
		vp_cpy [r4 + app_window], r0
		vp_cpy 256, r8
		vp_cpy 256, r9
		static_call window, change

		;add to screen and dirty
		vp_cpy [r4 + app_window], r0
		static_call gui, add
		static_call window, dirty_all

		;app event loop
		loop_start
			static_call mail, mymail
			vp_cpy r0, [r4 + app_last_event]

			;dispatch event to view
			vp_cpy r0, r1
			vp_cpy [r1 + (ml_msg_data + ev_data_view)], r0
			method_call view, event

			;launch button ?
			vp_cpy [r4 + app_last_event], r0
			vp_cpy [r0 + (ml_msg_data + ev_data_view)], r1
			vp_cpy [r0 + (ml_msg_data + ev_data_buttons)], r2
			if r2, ==, 0
				switch
				case r1, ==, [r4 + app_button1]
					fn_debug start task1
					vp_lea [rel child_task1], r0
					static_call task, child
					break
				case r1, ==, [r4 + app_button2]
					fn_debug start task2
					vp_lea [rel child_task2], r0
					static_call task, child
					break
				case r1, ==, [r4 + app_button3]
					fn_debug start task3
					vp_lea [rel child_task3], r0
					static_call task, child
					break
				case r1, ==, [r4 + app_button4]
					fn_debug start task4
					vp_lea [rel child_task4], r0
					static_call task, child
					break
				default
				endswitch
			endif

			;free event message
			vp_cpy [r4 + app_last_event], r0
			static_call mem, free
		loop_end

		;deref window
		vp_cpy [r4 + app_window], r0
		static_call window, deref

		vp_add app_size, r4
		vp_ret

	child_task1:
		db 'tests/test5', 0
	child_task2:
		db 'tests/test7', 0
	child_task3:
		db 'tests/test9', 0
	child_task4:
		db 'tests/test11', 0

	fn_function_end
