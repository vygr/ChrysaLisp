%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_window.inc'
%include 'class/class_button.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui1/app

		def_structure app
			def_long	app_last_event
			def_long	app_window
			def_long	app_window_panel
			def_long	app_panel
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

		;add my button panel
		static_call flow, create
		fn_assert r0, !=, 0
		vp_cpy r0, [r4 + app_panel]
		vp_cpy flow_flag_right | flow_flag_down, r8
		static_call flow, set_flags
		vp_xor r8, r8
		vp_xor r9, r9
		vp_xor r10, r10
		vp_xor r11, r11
		static_call view, set_color
		vp_cpy [r4 + app_window_panel], r1
		static_call flow, add

		;add 4 buttons to my app panel
		static_call button, create
		fn_assert r0, !=, 0
		vp_cpy 255, r8
		vp_cpy 255, r9
		vp_cpy 0, r10
		vp_cpy 192, r11
		static_call button, set_color
		vp_cpy [r4 + app_panel], r1
		static_call button, add

		static_call button, create
		fn_assert r0, !=, 0
		vp_cpy 0, r8
		vp_cpy 255, r9
		vp_cpy 255, r10
		vp_cpy 192, r11
		static_call button, set_color
		vp_cpy [r4 + app_panel], r1
		static_call button, add

		static_call button, create
		fn_assert r0, !=, 0
		vp_cpy 255, r8
		vp_cpy 0, r9
		vp_cpy 255, r10
		vp_cpy 192, r11
		static_call button, set_color
		vp_cpy [r4 + app_panel], r1
		static_call button, add

		static_call button, create
		fn_assert r0, !=, 0
		vp_cpy 0, r8
		vp_cpy 0, r9
		vp_cpy 0, r10
		vp_cpy 192, r11
		static_call button, set_color
		vp_cpy [r4 + app_panel], r1
		static_call button, add

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

		;app event loop
		loop_start
			;read events
			static_call mail, mymail
			vp_cpy r0, [r4 + app_last_event]

			;dispatch event to view
			vp_cpy r0, r1
			vp_cpy [r1 + (ml_msg_data + ev_data_view)], r0
			method_call view, event

			;free event message
			vp_cpy [r4 + app_last_event], r0
			static_call mem, free
		loop_end

		;deref window
		vp_cpy [r4 + app_window], r0
		static_call window, deref

		vp_add app_size, r4
		vp_ret

	fn_function_end
