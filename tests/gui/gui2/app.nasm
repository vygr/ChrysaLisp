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

		def_structure	local
			def_long	local_last_event
			def_long	local_window
			def_long	local_window_panel
			def_long	local_panel
		def_structure_end

		;init app vars
		vp_sub local_size, r4

		;create my window
		static_call window, create
		fn_assert r0, !=, 0
		vp_cpy r0, [r4 + local_window]
		static_call window, get_panel
		vp_cpy r1, [r4 + local_window_panel]
		fn_string 'Test Runner', r1
		static_call window, set_title
		fn_string 'Status Text', r1
		static_call window, set_status

		;add my panel
		static_call flow, create
		fn_assert r0, !=, 0
		vp_cpy r0, [r4 + local_panel]
		vp_cpy flow_flag_down | flow_flag_fillw, r1
		static_call flow, set_flow_flags
		vp_xor r1, r1
		static_call flow, set_color
		vp_cpy [r4 + local_window_panel], r1
		static_call flow, add

		;add launch buttons to my app panel
		static_call button, create
		fn_assert r0, !=, 0
		vp_cpy 0xffffff00, r1
		static_call button, set_color
		fn_string 'tests/farm', r1
		static_call button, set_text
		vp_cpy [r4 + local_panel], r1
		static_call button, add

		static_call button, create
		fn_assert r0, !=, 0
		vp_cpy 0xffffff00, r1
		static_call button, set_color
		fn_string 'tests/array', r1
		static_call button, set_text
		vp_cpy [r4 + local_panel], r1
		static_call button, add

		static_call button, create
		fn_assert r0, !=, 0
		vp_cpy 0xffffff00, r1
		static_call button, set_color
		fn_string 'tests/pipe', r1
		static_call button, set_text
		vp_cpy [r4 + local_panel], r1
		static_call button, add

		static_call button, create
		fn_assert r0, !=, 0
		vp_cpy 0xffffff00, r1
		static_call button, set_color
		fn_string 'tests/global', r1
		static_call button, set_text
		vp_cpy [r4 + local_panel], r1
		static_call button, add

		;set to pref size
		vp_cpy [r4 + local_window], r0
		method_call window, pref_size
		vp_cpy 256, r8
		vp_cpy 256, r9
		static_call window, change

		;set owner
		static_call sys_task, tcb
		vp_cpy r0, r1
		vp_cpy [r4 + local_window], r0
		static_call window, set_owner

		;add to screen and dirty
		static_call gui_gui, add
		static_call window, dirty_all

		;app event loop
		loop_start
			static_call sys_mail, mymail
			vp_cpy r0, [r4 + local_last_event]

			;dispatch event to view
			vp_cpy r0, r1
			vp_cpy [r1 + ev_data_view], r0
			method_call view, event

			;launch button ?
			vp_cpy [r4 + local_last_event], r1
			vp_cpy [r1 + ev_data_buttons], r1
			if r1, ==, 0
				static_call view, get_parent
				if r1, ==, [r4 + local_panel]
					static_call button, get_text
					vp_cpy r1, r0
					static_call sys_task, open_child
				endif
			endif

			;free event message
			vp_cpy [r4 + local_last_event], r0
			static_call sys_mem, free
		loop_end

		;deref window
		vp_cpy [r4 + local_window], r0
		static_call window, deref

		vp_add local_size, r4
		vp_ret

	fn_function_end
