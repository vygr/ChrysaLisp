%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/gui.inc'
%include 'inc/string.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_button.inc'
%include 'class/class_string.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui2/app

		def_structure	local, obj
			def_long	local_last_event
			def_long	local_window
			def_long	local_window_panel
			def_long	local_panel
			def_long	local_next
			def_long	local_button
		def_structure_end

		;init app vars
		vp_sub local_size, r4
		static_bind class, obj, r1
		static_call obj, init, {r4, r1}
		assert r1, !=, 0

		;create my window
		static_call window, create, {}, {[r4 + local_window]}
		assert r0, !=, 0
		static_call window, get_panel, {r0}, {[r4 + local_window_panel]}
		static_call string, create, {"Test Runner"}
		assert r0, !=, 0
		static_call window, set_title, {[r4 + local_window], r0}
		static_call string, create, {"Status Text"}
		assert r0, !=, 0
		static_call window, set_status, {[r4 + local_window], r0}

		;add my app panel
		static_call flow, create, {}, {[r4 + local_panel]}
		assert r0, !=, 0
		static_call flow, set_flow_flags, {r0, flow_flag_down | flow_flag_fillw}
		static_call flow, set_color, {r0, 0}
		static_call flow, add, {r0, [r4 + local_window_panel]}

		;add launch buttons to my app panel
		vp_rel launch_list, r0
		loop_start
			vp_xor r1, r1
			vp_cpy_b [r0], r1
			breakif r1, ==, 0
			vp_cpy r0, [r4 + local_next]

			static_call button, create, {}, {[r4 + local_button]}
			assert r0, !=, 0
			static_call button, set_color, {r0, 0xffffff00}
			static_call string, create, {[r4 + local_next]}
			assert r0, !=, 0
			static_call button, set_text, {[r4 + local_button], r0}
			static_call button, add, {r0, [r4 + local_panel]}
			vp_lea [r0 + button_pressed_signal], r1
			vp_rel on_press, r3
			static_call button, connect, {r0, r1, r4, r3}

			static_call sys_string, length, {[r4 + local_next]}
			vp_lea [r0 + r1 + 1], r0
		loop_end

		;set to pref size
		method_call window, pref_size, {[r4 + local_window]}
		vp_add 64, r10
		static_call window, change, {r0, 400, 256, r10, r11}

		;set window owner
		static_call sys_task, tcb
		static_call window, set_owner, {[r4 + local_window], r0}

		;add to screen and dirty
		static_call gui_gui, add
		static_call window, dirty_all

		;app event loop
		loop_start
			static_call sys_mail, mymail, {}, {[r4 + local_last_event]}

			;dispatch event to view
			method_call view, event, {[r0 + ev_data_view], r0}

			;free event message
			static_call sys_mem, free, {[r4 + local_last_event]}
		loop_end

		;deref window
		static_call window, deref, {[r4 + local_window]}

		method_call obj, deinit, {r4}
		vp_add local_size, r4
		vp_ret

	on_press:
		;inputs
		;r0 = app local object
		;r1 = button object

		static_call button, get_text, {r1}
		vp_lea [r1 + string_data], r0
		static_jmp sys_task, open_child, {r0}

	launch_list:
		db 'tests/farm', 0
		db 'tests/array', 0
		db 'tests/pipe', 0
		db 'tests/global', 0
		db 0

	fn_function_end
