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

		def_structure local, obj
			long local_last_event
			long local_window
			long local_window_panel
			long local_panel
			long local_next
			long local_button
		def_structure_end

		;init app vars
		vp_sub local_size, r4
		slot_function class, obj
		s_call obj, init, {r4, @_function_}, {r1}
		assert r1, !=, 0

		;create my window
		s_call window, create, {}, {[r4 + local_window]}
		assert r0, !=, 0
		s_call window, get_panel, {r0}, {[r4 + local_window_panel]}
		s_call string, create, {"Test Runner"}, {r0}
		assert r0, !=, 0
		s_call window, set_title, {[r4 + local_window], r0}
		s_call string, create, {"Status Text"}, {r0}
		assert r0, !=, 0
		s_call window, set_status, {[r4 + local_window], r0}

		;add my app panel
		s_call flow, create, {}, {[r4 + local_panel]}
		assert r0, !=, 0
		s_call flow, set_flow_flags, {r0, flow_flag_down | flow_flag_fillw}
		s_call flow, set_color, {r0, 0}
		s_call flow, add, {r0, [r4 + local_window_panel]}

		;add launch buttons to my app panel
		vp_rel launch_list, r0
		loop_start
			vp_cpy_ub [r0], r1
			breakif r1, ==, 0
			vp_cpy r0, [r4 + local_next]

			s_call button, create, {}, {[r4 + local_button]}
			assert r0, !=, 0
			s_call button, set_color, {r0, 0xffffff00}
			s_call string, create, {[r4 + local_next]}, {r0}
			assert r0, !=, 0
			s_call button, set_text, {[r4 + local_button], r0}
			s_call button, add, {r0, [r4 + local_panel]}
			s_call button, connect, {r0, :[r0 + button_pressed_signal], r4, $on_press}

			s_call sys_string, length, {[r4 + local_next]}, {r1}
			vp_lea [r0 + r1 + 1], r0
		loop_end

		;set to pref size
		m_call window, pref_size, {[r4 + local_window]}, {r10, r11}
		vp_add 64, r10
		s_call window, change, {r0, 400, 256, r10, r11}

		;set window owner
		s_call sys_task, tcb, {}, {r0}
		s_call window, set_owner, {[r4 + local_window], r0}

		;add to screen and dirty
		s_call gui_gui, add, {r0}
		s_call window, dirty_all, {r0}

		;app event loop
		loop_start
			s_call sys_mail, mymail, {}, {[r4 + local_last_event]}

			;dispatch event to view
			m_call view, event, {[r0 + ev_data_view], r0}

			;free event message
			s_call sys_mem, free, {[r4 + local_last_event]}
		loop_end

		;deref window
		s_call window, deref, {[r4 + local_window]}

		m_call obj, deinit, {r4}
		vp_add local_size, r4
		vp_ret

	on_press:
		;inputs
		;r0 = app local object
		;r1 = button object

		s_call button, get_text, {r1}, {r1}
		s_jmp sys_task, open_child, {:[r1 + string_data]}

	launch_list:
		db 'tests/farm', 0
		db 'tests/array', 0
		db 'tests/pipe', 0
		db 'tests/global', 0
		db 0

	fn_function_end
