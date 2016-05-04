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

		def_local local, obj
			def_local_long		last_event
			def_local_long		window
			def_local_long		window_panel
			def_local_long		panel
			def_local_long		next
			def_local_long		button
		def_local_end

		;init app vars
		vp_sub local_size, r4
		slot_function class, obj
		static_call obj, init, {r4, @_function_}, {r1}
		assert r1, !=, 0

		;create my window
		static_call window, create, {}, {.window}
		assert r0, !=, 0
		static_call window, get_panel, {r0}, {.window_panel}
		static_call string, create, {"Test Runner"}, {r0}
		assert r0, !=, 0
		static_call window, set_title, {.window, r0}
		static_call string, create, {"Status Text"}, {r0}
		assert r0, !=, 0
		static_call window, set_status, {.window, r0}

		;add my app panel
		static_call flow, create, {}, {.panel}
		assert r0, !=, 0
		static_call flow, set_flow_flags, {r0, flow_flag_down | flow_flag_fillw}
		static_call flow, set_color, {r0, 0}
		static_call flow, add, {r0, .window_panel}

		;add launch buttons to my app panel
		vp_rel launch_list, r0
		loop_start
			vp_cpy_ub [r0], r1
			breakif r1, ==, 0
			vp_cpy r0, .next

			static_call button, create, {}, {.button}
			assert r0, !=, 0
			static_call button, set_color, {r0, 0xffffff00}
			static_call string, create, {.next}, {r0}
			assert r0, !=, 0
			static_call button, set_text, {.button, r0}
			static_call button, add, {r0, .panel}
			static_call button, connect, {r0, :[r0 + button_pressed_signal], r4, $on_press}

			static_call sys_string, length, {.next}, {r1}
			vp_lea [r0 + r1 + 1], r0
		loop_end

		;set to pref size
		method_call window, pref_size, {.window}, {r10, r11}
		vp_add 64, r10
		static_call window, change, {r0, 400, 256, r10, r11}

		;set window owner
		static_call sys_task, tcb, {}, {r0}
		static_call window, set_owner, {.window, r0}

		;add to screen and dirty
		static_call gui_gui, add, {r0}
		static_call window, dirty_all, {r0}

		;app event loop
		loop_start
			static_call sys_mail, mymail, {}, {.last_event}

			;dispatch event to view
			method_call view, event, {[r0 + ev_data_view], r0}

			;free event message
			static_call sys_mem, free, {.last_event}
		loop_end

		;deref window
		static_call window, deref, {.window}

		method_call obj, deinit, {r4}
		vp_add local_size, r4
		vp_ret

	on_press:
		;inputs
		;r0 = app local object
		;r1 = button object

		static_call button, get_text, {r1}, {r1}
		static_jmp sys_task, open_child, {:[r1 + string_data]}

	launch_list:
		db 'tests/farm', 0
		db 'tests/array', 0
		db 'tests/pipe', 0
		db 'tests/global', 0
		db 0

	fn_function_end
