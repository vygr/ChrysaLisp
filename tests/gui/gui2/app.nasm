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

		struct myapp, obj
		long msg
		long window
		long window_panel
		long panel
		pubyte next
		long button
		long string
		long length
		long pressed
		long width
		long height
		long owner

		;init app vars
		push_scope
		slot_function class, obj
		static_call obj, init, {&myapp, @_function_}, {_}

		;create my window
		static_call window, create, {}, {window}
		static_call window, get_panel, {window}, {window_panel}
		static_call string, create, {"Test Runner"}, {string}
		static_call window, set_title, {window, string}
		static_call string, create, {"Status Text"}, {string}
		static_call window, set_status, {window, string}

		;add my app panel
		static_call flow, create, {}, {panel}
		static_call flow, set_flow_flags, {panel, flow_flag_down | flow_flag_fillw}
		static_call flow, set_color, {panel, 0}
		static_call flow, add, {panel, window_panel}

		;add launch buttons to my app panel
		assign {$launch_list}, {next}
		loop_start
			breakifnot {*next}

			static_call button, create, {}, {button}
			static_call button, set_color, {button, 0xffffff00}
			static_call string, create, {next}, {string}
			static_call button, set_text, {button, string}
			static_call button, add, {button, panel}
			static_call button, sig_pressed, {button}, {pressed}
			static_call button, connect, {button, pressed, &myapp, $on_press}

			static_call sys_string, length, {next}, {length}
			assign {next + length + 1}, {next}
		loop_end

		;set to pref size
		method_call window, pref_size, {window}, {width, height}
		static_call window, change, {window, 400, 256, width + 40, height}

		;set window owner
		static_call sys_task, tcb, {}, {owner}
		static_call window, set_owner, {window, owner}

		;add to screen and dirty
		static_call gui_gui, add, {window}
		static_call window, dirty_all, {window}

		;app event loop
		loop_start
			static_call sys_mail, mymail, {}, {msg}

			;dispatch event to view
			method_call view, event, {msg->ev_data_view, msg}

			;free event message
			static_call sys_mem, free, {msg}
		loop_end

		;deref window
		static_call window, deref, {window}
		method_call obj, deinit, {&myapp}
		pop_scope
		vp_ret

	on_press:
		;inputs
		;r0 = app local object
		;r1 = button object

		long inst
		long button
		long string

		push_scope
		retire {r0, r1}, {inst, button}
		static_call button, get_text, {button}, {string}
		static_call sys_task, open_child, {&string->string_data}, {_, _}
		static_call string, deref, {string}
		pop_scope
		vp_ret

	launch_list:
		db 'tests/farm', 0
		db 'tests/array', 0
		db 'tests/pipe', 0
		db 'tests/global', 0
		db 0

	fn_function_end
