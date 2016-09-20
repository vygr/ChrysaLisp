%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/gui.inc'
%include 'inc/string.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_button.inc'
%include 'class/class_string.inc'

	def_function apps/launcher/app

		struct myapp, obj
		ptr msg, window, window_panel, panel, button, string, pressed
		pubyte next
		ulong owner
		int width, height
		ubyte length

		;init app vars
		push_scope
		slot_function class, obj
		static_call obj, init, {&myapp, @_function_}, {_}

		;create my window
		static_call window, create, {}, {window}
		static_call window, get_panel, {window}, {window_panel}
		static_call string, create_from_cstr, {"Launcher"}, {string}
		static_call window, set_title, {window, string}
		static_call string, create_from_cstr, {"Status Text"}, {string}
		static_call window, set_status, {window, string}

		;add my app panel
		static_call flow, create, {}, {panel}
		static_call flow, set_flow_flags, {panel, flow_flag_down | flow_flag_fillw}
		static_call flow, add_back, {panel, window_panel}

		;add launch buttons to my app panel
		assign {$launch_list}, {next}
		loop_while {*next}
			static_call button, create, {}, {button}
			static_call button, set_color, {button, 0xffffff00}
			static_call string, create_from_cstr, {next}, {string}
			static_call button, set_text, {button, string}
			static_call button, add_back, {button, panel}
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
			method_call view, event, {msg->ev_msg_view, msg}

			;free event message
			static_call sys_mem, free, {msg}
		loop_end

		;deref window
		static_call window, deref, {window}
		method_call obj, deinit, {&myapp}
		pop_scope
		return

	on_press:
		;inputs
		;r0 = app local object
		;r1 = button object

		ptr inst, button, string

		push_scope
		retire {r0, r1}, {inst, button}
		static_call button, get_text, {button}, {string}
		static_call sys_task, open, {string}, {_, _}
		static_call string, deref, {string}
		pop_scope
		return

	launch_list:
		db 'apps/netmon/app',0
		db 'apps/terminal/app', 0
		db 'apps/calculator/app',0
		db 'tests/farm', 0
		db 'tests/array', 0
		db 'tests/pipe', 0
		db 'tests/global', 0
		db 0

	def_function_end
