%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/gui.inc'
%include 'inc/string.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_button.inc'
%include 'class/class_string.inc'

	def_func apps/launcher/app

		struct myapp, obj
		ptr msg, window, window_panel, panel, button, string, pressed
		pubyte next
		ulong owner
		int width, height
		ubyte length

		;init app vars
		push_scope
		func_path class, obj
		func_call obj, init, {&myapp, @_function_}, {_}

		;create my window
		func_call window, create, {}, {window}
		func_call window, get_panel, {window}, {window_panel}
		func_call string, create_from_cstr, {"Launcher"}, {string}
		func_call window, set_title, {window, string}
		func_call string, create_from_cstr, {"Status Text"}, {string}
		func_call window, set_status, {window, string}

		;add my app panel
		func_call flow, create, {}, {panel}
		func_call flow, set_flow_flags, {panel, flow_flag_down | flow_flag_fillw}
		func_call flow, add_back, {panel, window_panel}

		;add launch buttons to my app panel
		assign {$launch_list}, {next}
		loop_while {*next}
			func_call button, create, {}, {button}
			func_call button, set_color, {button, 0xffffff00}
			func_call string, create_from_cstr, {next}, {string}
			func_call button, set_text, {button, string}
			func_call button, add_back, {button, panel}
			func_call button, sig_pressed, {button}, {pressed}
			func_call button, connect, {button, pressed, &myapp, $on_press}

			func_call sys_string, length, {next}, {length}
			assign {next + length + 1}, {next}
		loop_end

		;set to pref size
		virt_call window, pref_size, {window}, {width, height}
		func_call window, change, {window, 400, 256, width + 40, height}

		;set window owner
		func_call sys_task, tcb, {}, {owner}
		func_call window, set_owner, {window, owner}

		;add to screen and dirty
		func_call gui_gui, add, {window}
		func_call window, dirty_all, {window}

		;app event loop
		loop_start
			func_call sys_mail, mymail, {}, {msg}

			;dispatch event to view
			virt_call view, event, {msg->ev_msg_view, msg}

			;free event message
			func_call sys_mem, free, {msg}
		loop_end

		;deref window
		func_call window, deref, {window}
		virt_call obj, deinit, {&myapp}
		pop_scope
		return

	on_press:
		;inputs
		;r0 = app local object
		;r1 = button object

		ptr inst, button, string

		push_scope
		retire {r0, r1}, {inst, button}
		func_call button, get_text, {button}, {string}
		func_call sys_task, open, {string}, {_, _}
		func_call string, deref, {string}
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

	def_func_end
