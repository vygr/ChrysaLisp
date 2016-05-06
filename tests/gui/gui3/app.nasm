%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/gui.inc'
%include 'inc/string.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_grid.inc'
%include 'class/class_button.inc'
%include 'class/class_string.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui3/app

		def_structure shared, obj
			long shared_display
		def_structure_end

		struct myapp, shared
		long last_event
		long window
		long window_panel
		long flow_panel
		long grid_panel
		long next
		long button
		long owner
		long pressed
		long width
		long height
		long string
		long length

		;init app vars
		push_scope
		slot_function class, obj
		static_call obj, init, {&myapp, @_function_}, {_}

		;create my window
		static_call window, create, {}, {window}
		static_call window, get_panel, {window}, {window_panel}
		static_call string, create, {"Calculator"}, {string}
		static_call window, set_title, {window, string}
		static_call string, create, {"Status Text"}, {string}
		static_call window, set_status, {window, string}

		;add my app flow panel
		static_call flow, create, {}, {flow_panel}
		static_call flow, set_flow_flags, {flow_panel, flow_flag_down | flow_flag_fillw | flow_flag_lasth}
		static_call flow, set_color, {flow_panel, 0x00000000}
		static_call flow, add, {flow_panel, window_panel}

		;add my display label
		static_call label, create, {}, {myapp.shared_display}
		static_call label, set_color, {myapp.shared_display, 0xffffffff}
		static_call label, set_flow_flags, {myapp.shared_display, flow_flag_align_hright | flow_flag_align_vcenter}
		static_call label, set_font, {myapp.shared_display, "fonts/OpenSans-Regular.ttf", 24}
		static_call string, create, {"0"}, {string}
		static_call label, set_text, {myapp.shared_display, string}
		static_call label, add, {myapp.shared_display, flow_panel}

		;add my app grid panel
		static_call grid, create, {}, {grid_panel}
		static_call grid, set_color, {grid_panel, 0x00000000}
		static_call grid, set_grid, {grid_panel, 4, 4}
		static_call grid, add, {grid_panel, flow_panel}

		;add buttons to my grid panel
		eval {$button_list}, {r0}
		loop_start
			vp_cpy_ub [r0], r1
			breakif r1, ==, 0
			retire {r0}, {next}

			static_call button, create, {}, {button}
			static_call button, set_color, {button, 0xffffff00}
			static_call string, create, {next}, {string}
			static_call button, set_text, {button, string}
			static_call button, set_flow_flags, {button, flow_flag_align_hcenter | flow_flag_align_vcenter}
			static_call button, add, {button, grid_panel}
			static_call button, sig_pressed, {button}, {pressed}
			static_call button, connect, {button, pressed, &myapp, $on_press}

			static_call sys_string, length, {next}, {length}
			eval {next + length + 1}, {r0}
		loop_end

		;set to pref size
		method_call window, pref_size, {window}, {width, height}
		static_call window, change, {window, 920, 48, width + (width >> 1), height + (height >> 1)}

		;set window owner
		static_call sys_task, tcb, {}, {owner}
		static_call window, set_owner, {window, owner}

		;add to screen and dirty
		static_call gui_gui, add, {window}
		static_call window, dirty_all, {window}

		;app event loop
		loop_start
			static_call sys_mail, mymail, {}, {last_event}

			;dispatch event to view
			method_call view, event, {last_event->ev_data_view, last_event}

			;free event message
			static_call sys_mem, free, {last_event}
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
		long string1
		long string2
		long string

		;save inputs
		push_scope
		retire {r0, r1}, {inst, button}

		static_call button, get_text, {button}, {string1}
		static_call label, get_text, {inst->shared_display}, {string2}
		static_call string, add, {string2, string1}, {string}
		static_call label, set_text, {inst->shared_display, string}
		static_call label, dirty, {inst->shared_display}
		static_call string, deref, {string1}
		static_call string, deref, {string2}

		pop_scope
		vp_ret

	button_list:
		db '7', 0
		db '8', 0
		db '9', 0
		db '/', 0
		db '4', 0
		db '5', 0
		db '6', 0
		db '*', 0
		db '1', 0
		db '2', 0
		db '3', 0
		db '-', 0
		db '0', 0
		db '=', 0
		db 'AC', 0
		db '+', 0
		db 0

	fn_function_end
