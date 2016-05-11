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

		buffer_size equ 32

		def_structure shared, obj
			long shared_display
			long shared_accum
			long shared_value
			struct shared_buffer, buffer
			ubyte shared_last_op
			ubyte shared_last_flag
		def_structure_end

		struct myapp, shared
		long msg
		long window
		long window_panel
		long flow_panel
		long grid_panel
		pubyte next
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
		assign {0}, {myapp.shared_accum}
		assign {0}, {myapp.shared_value}
		assign {0}, {myapp.shared_last_op}
		assign {0}, {myapp.shared_last_flag}

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
		assign {$button_list}, {next}
		loop_start
			breakifnot {*next}

			static_call button, create, {}, {button}
			static_call button, set_color, {button, 0xffffff00}
			static_call string, create, {next}, {string}
			static_call button, set_text, {button, string}
			static_call button, set_flow_flags, {button, flow_flag_align_hcenter | flow_flag_align_vcenter}
			static_call button, add, {button, grid_panel}
			static_call button, sig_pressed, {button}, {pressed}
			static_call button, connect, {button, pressed, &myapp, $on_press}

			static_call sys_string, length, {next}, {length}
			assign {next + length + 1}, {next}
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

		const char_zero, 48
		const char_nine, 57
		const char_equal, 61
		const char_plus, 43
		const char_minus, 45
		const char_multiply, 42
		const char_divide, 47

		long inst
		long button
		long button_string
		long display_string
		long string
		long string1
		long string2
		pubyte charp
		ubyte char

		;save inputs
		push_scope
		retire {r0, r1}, {inst, button}
		static_call button, get_text, {button}, {button_string}
		if {button_string->string_length == 2}
			;AC
			static_call string, create, {"0"}, {string}
			static_call label, set_text, {inst->shared_display, string}
			assign {0}, {inst->shared_accum}
			assign {0}, {inst->shared_value}
			assign {0}, {inst->shared_last_op}
			assign {0}, {inst->shared_last_flag}
		else
			static_call label, get_text, {inst->shared_display}, {display_string}
			assign {&button_string->string_data}, {charp}
			assign {*charp}, {char}
			if {char >= char_zero && char <= char_nine}
				;numeral
				assign {&display_string->string_data}, {charp}
				assign {*charp}, {char}
				if {char == char_zero || inst->shared_last_flag == 0}
					;clear it
					static_call string, deref, {display_string}
					static_call string, create, {""}, {display_string}
					assign {1}, {inst->shared_last_flag}
				endif
				;append numeral
				static_call string, add, {display_string, button_string}, {string}
				static_call sys_string, to_long, {&string->string_data, 10}, {inst->shared_value}
			else
				;operator
				if {inst->shared_last_op == char_plus}
					;+
					assign {inst->shared_accum + inst->shared_value}, {inst->shared_accum}
				elseif {inst->shared_last_op == char_minus}
					;-
					assign {inst->shared_accum - inst->shared_value}, {inst->shared_accum}
				elseif {inst->shared_last_op == char_multiply}
					;*
					assign {inst->shared_accum * inst->shared_value}, {inst->shared_accum}
				elseif {inst->shared_last_op == char_divide && inst->shared_value != 0}
					;/
					assign {inst->shared_accum / inst->shared_value}, {inst->shared_accum}
				else
					;equals
					assign {inst->shared_value}, {inst->shared_accum}
				endif
				if {char != char_equal}
					assign {char}, {inst->shared_last_op}
				endif
				assign {0}, {inst->shared_last_flag}
				if {inst->shared_accum < 0}
					;negative accum
					static_call sys_string, from_long, {-inst->shared_accum, &inst->shared_buffer, 10}
					static_call string, create, {"-"}, {string1}
					static_call string, create, {&inst->shared_buffer}, {string2}
					static_call string, add, {string1, string2}, {string}
					static_call string, deref, {string1}
					static_call string, deref, {string2}
				else
					;positive accum
					static_call sys_string, from_long, {inst->shared_accum, &inst->shared_buffer, 10}
					static_call string, create, {&inst->shared_buffer}, {string}
				endif
			endif
			static_call label, set_text, {inst->shared_display, string}
			static_call string, deref, {display_string}
		endif
		static_call string, deref, {button_string}
		static_call label, dirty, {inst->shared_display}
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
