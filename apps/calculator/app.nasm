%include 'inc/func.ninc'
%include 'inc/mail.ninc'
%include 'inc/gui.ninc'
%include 'inc/string.ninc'
%include 'class/class_window.ninc'
%include 'class/class_flow.ninc'
%include 'class/class_grid.ninc'
%include 'class/class_button.ninc'
%include 'class/class_string.ninc'

def_func apps/calculator/app

	buffer_size equ 32

	def_struct shared, obj
		ptr shared_display
		long shared_accum
		long shared_value
		struct shared_buffer, buffer
		ubyte shared_last_op
		ubyte shared_last_flag
	def_struct_end

	struct myapp, shared
	ptr msg, window, window_panel, flow_panel, grid_panel, button, pressed, string
	pubyte next
	ulong owner
	int width, height
	ubyte length

	;init app vars
	push_scope
	func_path class, obj
	func_call obj, init, {&myapp, @_function_}, {_}
	assign {0}, {myapp.shared_accum}
	assign {0}, {myapp.shared_value}
	assign {0}, {myapp.shared_last_op}
	assign {0}, {myapp.shared_last_flag}

	;create my window
	func_call window, create, {}, {window}
	func_call window, get_panel, {window}, {window_panel}
	func_call string, create_from_cstr, {"Calculator"}, {string}
	func_call window, set_title, {window, string}
	func_call string, create_from_cstr, {"Status Text"}, {string}
	func_call window, set_status, {window, string}

	;add my app flow panel
	func_call flow, create, {}, {flow_panel}
	func_call flow, set_flow_flags, {flow_panel, flow_flag_down | flow_flag_fillw | flow_flag_lasth}
	func_call flow, add_back, {flow_panel, window_panel}

	;add my display label
	func_call label, create, {}, {myapp.shared_display}
	func_call label, set_color, {myapp.shared_display, 0xffffffff}
	func_call label, set_flow_flags, {myapp.shared_display, flow_flag_align_hright | flow_flag_align_vcenter}
	func_call label, set_font, {myapp.shared_display, "fonts/OpenSans-Regular.ttf", 24}
	func_call string, create_from_cstr, {"0"}, {string}
	func_call label, set_text, {myapp.shared_display, string}
	func_call label, add_back, {myapp.shared_display, flow_panel}

	;add my app grid panel
	func_call grid, create, {}, {grid_panel}
	func_call grid, set_grid, {grid_panel, 4, 4}
	func_call grid, add_back, {grid_panel, flow_panel}

	;add buttons to my grid panel
	assign {$button_list}, {next}
	loop_while {*next}
		func_call button, create, {}, {button}
		func_call button, set_color, {button, 0xffffff00}
		func_call string, create_from_cstr, {next}, {string}
		func_call button, set_text, {button, string}
		func_call button, set_flow_flags, {button, flow_flag_align_hcenter | flow_flag_align_vcenter}
		func_call button, add_back, {button, grid_panel}
		func_call button, sig_pressed, {button}, {pressed}
		func_call button, connect, {button, pressed, &myapp, $on_press}

		func_call sys_string, length, {next}, {length}
		assign {next + length + 1}, {next}
	loop_end

	;set to pref size
	virt_call window, pref_size, {window}, {width, height}
	func_call window, change, {window, 920, 48, width + (width >> 1), height + (height >> 1)}

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

	const char_zero, '0'
	const char_nine, '9'
	const char_equal, '='
	const char_plus, '+'
	const char_minus, '-'
	const char_multiply, '*'
	const char_divide, '/'

	ptr inst, button, button_string, display_string, string, string1, string2
	pubyte charp
	ubyte char

	;save inputs
	push_scope
	retire {r0, r1}, {inst, button}
	func_call button, get_text, {button}, {button_string}
	vpif {button_string->string_length == 2}
		;AC
		func_call string, create_from_cstr, {"0"}, {string}
		func_call label, set_text, {inst->shared_display, string}
		assign {0}, {inst->shared_accum}
		assign {0}, {inst->shared_value}
		assign {0}, {inst->shared_last_op}
		assign {0}, {inst->shared_last_flag}
	else
		func_call label, get_text, {inst->shared_display}, {display_string}
		assign {&button_string->string_data}, {charp}
		assign {*charp}, {char}
		vpif {char >= char_zero && char <= char_nine}
			;numeral
			assign {&display_string->string_data}, {charp}
			assign {*charp}, {char}
			vpif {char == char_zero || inst->shared_last_flag == 0}
				;clear it
				func_call string, deref, {display_string}
				func_call string, create_from_cstr, {""}, {display_string}
				assign {1}, {inst->shared_last_flag}
			endif
			;append numeral
			func_call string, append, {display_string, button_string}, {string}
			func_call sys_string, to_long, {&string->string_data, 10}, {inst->shared_value}
		else
			;operator
			vpif {inst->shared_last_op == char_plus}
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
				assign {inst->shared_accum // inst->shared_value}, {inst->shared_accum}
			else
				;equals
				assign {inst->shared_value}, {inst->shared_accum}
			endif
			vpif {char != char_equal}
				assign {char}, {inst->shared_last_op}
			endif
			assign {0}, {inst->shared_last_flag}
			vpif {inst->shared_accum < 0}
				;negative accum
				func_call sys_string, from_long, {-inst->shared_accum, &inst->shared_buffer, 10}
				func_call string, create_from_cstr, {"-"}, {string1}
				func_call string, create_from_cstr, {&inst->shared_buffer}, {string2}
				func_call string, append, {string1, string2}, {string}
				func_call string, deref, {string1}
				func_call string, deref, {string2}
			else
				;positive accum
				func_call sys_string, from_long, {inst->shared_accum, &inst->shared_buffer, 10}
				func_call string, create_from_cstr, {&inst->shared_buffer}, {string}
			endif
		endif
		func_call label, set_text, {inst->shared_display, string}
		func_call string, deref, {display_string}
	endif
	func_call string, deref, {button_string}
	func_call label, dirty, {inst->shared_display}
	pop_scope
	return

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

def_func_end
