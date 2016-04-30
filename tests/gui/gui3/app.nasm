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

		string_buf_size	equ 32

		def_local local, obj
			def_local_long		last_event
			def_local_long		window
			def_local_long		window_panel
			def_local_long		flow_panel
			def_local_long		grid_panel
			def_local_long		display
			def_local_long		next
			def_local_long		button
			def_local_struct	buffer, string_buf

			def_local_long		owner
			def_local_long		pressed
			def_local_long		width
			def_local_long		height
			def_local_long		string
			def_local_long		length
		def_local_end

		;init app vars
		vp_sub local_size, r4
		slot_function class, obj
		s_call obj, init, {r4, @_function_}, {_}
		s_call sys_string, from_long, {0, :.buffer, 10}

		;create my window
		s_call window, create, {}, {.window}
		s_call window, get_panel, {.window}, {.window_panel}
		s_call string, create, {"Calculator"}, {.string}
		s_call window, set_title, {.window, .string}
		s_call string, create, {"Status Text"}, {.string}
		s_call window, set_status, {.window, .string}

		;add my app flow panel
		s_call flow, create, {}, {.flow_panel}
		s_call flow, set_flow_flags, {.flow_panel, flow_flag_down | flow_flag_fillw | flow_flag_lasth}
		s_call flow, set_color, {.flow_panel, 0x00000000}
		s_call flow, add, {.flow_panel, .window_panel}

		;add my display label
		s_call label, create, {}, {.display}
		s_call label, set_color, {.display, 0xffffffff}
		s_call label, set_flow_flags, {.display, flow_flag_align_hright | flow_flag_align_vcenter}
		s_call label, set_font, {.display, "fonts/OpenSans-Regular.ttf", 24}
		s_call string, create, {"0"}, {.string}
		s_call label, set_text, {.display, .string}
		s_call label, add, {.display, .flow_panel}

		;add my app grid panel
		s_call grid, create, {}, {.grid_panel}
		s_call grid, set_color, {.grid_panel, 0x00000000}
		s_call grid, set_grid, {.grid_panel, 4, 4}
		s_call grid, add, {.grid_panel, .flow_panel}

		;add buttons to my grid panel
		assign {$button_list}, {r0}
		loop_start
			vp_xor r1, r1
			vp_cpy_b [r0], r1
			breakif r1, ==, 0
			vp_cpy r0, .next

			s_call button, create, {}, {.button}
			s_call button, set_color, {.button, 0xffffff00}
			s_call string, create, {.next}, {.string}
			s_call button, set_text, {.button, .string}
			s_call button, set_flow_flags, {.button, flow_flag_align_hcenter | flow_flag_align_vcenter}
			s_call button, add, {.button, .grid_panel}
			s_call button, sig_pressed, {.button}, {.pressed}
			s_call button, connect, {.button, .pressed, r4, $on_press}

			s_call sys_string, length, {.next}, {.length}
			assign {.next + .length + 1}, {r0}
		loop_end

		;set to pref size
		method_call window, pref_size, {.window}, {.width, .height}
		s_call window, change, {.window, 920, 48, .width / 2 + .width, .height / 2 + .height}

		;set window owner
		s_call sys_task, tcb, {}, {.owner}
		s_call window, set_owner, {.window, .owner}

		;add to screen and dirty
		s_call gui_gui, add, {.window}
		s_call window, dirty_all, {.window}

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

		def_local on_press
			def_local_long	inst
			def_local_long	button
			def_local_long	string1
			def_local_long	string2
			def_local_long	string
			def_local_long	display
		def_local_end

		;save inputs
		vp_sub on_press_size, r4
		vp_cpy [r0 + local_display], r2
		set_src r0, r1, r2
		set_dst .inst, .button, .display
		map_src_to_dst

		s_call button, get_text, {.button}, {.string1}
		s_call label, get_text, {.display}, {.string2}
		s_call string, add, {.string2, .string1}, {.string}
		s_call label, set_text, {.display, .string}
		s_call label, dirty, {.display}
		s_call string, deref, {.string1}
		s_call string, deref, {.string2}

		vp_add on_press_size, r4
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
