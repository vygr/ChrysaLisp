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

		def_structure	local, obj
			def_long	local_last_event
			def_long	local_window
			def_long	local_window_panel
			def_long	local_flow_panel
			def_long	local_grid_panel
			def_long	local_display
			def_long	local_next
			def_long	local_button
			def_long	local_accum
			def_struct	local_buffer, string_buf
		def_structure_end

		;init app vars
		vp_sub local_size, r4
		static_bind class, obj, r1
		static_call obj, init, {r4, r1}
		assert r1, !=, 0
		vp_xor r0, r0
		vp_cpy r0, [r4 + local_accum]
		static_call sys_string, from_long, {r0, &[r4 + local_buffer], 10}

		;create my window
		static_call window, create, {}, {[r4 + local_window]}
		assert r0, !=, 0
		static_call window, get_panel, {r0}, {[r4 + local_window_panel]}
		static_call string, create, {"Calculator"}
		assert r0, !=, 0
		static_call window, set_title, {[r4 + local_window], r0}
		static_call string, create, {"Status Text"}
		assert r0, !=, 0
		static_call window, set_status, {[r4 + local_window], r0}

		;add my app flow panel
		static_call flow, create, {}, {[r4 + local_flow_panel]}
		assert r0, !=, 0
		static_call flow, set_flow_flags, {r0, flow_flag_down | flow_flag_fillw | flow_flag_lasth}
		static_call flow, set_color, {r0, 0x00000000}
		static_call flow, add, {r0, [r4 + local_window_panel]}

		;add my display label
		static_call label, create, {}, {[r4 +local_display]}
		assert r0, !=, 0
		static_call label, set_color, {r0, -1}
		static_call label, set_flow_flags, {r0, flow_flag_align_hright | flow_flag_align_vcenter}
		static_call label, set_font, {r0, "fonts/OpenSans-Regular.ttf", 24}
		static_call string, create, {"0"}
		assert r0, !=, 0
		static_call label, set_text, {[r4 + local_display], r0}
		static_call label, add, {r0, [r4 + local_flow_panel]}

		;add my app grid panel
		static_call grid, create, {}, {[r4 + local_grid_panel]}
		assert r0, !=, 0
		static_call grid, set_color, {r0, 0x00000000}
		static_call grid, set_grid, {r0, 4, 4}
		static_call grid, add, {r0, [r4 + local_flow_panel]}

		;add buttons to my grid panel
		vp_rel button_list, r0
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
			static_call button, set_flow_flags, {r0, flow_flag_align_hcenter | flow_flag_align_vcenter}
			static_call button, add, {r0, [r4 + local_grid_panel]}
			vp_rel on_press, r3
			static_call button, connect, {r0, &[r0 + button_pressed_signal], r4, r3}

			static_call sys_string, length, {[r4 + local_next]}, {r1}
			vp_lea [r0 + r1 + 1], r0
		loop_end

		;set to pref size
		method_call window, pref_size, {[r4 + local_window]}
		vp_cpy r10, r12
		vp_cpy r11, r13
		vp_shr 1, r12
		vp_shr 1, r13
		vp_add r12, r10
		vp_add r13, r11
		static_call window, change, {r0, 920, 48, r10, r11}

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

		def_structure	on_press
			def_long	on_press_inst
			def_long	on_press_button
			def_long	on_press_string1
			def_long	on_press_string2
		def_structure_end

		;save inputs
		vp_sub on_press_size, r4
		vp_cpy r0, [r4 + on_press_inst]
		vp_cpy r1, [r4 + on_press_button]

		static_call button, get_text, {r1}, {[r4 + on_press_string1]}

		vp_cpy [r4 + on_press_inst], r0
		vp_cpy [r0 + local_display], r0
		static_call label, get_text, {}, {[r4 + on_press_string2]}
		static_call string, add, {[r4 + on_press_string2], [r4 + on_press_string1]}
		assert r0, !=, 0
		vp_cpy r0, r1
		vp_cpy [r4 + on_press_inst], r0
		vp_cpy [r0 + local_display], r0
		static_call label, set_text
		static_call label, dirty

		static_call string, deref, {[r4 + on_press_string1]}
		static_call string, deref, {[r4 + on_press_string2]}

;		vp_cpy r1, r0
;		vp_cpy 10, r1
;		static_call sys_string, to_long
;		vp_cpy [r4], r6
;		vp_cpy r0, [r6 + local_accum]
;		vp_cpy [r6 + local_accum], r0
;		vp_cpy 123456789, r0
;		vp_lea [r6 + local_buffer], r1
;		vp_cpy 10, r2
;		static_call sys_string, from_long

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
