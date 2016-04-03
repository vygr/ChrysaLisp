%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'inc/gui.inc'
%include 'inc/string.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'
%include 'class/class_grid.inc'
%include 'class/class_button.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui3/app

		%assign string_buf_size 32

		def_structure	local, obj
			def_long	local_last_event
			def_long	local_window
			def_long	local_window_panel
			def_long	local_flow_panel
			def_long	local_grid_panel
			def_long	local_display
			def_long	local_next
			def_long	local_accum
			def_struct	local_buffer, string_buf
		def_structure_end

		;init app vars
		vp_sub local_size, r4
		vp_cpy r4, r0
		static_bind class, obj, r1
		static_call obj, init
		fn_assert r1, !=, 0
		vp_xor r0, r0
		vp_cpy r0, [r4 + local_accum]
		vp_lea [r4 + local_buffer], r1
		vp_cpy 10, r2
		static_call sys_string, from_long

		;create my window
		static_call window, create
		fn_assert r0, !=, 0
		vp_cpy r0, [r4 + local_window]
		static_call window, get_panel
		vp_cpy r1, [r4 + local_window_panel]
		fn_string 'Calculator', r1
		static_call window, set_title
		fn_string 'Status Text', r1
		static_call window, set_status

		;add my app flow panel
		static_call flow, create
		fn_assert r0, !=, 0
		vp_cpy r0, [r4 + local_flow_panel]
		vp_cpy flow_flag_down | flow_flag_fillw | flow_flag_lasth, r1
		static_call flow, set_flow_flags
		vp_xor r1, r1
		static_call flow, set_color
		vp_cpy [r4 + local_window_panel], r1
		static_call flow, add

		;add my display label
		static_call label, create
		fn_assert r0, !=, 0
		vp_cpy r0, [r4 +local_display]
		vp_cpy -1, r1
		static_call label, set_color
		vp_cpy flow_flag_align_hright | flow_flag_align_vcenter, r1
		static_call label, set_flow_flags
		fn_string 'fonts/OpenSans-Regular.ttf', r1
		vp_cpy 24, r2
		static_call label, set_font
		vp_lea [r4 + local_buffer], r1
		static_call label, set_text
		vp_cpy [r4 + local_flow_panel], r1
		static_call label, add

		;add my app grid panel
		static_call grid, create
		fn_assert r0, !=, 0
		vp_cpy r0, [r4 + local_grid_panel]
		vp_xor r1, r1
		static_call grid, set_color
		vp_cpy 4, r10
		vp_cpy 4, r11
		static_call grid, set_grid
		vp_cpy [r4 + local_flow_panel], r1
		static_call grid, add

		;add buttons to my grid panel
		vp_rel button_list, r0
		loop_start
			vp_xor r1, r1
			vp_cpy_b [r0], r1
			breakif r1, ==, 0
			vp_cpy r0, [r4 + local_next]

			static_call button, create
			fn_assert r0, !=, 0
			vp_cpy 0xffffff00, r1
			static_call button, set_color
			vp_cpy [r4 + local_next], r1
			static_call button, set_text
			vp_cpy flow_flag_align_hcenter | flow_flag_align_vcenter, r1
			static_call button, set_flow_flags
			vp_cpy [r4 + local_grid_panel], r1
			static_call button, add
			vp_lea [r0 + button_pressed_signal], r1
			vp_cpy r4, r2
			vp_rel on_press, r3
			static_call button, connect

			vp_cpy [r4 + local_next], r0
			static_call sys_string, length
			vp_lea [r0 + r1 + 1], r0
		loop_end

		;set to pref size
		vp_cpy [r4 + local_window], r0
		method_call window, pref_size
		vp_cpy r10, r12
		vp_cpy r11, r13
		vp_shr 1, r12
		vp_shr 1, r13
		vp_cpy 920, r8
		vp_cpy 48, r9
		vp_add r12, r10
		vp_add r13, r11
		static_call window, change

		;set window owner
		static_call sys_task, tcb
		vp_cpy r0, r1
		vp_cpy [r4 + local_window], r0
		static_call window, set_owner

		;add to screen and dirty
		static_call gui_gui, add
		static_call window, dirty_all

		;app event loop
		loop_start
			static_call sys_mail, mymail
			vp_cpy r0, [r4 + local_last_event]

			;dispatch event to view
			vp_cpy r0, r1
			vp_cpy [r1 + ev_data_view], r0
			method_call view, event

			;free event message
			vp_cpy [r4 + local_last_event], r0
			static_call sys_mem, free
		loop_end

		;deref window
		vp_cpy [r4 + local_window], r0
		static_call window, deref

		vp_cpy r4, r0
		method_call obj, deinit
		vp_add local_size, r4
		vp_ret

	on_press:
		;inputs
		;r0 = app local object
		;r1 = button object

		vp_push r1, r0

		vp_cpy r1, r0
		static_call button, get_text

		vp_cpy r1, r0
		vp_cpy 10, r1
		static_call sys_string, to_long

		vp_cpy [r4], r6
		vp_cpy r0, [r6 + local_accum]

		vp_cpy [r6 + local_accum], r0
		vp_cpy 123456789, r0
		vp_lea [r6 + local_buffer], r1
		vp_cpy 10, r2
		static_call sys_string, from_long

		vp_cpy [r6 + local_display], r0
		vp_lea [r6 + local_buffer], r1
		static_call label, set_text
		static_call label, dirty

		vp_pop r1, r0
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
