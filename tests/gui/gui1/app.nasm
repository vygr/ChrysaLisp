%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_flow.inc'
%include 'class/class_title.inc'
%include 'tests/gui/gui1/class_window.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui1/app

		def_structure app
			def_long	app_window
			def_long	app_last_event
		def_structure_end

		;init app vars
		vp_sub app_size, r4

		;create my window
		static_call flow, create
		vp_cpy r0, [r4 + app_window]
		vp_cpy flow_flag_down, qword[r0 + flow_flags]
		vp_cpy 128, qword[r0 + view_x]
		vp_cpy 128, qword[r0 + view_y]
		vp_cpy 128, qword[r0 + view_w]
		vp_cpy 128, qword[r0 + view_h]
		vp_cpy 255, qword[r0 + view_red]
		vp_cpy 255, qword[r0 + view_green]
		vp_cpy 0, qword[r0 + view_blue]
		vp_cpy 192, qword[r0 + view_alpha]

		;set owner
		static_call task, tcb
		vp_cpy [r4 + app_window], r1
		vp_cpy r0, [r1 + view_tcb]

		;add 4 titles
		static_call title, create
		vp_cpy 255, qword[r0 + view_red]
		vp_cpy 0, qword[r0 + view_green]
		vp_cpy 0, qword[r0 + view_blue]
		vp_cpy 192, qword[r0 + view_alpha]
		vp_cpy [r4 + app_window], r1
		static_call view, add

		static_call title, create
		vp_cpy 0, qword[r0 + view_red]
		vp_cpy 255, qword[r0 + view_green]
		vp_cpy 0, qword[r0 + view_blue]
		vp_cpy 192, qword[r0 + view_alpha]
		vp_cpy [r4 + app_window], r1
		static_call view, add

		static_call title, create
		vp_cpy 0, qword[r0 + view_red]
		vp_cpy 0, qword[r0 + view_green]
		vp_cpy 255, qword[r0 + view_blue]
		vp_cpy 192, qword[r0 + view_alpha]
		vp_cpy [r4 + app_window], r1
		static_call view, add

		static_call title, create
		vp_cpy 255, qword[r0 + view_red]
		vp_cpy 255, qword[r0 + view_green]
		vp_cpy 255, qword[r0 + view_blue]
		vp_cpy 192, qword[r0 + view_alpha]
		vp_cpy [r4 + app_window], r1
		static_call view, add

		;get pref size and layout
		vp_cpy [r4 + app_window], r0
		method_call view, pref_size
		vp_cpy [r4 + app_window], r0
		vp_cpy r10, [r0 + view_w]
		vp_cpy r11, [r0 + view_h]
		method_call view, layout

		;add to screen and dirty
		vp_cpy [r4 + app_window], r0
		static_call gui, add
		static_call view, dirty_all

		;app event loop
		loop_start
			;read events
			static_call mail, mymail
			vp_cpy r0, [r4 + app_last_event]

			;dispatch event to view
			vp_cpy r0, r1
			vp_cpy [r1 + (ml_msg_data + ev_data_view)], r0
			method_call view, event

			;free event message
			vp_cpy [r4 + app_last_event], r0
			static_call mem, free
		loop_end

		vp_add app_size, r4
		vp_ret

	fn_function_end
