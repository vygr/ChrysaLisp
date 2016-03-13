%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_window.inc'
%include 'class/class_button.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui1/app

		def_structure app
			def_long	app_last_event
			def_long	app_window
		def_structure_end

		;init app vars
		vp_sub app_size, r4

		;create my window
		static_call window, create
		vp_cpy r0, [r4 + app_window]

		;set owner
		static_call task, tcb
		vp_cpy [r4 + app_window], r1
		vp_cpy r0, [r1 + view_tcb]

		;add 4 buttons to panel
		static_call button, create
		vp_cpy 255, qword[r0 + view_red]
		vp_cpy 255, qword[r0 + view_green]
		vp_cpy 0, qword[r0 + view_blue]
		vp_cpy 192, qword[r0 + view_alpha]
		vp_cpy [r4 + app_window], r1
		vp_cpy [r1 + window_panel], r1
		static_call view, add
		static_call button, create
		vp_cpy 0, qword[r0 + view_red]
		vp_cpy 255, qword[r0 + view_green]
		vp_cpy 255, qword[r0 + view_blue]
		vp_cpy 192, qword[r0 + view_alpha]
		vp_cpy [r4 + app_window], r1
		vp_cpy [r1 + window_panel], r1
		static_call view, add
		static_call button, create
		vp_cpy 255, qword[r0 + view_red]
		vp_cpy 0, qword[r0 + view_green]
		vp_cpy 255, qword[r0 + view_blue]
		vp_cpy 192, qword[r0 + view_alpha]
		vp_cpy [r4 + app_window], r1
		vp_cpy [r1 + window_panel], r1
		static_call view, add
		static_call button, create
		vp_cpy 0, qword[r0 + view_red]
		vp_cpy 0, qword[r0 + view_green]
		vp_cpy 0, qword[r0 + view_blue]
		vp_cpy 192, qword[r0 + view_alpha]
		vp_cpy [r4 + app_window], r1
		vp_cpy [r1 + window_panel], r1
		static_call view, add

		;set to pref size
		vp_cpy [r4 + app_window], r0
		method_call window, pref_size
		vp_cpy [r4 + app_window], r0
		vp_cpy 32, r8
		vp_cpy 32, r9
		vp_mul 2, r10
		vp_mul 2, r11
		static_call window, change

		;add to screen and dirty
		vp_cpy [r4 + app_window], r0
		static_call gui, add
		static_call window, dirty_all

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
