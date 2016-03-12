%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'tests/gui/gui2/class_window.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui2/app

		def_structure app
			def_long	app_window
			def_long	app_last_event
		def_structure_end

		;init app vars
		vp_sub app_size, r4

		;create my window
		static_call window, create
		vp_cpy r0, [r4 + app_window]

		;fill in sizes etc
		vp_cpy 256, qword[r0 + view_x]
		vp_cpy 256, qword[r0 + view_y]
		vp_cpy 512, qword[r0 + view_w]
		vp_cpy 256, qword[r0 + view_h]
		vp_cpy 0, qword[r0 + view_red]
		vp_cpy 255, qword[r0 + view_green]
		vp_cpy 255, qword[r0 + view_blue]
		vp_cpy 255, qword[r0 + view_alpha]

		;set owner
		static_call task, tcb
		vp_cpy [r4 + app_window], r1
		vp_cpy r0, [r1 + view_tcb]

		;add to screen and dirty and opaque
		vp_cpy r1, r0
		static_call gui, add
		static_call view, dirty
		vp_cpy [r4 + app_window], r0
		static_call view, opaque

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
