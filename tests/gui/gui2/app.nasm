%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui2/app

		struc app
			app_window:			resq 1
			app_last_x:			resq 1
			app_last_y:			resq 1
			app_last_buttons:	resq 1
			app_last_event:		resq 1
		endstruc

		;init app vars
		vp_sub app_size, r4
		vp_cpy 0, qword[r4 + app_last_buttons]

		;allocate view
		static_call view, create
		vp_cpy r0, [r4 + app_window]

		;fill in sizes etc
		vp_cpy 256, qword[r0 + view_x]
		vp_cpy 256, qword[r0 + view_y]
		vp_cpy 512, qword[r0 + view_w]
		vp_cpy 256, qword[r0 + view_h]
		vp_cpy 255, qword[r0 + view_red]
		vp_cpy 0, qword[r0 + view_green]
		vp_cpy 255, qword[r0 + view_blue]
		vp_cpy 255, qword[r0 + view_alpha]

		;set owner
		static_call task, tcb
		vp_cpy [r4 + app_window], r1
		vp_cpy r0, [r1 + view_tcb]

		;add to screen
		vp_cpy r1, r0
		static_call gui, add

		;set as opaque and dirty
		static_call view, opaque
		vp_cpy [r4 + app_window], r0
		static_call view, dirty

		;app event loop
		loop_start
			;read events
			static_call mail, mymail
			vp_cpy r0, [r4 + app_last_event]

			;what type of event ?
			vp_cpy [r0 + (ml_msg_data + ev_data_type)], r1
			switch
			case r1, ==, ev_type_mouse
				;so what state are we in ?
				vp_cpy [r0 + (ml_msg_data + ev_data_buttons)], r10
				if qword[r4 + app_last_buttons], !=, 0
					;was down previously
					if r10, !=, 0
						;is down now, so move
						vp_cpy [r0 + (ml_msg_data + ev_data_x)], r8
						vp_cpy [r0 + (ml_msg_data + ev_data_y)], r9
						vp_sub [r4 + app_last_x], r8
						vp_sub [r4 + app_last_y], r9
						vp_cpy [r0 + (ml_msg_data + ev_data_view)], r0
						vp_cpy [r0 + view_w], r10
						vp_cpy [r0 + view_h], r11
						static_call view, move
					else
						;is not down now, so release
						vp_cpy r10, [r4 + app_last_buttons]
					endif
				else
					;was not down previously
					if r10, !=, 0
						;is down now, so first down
						vp_cpy r10, [r4 + app_last_buttons]
						vp_cpy [r0 + (ml_msg_data + ev_data_rx)], r8
						vp_cpy [r0 + (ml_msg_data + ev_data_ry)], r9
						vp_cpy r8, [r4 + app_last_x]
						vp_cpy r9, [r4 + app_last_y]
					else
						;is not down now, so hover
					endif
				endif
				break
			default
			endswitch

			;free event message
			vp_cpy [r4 + app_last_event], r0
			static_call mem, free
		loop_end

		vp_add app_size, r4
		vp_ret

	fn_function_end
