%include 'inc/func.inc'
%include 'tests/gui/gui1/app.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui1/child
		;monitor task

		loop_start
			;read mail command
			static_call sys_mail, mymail

			vp_cpy [r0 + sample_mail_command], r1
			if r1, ==, 0
				;exit command
				static_call sys_mem, free
				vp_ret
			else
				;sample command
				vp_cpy r0, r1
				static_call sys_task, count
				vp_cpy r0, [r1 + sample_mail_task_count]
				vp_cpy [r1 + sample_mail_reply_id], r2
				vp_cpy [r1 + sample_mail_reply_id + 8], r3
				vp_cpy r2, [r1 + ml_msg_dest]
				vp_cpy r3, [r1 + ml_msg_dest + 8]
				vp_cpy r1, r0
				static_call sys_mail, send
			endif

			;be friendly
			static_call sys_task, yield
		loop_end

	fn_function_end
