%include 'inc/func.inc'
%include 'tests/gui/gui1/app.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui1/child
		;monitor task

		loop_start
			;read mail command
			s_call sys_mail, mymail, {}, {r0}

			vp_cpy [r0 + sample_mail_command], r1
			if r1, ==, 0
				;exit command
				s_call sys_mem, free, {r0}
				vp_ret
			else
				;sample command
				vp_cpy r0, r1
				s_call sys_task, count, {}, {[r1 + sample_mail_task_count]}
				vp_cpy [r1 + sample_mail_reply_id], r2
				vp_cpy [r1 + sample_mail_reply_id + 8], r3
				vp_cpy r2, [r1 + ml_msg_dest]
				vp_cpy r3, [r1 + ml_msg_dest + 8]
				s_call sys_mail, send, {r1}
			endif

			;be friendly
			s_call sys_task, yield
		loop_end

	fn_function_end
