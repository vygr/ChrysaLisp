%include 'inc/func.inc'
%include 'tests/gui/gui1/app.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/gui/gui1/child
		;monitor task

		ptr msg

		push_scope
		loop_start
			;read mail command
			static_call sys_mail, mymail, {}, {msg}
			breakif {!msg->sample_mail_command}

			;sample command
			static_call sys_task, count, {}, {msg->sample_mail_task_count}
			assign {msg->sample_mail_reply_id.mb_mbox}, {msg->ml_msg_dest.mb_mbox}
			assign {msg->sample_mail_reply_id.mb_cpu}, {msg->ml_msg_dest.mb_cpu}
			static_call sys_mail, send, {msg}

			;be friendly
			static_call sys_task, yield
		loop_end
		static_call sys_mem, free, {msg}
		pop_scope
		vp_ret

	fn_function_end
