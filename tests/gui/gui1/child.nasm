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
			breakif {!msg->sample_msg_command}

			;sample command
			static_call sys_task, count, {}, {msg->sample_msg_task_count}
			assign {msg->sample_msg_reply_id.id_mbox}, {msg->msg_dest.id_mbox}
			assign {msg->sample_msg_reply_id.id_cpu}, {msg->msg_dest.id_cpu}
			assign {sample_msg_reply_size}, {msg->msg_length}
			static_call sys_mail, send, {msg}

			;be friendly
			static_call sys_task, yield
		loop_end
		static_call sys_mem, free, {msg}
		pop_scope
		return

	fn_function_end
