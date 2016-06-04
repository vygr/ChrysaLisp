%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/pipe

		const num_child, 128

		ptr tasks
		ptr name
		ptr ids
		ptr msg
		ulong cnt

		push_scope

		;vector of tasks
		static_call vector, create, {}, {tasks}
		assign {0}, {cnt}
		loop_while {cnt != num_child}
			static_call string, create_from_cstr, {"tests/pipe_child"}, {name}
			static_call vector, push_back, {tasks, name}
			assign {cnt + 1}, {cnt}
		loop_end

		;open pipe
		static_call sys_task, open_pipe, {tasks}, {ids}

		;send exit messages etc
		assign {0}, {cnt}
		loop_while {cnt != num_child}
			static_call sys_mail, alloc, {}, {msg}
			assign {ids[cnt * mailbox_id_size].mb_mbox}, {msg->ml_msg_dest.mb_mbox}
			assign {ids[cnt * mailbox_id_size].mb_cpu}, {msg->ml_msg_dest.mb_cpu}
			static_call sys_mail, send, {msg}
			static_call sys_task, yield
			assign {cnt + 1}, {cnt}
		loop_end

		;free vector and ID array
		static_call vector, deref, {tasks}
		static_call sys_mem, free, {ids}
		pop_scope
		vp_ret

	fn_function_end
