%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	def_func tests/pipe

		const num_child, 128

		ptr tasks, name, ids, msg
		ulong cnt

		push_scope

		;vector of tasks
		func_call vector, create, {}, {tasks}
		assign {0}, {cnt}
		loop_while {cnt != num_child}
			func_call string, create_from_cstr, {"tests/pipe_child"}, {name}
			func_call vector, push_back, {tasks, name}
			assign {cnt + 1}, {cnt}
		loop_end

		;open pipe
		func_call sys_task, open_pipe, {tasks}, {ids}

		;send exit messages etc
		loop_while {cnt != 0}
			assign {cnt - 1}, {cnt}
			continueifnot {ids[cnt * id_size].id_mbox}
			func_call sys_mail, alloc, {}, {msg}
			assign {ids[cnt * id_size].id_mbox}, {msg->msg_dest.id_mbox}
			assign {ids[cnt * id_size].id_cpu}, {msg->msg_dest.id_cpu}
			func_call sys_mail, send, {msg}
			func_call sys_task, yield
		loop_end

		;free vector and ID array
		func_call vector, deref, {tasks}
		func_call sys_mem, free, {ids}
		pop_scope
		return

	def_func_end
