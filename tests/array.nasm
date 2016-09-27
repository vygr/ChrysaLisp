%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	def_function tests/array

		const num_child, 128

		ptr tasks, name, ids, msg
		ulong cnt

		push_scope

		;vector of tasks
		static_call vector, create, {}, {tasks}
		assign {0}, {cnt}
		loop_while {cnt != num_child}
			static_call string, create_from_cstr, {"tests/array_child"}, {name}
			static_call vector, push_back, {tasks, name}
			assign {cnt + 1}, {cnt}
		loop_end

		;open array
		static_call sys_task, open_array, {tasks}, {ids}

		;send exit messages etc
		loop_while {cnt != 0}
			assign {cnt - 1}, {cnt}
			continueifnot {ids[cnt * id_size].id_mbox}
			static_call sys_mail, alloc, {}, {msg}
			assign {ids[cnt * id_size].id_mbox}, {msg->msg_dest.id_mbox}
			assign {ids[cnt * id_size].id_cpu}, {msg->msg_dest.id_cpu}
			static_call sys_mail, send, {msg}
			static_call sys_task, yield
		loop_end

		;free vector and ID array
		static_call vector, deref, {tasks}
		static_call sys_mem, free, {ids}
		pop_scope
		return

	def_function_end
