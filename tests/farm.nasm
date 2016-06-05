%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'class/class_string.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/farm

		const num_child, 128

		ptr name
		ptr ids
		ptr msg
		ulong cnt

		push_scope

		;task
		static_call string, create_from_cstr, {"tests/farm_child"}, {name}

		;open farm
		static_call sys_task, open_farm, {name, num_child}, {ids}

		;send exit messages etc
		assign {0}, {cnt}
		loop_while {cnt != num_child}
			static_call sys_mail, alloc, {}, {msg}
			assign {ids[cnt * id_size].id_mbox}, {msg->msg_dest.id_mbox}
			assign {ids[cnt * id_size].id_cpu}, {msg->msg_dest.id_cpu}
			assign {msg_header_size}, {msg->msg_length}
			static_call sys_mail, send, {msg}
			static_call sys_task, yield
			assign {cnt + 1}, {cnt}
		loop_end

		;free name and ID array
		static_call string, deref, {name}
		static_call sys_mem, free, {ids}
		pop_scope
		vp_ret

	fn_function_end
