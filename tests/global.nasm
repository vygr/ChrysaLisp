%include 'inc/func.inc'
%include 'inc/task.inc'
%include 'class/class_string.inc'

;;;;;;;;;;;
; test code
;;;;;;;;;;;

	fn_function tests/global

		const test_size, 10000

		ptr name, ids, msg
		ulong cnt, total

		push_scope

		;task
		static_call string, create_from_cstr, {"tests/global_child"}, {name}

		;open global farm
		static_call sys_cpu, total, {}, {total}
		static_call sys_task, open_global, {name, total}, {ids}

		;send exit parcels etc
		assign {0}, {cnt}
		loop_while {cnt != total}
			if {ids[cnt * id_size].id_mbox}
				static_call sys_mail, alloc_parcel, {test_size}, {msg}
				assign {ids[cnt * id_size].id_mbox}, {msg->msg_dest.id_mbox}
				assign {ids[cnt * id_size].id_cpu}, {msg->msg_dest.id_cpu}
				static_call sys_mail, send, {msg}
				static_call sys_task, yield
			endif
			assign {cnt + 1}, {cnt}
		loop_end

		;free name and ID array
		static_call string, deref, {name}
		static_call sys_mem, free, {ids}
		pop_scope
		return

	fn_function_end
