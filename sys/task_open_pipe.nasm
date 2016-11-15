%include 'inc/func.ninc'
%include 'inc/mail.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_string.ninc'

def_func sys/task_open_pipe
	;inputs
	;r0 = vector of strings
	;outputs
	;r0 = array of mailbox id's
	;trashes
	;all but r4

	ptr tasks, ids, msg, name
	ulong cpu, index, length
	struct mailbox, mailbox

	;save task info
	push_scope
	retire {r0}, {tasks}

	;create output array
	devirt_call vector, get_length, {tasks}, {length}
	func_call sys_mem, alloc, {length * id_size}, {ids, _}

	;init temp mailbox
	func_call sys_mail, init_mailbox, {&mailbox}

	;start all tasks, starting on kernel of this chip
	func_call sys_cpu, id, {}, {cpu}
	assign {0}, {index}
	loop_while {index != length}
		func_call sys_mail, alloc, {}, {msg}
		assign {(tasks->vector_array)[index * ptr_size]}, {name}
		assign {name->string_length + 1 + kn_msg_child_size}, {msg->msg_length}
		assign {0}, {msg->msg_dest.id_mbox}
		assign {cpu}, {msg->msg_dest.id_cpu}
		assign {&mailbox}, {msg->kn_msg_reply_id.id_mbox}
		func_call sys_cpu, id, {}, {msg->kn_msg_reply_id.id_cpu}
		assign {kn_call_task_child}, {msg->kn_msg_function}
		func_call sys_mem, copy, {&name->string_data, &msg->kn_msg_child_pathname, \
									name->string_length + 1}, {_, _}

		;send mail to kernel, wait for reply
		func_call sys_mail, send, {msg}
		func_call sys_mail, read, {&mailbox}, {msg}

		;save reply mailbox ID
		assign {msg->kn_msg_reply_id.id_cpu}, {cpu}
		assign {msg->kn_msg_reply_id.id_mbox}, {ids[index * id_size].id_mbox}
		assign {cpu}, {ids[index * id_size].id_cpu}
		func_call sys_mem, free, {msg}

		;next pipe worker
		assign {index + 1}, {index}
	loop_end

	;return ids array
	expr {ids}, {r0}
	pop_scope
	vp_ret

def_func_end
