%include 'inc/func.inc'
%include 'inc/mail.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'

def_func sys/task_open_farm
	;inputs
	;r0 = name string object
	;r1 = number to spawn
	;outputs
	;r0 = array of mailbox id's
	;trashes
	;all but r4

	ptr name, ids, msg
	ulong length, cpu, index
	struct mailbox, mailbox

	;save task info
	push_scope
	retire {r0, r1}, {name, length}

	;create output array
	func_call sys_mem, alloc, {length * id_size}, {ids, _}

	;init temp mailbox
	func_call sys_mail, init_mailbox, {&mailbox}

	;start all tasks in parallel
	func_call sys_cpu, id, {}, {cpu}
	assign {0}, {index}
	loop_while {index != length}
		func_call sys_mail, alloc, {}, {msg}
		assign {name->string_length + 1 + kn_msg_child_size}, {msg->msg_length}
		assign {0}, {msg->msg_dest.id_mbox}
		assign {cpu}, {msg->msg_dest.id_cpu}
		assign {&mailbox}, {msg->kn_msg_reply_id.id_mbox}
		assign {cpu}, {msg->kn_msg_reply_id.id_cpu}
		assign {kn_call_task_child}, {msg->kn_msg_function}
		assign {&ids[index * id_size]}, {msg->kn_msg_user}
		func_call sys_mem, copy, {&name->string_data, &msg->kn_msg_child_pathname, \
									name->string_length + 1}, {_, _}
		func_call sys_mail, send, {msg}
		assign {index + 1}, {index}
	loop_end

	;wait for replys
	loop_while {index != 0}
		assign {index - 1}, {index}
		func_call sys_mail, read, {&mailbox}, {msg}
		assign {msg->kn_msg_reply_id.id_mbox}, {msg->kn_msg_user->id_mbox}
		assign {msg->kn_msg_reply_id.id_cpu}, {msg->kn_msg_user->id_cpu}
		func_call sys_mem, free, {msg}
	loop_end

	;return ids array
	eval {ids}, {r0}
	pop_scope
	vp_ret

def_func_end
