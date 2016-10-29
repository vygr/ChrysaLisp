%include 'inc/func.inc'
%include 'class/class_master.inc'
%include 'class/class_vector.inc'

def_func class/master/get_stream
	;inputs
	;r0 = master object
	;r1 = mailbox
	;outputs
	;r0 = master object
	;r1 = stream object
	;trashes
	;all but r0, r4

	ptr inst, mailbox
	ulong index

	push_scope
	retire {r0, r1}, {inst, mailbox}

	assign {0}, {index}
	loop_while {mailbox != (inst->master_select_array)[index]}
		assign {index + ptr_size}, {index}
	loop_end

	eval {inst, (inst->master_streams->vector_array)[index]}, {r0, r1}
	pop_scope
	return

def_func_end
