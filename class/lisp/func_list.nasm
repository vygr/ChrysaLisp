%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/func_list
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args
	ulong length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	devirt_call vector, slice, {args, 0, length}, {args}

	eval {this, args}, {r0, r1}
	pop_scope
	return

def_func_end
