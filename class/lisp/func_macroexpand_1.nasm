%include 'inc/func.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_pair.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/func_macroexpand_1
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value
	;r2 = 0 if expanded

	ptr this, args, value
	ulong length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	if (length == 1)
		func_call lisp, func_copy, {this, args}, {value}
		func_call lisp, repl_expand, {this, &value}, {length}
	else
		func_call error, create, {"(macroexpand-1 form) wrong number of args", args}, {value}
		assign {1}, {length}
	endif

	eval {this, value, length}, {r0, r1, r2}
	pop_scope
	return

def_func_end
