%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_boxed_long.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_length
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = else value

	ptr this, args, value
	ulong length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	if {length == 1}
		func_call vector, get_element, {args, 0}, {args}
		func_path class, sequence
		func_call obj, inst_of, {args, @_function_}, {value}
		if {value}
			virt_call sequence, get_length, {args}, {length}
			func_call boxed_long, create, {}, {value}
			func_call boxed_long, set_value, {value, length}
		else
			func_call error, create, {"(length seq) not a sequence", args}, {value}
		endif
	else
		func_call error, create, {"(length seq) wrong number of args", args}, {value}
	endif

	eval {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
