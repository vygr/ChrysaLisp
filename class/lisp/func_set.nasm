%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_set
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args, var, val
	uint length, index

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	if {length >= 2 && !(length & 1)}
		assign {0, 0}, {index, val}
		loop_start
			func_call ref, deref_if, {val}
			func_call vector, get_element, {args, index}, {var}
			func_call vector, get_element, {args, index + 1}, {val}
			func_call lisp, env_set, {this, var, val}, {val}
			breakif {val->obj_vtable == @class/class_error}
			assign {index + 2}, {index}
		loop_until {index == length}
	else
		func_call error, create, {"(set var val ...) wrong numbers of args", args}, {val}
	endif

	expr {this, val}, {r0, r1}
	pop_scope
	return

def_func_end
