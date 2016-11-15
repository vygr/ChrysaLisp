%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_not
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
	if {length == 1}
		func_call vector, get_element, {args, 0}, {args}
		if {args == this->lisp_sym_nil}
			assign {this->lisp_sym_t}, {args}
		else
			assign {this->lisp_sym_nil}, {args}
		endif
		func_call ref, ref, {args}
	else
		func_call error, create, {"(not form) wrong number of args", args}, {args}
	endif

	expr {this, args}, {r0, r1}
	pop_scope
	return

def_func_end
