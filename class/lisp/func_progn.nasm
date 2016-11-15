%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_progn
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
	if {length}
		devirt_call vector, ref_element, {args, length - 1}, {args}
	else
		assign {this->lisp_sym_nil}, {args}
		func_call ref, ref, {args}
	endif

	expr {this, args}, {r0, r1}
	pop_scope
	return

def_func_end
