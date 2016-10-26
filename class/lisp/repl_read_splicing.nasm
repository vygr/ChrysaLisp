%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/repl_read_splicing
	;inputs
	;r0 = lisp object
	;r1 = stream
	;r2 = next char
	;outputs
	;r0 = lisp object
	;r1 = list
	;r2 = next char

	ptr this, stream, list, elem
	ulong char

	push_scope
	retire {r0, r1, r2}, {this, stream, char}

	;skip "~"
	func_call stream, read_char, {stream}, {char}

	func_call vector, create, {}, {list}
	assign {this->lisp_sym_splicing}, {elem}
	func_call ref, ref, {elem}
	func_call vector, push_back, {list, elem}
	func_call lisp, repl_read, {this, stream, char}, {elem, char}
	if {elem->obj_vtable != @class/class_error}
		func_call vector, push_back, {list, elem}
	else
		func_call ref, deref, {list}
		assign {elem}, {list}
	endif

	eval {this, list, char}, {r0, r1, r2}
	pop_scope
	return

def_func_end
