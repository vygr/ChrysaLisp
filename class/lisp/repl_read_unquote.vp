%include 'inc/func.ninc'
%include 'class/class_stream.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/repl_read_unquote
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

	;skip ","
	func_call stream, read_char, {stream}, {char}

	func_call vector, create, {}, {list}
	assign {this->lisp_sym_unquote}, {elem}
	func_call ref, ref, {elem}
	func_call vector, push_back, {list, elem}
	func_call lisp, repl_read, {this, stream, char}, {elem, char}
	if {elem}
		func_call vector, push_back, {list, elem}
	else
		func_call ref, deref, {list}
		func_call error, create, {"read unquote error", this->lisp_sym_nil}, {list}
	endif

	eval {this, list, char}, {r0, r1, r2}
	pop_scope
	return

def_func_end
