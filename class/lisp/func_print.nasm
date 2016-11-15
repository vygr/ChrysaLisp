%include 'inc/func.ninc'
%include 'class/class_stream.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_print
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	const char_lf, 10

	ptr this, args

	push_scope
	retire {r0, r1}, {this, args}

	func_call lisp, func_prin, {this, args}, {args}
	func_call stream, write_char, {this->lisp_stdout, char_lf}

	expr {this, args}, {r0, r1}
	pop_scope
	return

def_func_end
