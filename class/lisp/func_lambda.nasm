%include 'inc/func.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_lambda
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args

	push_scope
	retire {r0, r1}, {this, args}

	func_call ref, ref, {args}

	expr {this, args}, {r0, r1}
	pop_scope
	return

def_func_end
