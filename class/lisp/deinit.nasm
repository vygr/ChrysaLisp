%include 'inc/func.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/deinit
	;inputs
	;r0 = object
	;trashes
	;all

	ptr this

	push_scope
	retire {r0}, {this}

	;deinit myself
	func_call ref, deref, {this->lisp_symbols}
	func_call ref, deref, {this->lisp_enviroment}
	func_call ref, deref, {this->lisp_macros}

	;deinit parent
	super_call lisp, deinit, {this}

	pop_scope
	return

def_func_end
