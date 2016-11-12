%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_env
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = enviroment

	ptr this, args

	push_scope
	retire {r0, r1}, {this, args}

	assign {this->lisp_enviroment}, {args}
	func_call ref, ref, {args}

	eval {this, args}, {r0, r1}
	pop_scope
	return

def_func_end
