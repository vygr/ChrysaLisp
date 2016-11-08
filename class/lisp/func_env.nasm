%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

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
