%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_unordered_map.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_rehash
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = enviroment

	ptr this, args

	push_scope
	retire {r0, r1}, {this, args}

	func_call unordered_map, copy, {this->lisp_enviroment, 71}, {args}
	func_call ref, deref, {this->lisp_enviroment}
	func_call ref, ref, {args}
	assign {args}, {this->lisp_enviroment}

	expr {this, args}, {r0, r1}
	pop_scope
	return

def_func_end
