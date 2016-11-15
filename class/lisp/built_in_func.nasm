%include 'inc/func.ninc'
%include 'class/class_boxed_ptr.ninc'
%include 'class/class_unordered_map.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/built_in_func
	;inputs
	;r0 = lisp object
	;r1 = symbol
	;r2 = function pointer
	;r3 = function flags
	;outputs
	;r0 = lisp object

	ptr this, symbol, func_ptr, func_flags, func

	push_scope
	retire {r0, r1, r2, r3}, {this, symbol, func_ptr, func_flags}

	func_call boxed_ptr, create, {}, {func}
	func_call boxed_ptr, set_value, {func, func_ptr}
	func_call boxed_ptr, set_flags, {func, func_flags}
	func_call unordered_map, insert, {this->lisp_enviroment, symbol, func}, {_, _}
	func_call ref, deref, {func}

	expr {this}, {r0}
	pop_scope
	return

def_func_end
