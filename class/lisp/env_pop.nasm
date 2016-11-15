%include 'inc/func.ninc'
%include 'class/class_unordered_map.ninc'
%include 'class/class_pair.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/env_pop
	;inputs
	;r0 = lisp object
	;outputs
	;r0 = lisp object

	ptr this, env
	pptr iter

	push_scope
	retire {r0}, {this}

	func_call unordered_map, find, {this->lisp_enviroment, this->lisp_sym_parent}, {iter, _}
	vpif {iter}
		func_call pair, ref_second, {*iter}, {env}
		func_call ref, deref, {this->lisp_enviroment}
		assign {env}, {this->lisp_enviroment}
	endif

	expr {this}, {r0}
	pop_scope
	return

def_func_end
