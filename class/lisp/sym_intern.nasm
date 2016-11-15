%include 'inc/func.ninc'
%include 'class/class_unordered_set.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/sym_intern
	;inputs
	;r0 = lisp object
	;r1 = symbol object
	;outputs
	;r0 = lisp object
	;r1 = interned symbol

	ptr this, symbol
	pptr iter

	push_scope
	retire {r0, r1}, {this, symbol}

	func_call unordered_set, insert, {this->lisp_symbols, symbol}, {iter, _}
	func_call ref, deref, {symbol}
	assign {*iter}, {symbol}
	func_call ref, ref, {symbol}

	expr {this, symbol}, {r0, r1}
	pop_scope
	return

def_func_end
