%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/sym_intern
		;inputs
		;r0 = lisp object
		;r1 = string object
		;outputs
		;r0 = lisp object
		;r1 = interned symbol

		ptr this, symbol
		pptr iter

		push_scope
		retire {r0, r1}, {this, symbol}

		static_call unordered_set, insert, {this->lisp_symbols, symbol}, {iter, _}
		static_call ref, deref, {symbol}
		assign {*iter}, {symbol}
		static_call ref, ref, {symbol}

		eval {this, symbol}, {r0, r1}
		pop_scope
		return

	def_function_end
