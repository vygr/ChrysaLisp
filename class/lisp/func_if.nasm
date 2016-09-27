%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_if
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, args, value
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		assign {0}, {value}
		static_call vector, get_length, {args}, {length}
		if {length == 3 || length == 4}
			static_call vector, get_element, {args, 1}, {value}
			static_call lisp, repl_eval, {this, value}, {value}
			breakifnot {value}
			if {value != this->lisp_sym_nil}
				static_call ref, deref, {value}
				static_call vector, get_element, {args, 2}, {value}
				static_call lisp, repl_eval, {this, value}, {value}
			elseif {length == 4}
				static_call ref, deref, {value}
				static_call vector, get_element, {args, 3}, {value}
				static_call lisp, repl_eval, {this, value}, {value}
			endif
		else
			static_call lisp, error, {this, "(if tst form form) wrong number of args", args}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
