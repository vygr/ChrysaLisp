%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_stream.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_macroexpand
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

		slot_call vector, get_length, {args}, {length}
		if (length == 2)
			static_call lisp, func_copy, {this, args}, {value}
			loop_start
				static_call lisp, repl_expand, {this, &value}, {length}
			loop_until {length}
		else
			static_call lisp, error, {this, "(macroexpand form) wrong number of args", args}
			assign {0}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
