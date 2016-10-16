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

		assign {0}, {value}
		slot_call vector, get_length, {args}, {length}
		if (length == 2)
			loop_start
				static_call lisp, func_macroexpand_1, {this, args}, {value, length}
				static_call vector, set_element, {args, value, 1}

				static_call lisp, repl_print, {this, this->lisp_stderr, value}
				static_call stream, write_char, {this->lisp_stderr, 10}

			loop_until {length}
			slot_call vector, ref_element, {args, 1}, {value}
		else
			static_call lisp, error, {this, "(macroexpand form) wrong number of args", args}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
