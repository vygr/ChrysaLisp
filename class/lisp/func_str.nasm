%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'
%include 'class/class_stream_str.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_str
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		const char_minus, "-"

		ptr this, args, value, stream
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		assign {0}, {value}
		static_call vector, get_length, {args}, {length}
		if {length == 2}
			static_call vector, get_element, {args, 1}, {args}
			static_call lisp, repl_eval, {this, args}, {args}
			breakifnot {args}
			static_call string, create_from_cstr, {"                "}, {value}
			static_call stream_str, create, {value}, {stream}
			static_call lisp, repl_print, {this, stream, args}
			static_call stream_str, ref_string, {stream}, {value}
			static_call ref, deref, {stream}
			static_call ref, deref, {args}
		else
			static_call lisp, error, {this, "(str arg) wrong numbers of args", args}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
