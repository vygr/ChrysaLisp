%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_symbol.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_str
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		const char_minus, "-"

		ptr this, args, value
		pubyte buffer
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		assign {0}, {value}
		static_call vector, get_length, {args}, {length}
		if {length == 2}
			static_call vector, get_element, {args, 1}, {args}
			static_call lisp, repl_eval, {this, args}, {args}
			breakifnot {args}
			switch
			case {args->obj_vtable == @class/class_symbol \
				|| args->obj_vtable == @class/class_string}
				assign {args}, {value}
				static_call ref, ref, {value}
				break
			case {args->obj_vtable == @class/class_boxed_long}
				static_call boxed_long, get_value, {args}, {length}
				static_call symbol, create_from_long, {length, 10}, {value}
				break
			default
				static_call lisp, error, {this, "(str arg) arg is not stringable", args}
			endswitch
			static_call ref, deref, {args}
		else
			static_call lisp, error, {this, "(str arg) wrong numbers of args", args}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
