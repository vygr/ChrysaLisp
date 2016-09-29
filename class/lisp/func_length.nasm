%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_length
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
		if {length == 2}
			static_call vector, get_element, {args, 1}, {args}
			static_call lisp, repl_eval, {this, args}, {args}
			breakifnot {args}
			if {args->obj_vtable == @class/class_vector}
				static_call vector, get_length, {args}, {length}
				static_call boxed_long, create, {}, {value}
				static_call boxed_long, set_value, {value, length}
			else
				static_call lisp, error, {this, "(length list) not a list", args}
			endif
			static_call vector, deref, {args}
		else
			static_call lisp, error, {this, "(length list) wrong number of args", args}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
