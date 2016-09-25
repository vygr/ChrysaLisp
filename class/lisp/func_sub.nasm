%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_sub
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, args, value, first
		pptr iter
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		assign {0}, {value}
		static_call vector, get_length, {args}, {length}
		if {length > 2}
			static_call vector, slice, {args, 1, length}, {args}
			static_call lisp, repl_eval_list, {this, args}, {length}
			if {length}
				static_call vector, get_element, {args, 0}, {first}
				if {first->obj_vtable == @class/class_boxed_long}
					static_call boxed_long, get_value, {first}, {length}
					static_call vector, for_each, {args, 1, $op_callback, &length}, {iter}
					if {!iter}
						static_call boxed_long, create, {}, {value}
						static_call boxed_long, set_value, {value, length}
					else
						static_call lisp, error, {this, "(sub val val ...) vals are not all numbers", args}
					endif
				else
					static_call lisp, error, {this, "(sub val val ...) vals are not all numbers", args}
				endif
			endif
			static_call vector, deref, {args}
		else
			static_call lisp, error, {this, "(sub val val ...) not enough args", args}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	op_callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		plong pdata
		long val

		push_scope
		retire {r0, r1}, {iter, pdata}

		if {(*iter)->obj_vtable != @class/class_boxed_long}
			eval {0}, {r1}
		else
			static_call boxed_long, get_value, {*iter}, {val}
			assign {*pdata - val}, {*pdata}
			eval {1}, {r1}
		endif

		pop_scope
		return

	def_function_end
