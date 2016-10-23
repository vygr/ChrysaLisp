%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_length
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = else value

		ptr this, args, value
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		slot_call vector, get_length, {args}, {length}
		if {length == 1}
			static_call vector, get_element, {args, 0}, {args}
			slot_function class, sequence
			static_call obj, inst_of, {args, @_function_}, {value}
			if {value}
				method_call sequence, get_length, {args}, {length}
				static_call boxed_long, create, {}, {value}
				static_call boxed_long, set_value, {value, length}
			else
				static_call error, create, {"(length seq) not a sequence", args}, {value}
			endif
		else
			static_call error, create, {"(length seq) wrong number of args", args}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
