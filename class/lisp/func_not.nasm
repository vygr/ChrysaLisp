%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_not
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, args
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		static_call vector, get_length, {args}, {length}
		if {length == 2}
			static_call vector, get_element, {args, 1}, {args}
			if {args == this->lisp_sym_nil}
				assign {this->lisp_sym_t}, {args}
			else
				assign {this->lisp_sym_nil}, {args}
			endif
			static_call ref, ref, {args}
		else
			static_call lisp, error, {this, "(not form) wrong number of args", args}
			assign {0}, {args}
		endif

		eval {this, args}, {r0, r1}
		pop_scope
		return

	def_function_end
