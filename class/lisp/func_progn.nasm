%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_progn
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, args
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		slot_call vector, get_length, {args}, {length}
		if {length}
			slot_call vector, ref_element, {args, length - 1}, {args}
		else
			assign {this->lisp_sym_nil}, {args}
			static_call ref, ref, {args}
		endif

		eval {this, args}, {r0, r1}
		pop_scope
		return

	def_function_end
