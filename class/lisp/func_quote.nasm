%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_quote
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

		slot_call vector, get_length, {args}, {length}
		if {length == 2}
			slot_call vector, ref_element, {args, 1}, {args}
		else
			static_call lisp, error, {this, "(quote arg) wrong numbers of args", args}
			assign {0}, {args}
		endif

		eval {this, args}, {r0, r1}
		pop_scope
		return

	def_function_end
