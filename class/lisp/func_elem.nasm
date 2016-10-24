%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_elem
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, args, seq, value
		ulong length, index

		push_scope
		retire {r0, r1}, {this, args}

		slot_call vector, get_length, {args}, {length}
		if {length == 2}
			static_call vector, get_element, {args, 1}, {value}
			if {value->obj_vtable == @class/class_boxed_long}
				static_call vector, get_element, {args, 0}, {seq}
				slot_function class, sequence
				static_call obj, inst_of, {seq, @_function_}, {index}
				if {index}
					static_call boxed_long, get_value, {value}, {index}
					method_call sequence, get_length, {seq}, {length}
					if {index < 0}
						assign {length + index}, {index}
					endif
					if {index >= 0 && index < length}
						method_call sequence, ref_element, {seq, index}, {value}
						eval {this, value}, {r0, r1}
						return
					else
						static_call error, create, {"(elem seq index) index out of bounds", args}, {value}
					endif
				else
					static_call error, create, {"(elem seq index) not a sequence", args}, {value}
				endif
			else
				static_call error, create, {"(elem seq index) not an index", args}, {value}
			endif
		else
			static_call error, create, {"(elem seq index) not enough args", args}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
