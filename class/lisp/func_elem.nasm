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

		ptr this, args, seq, value, index
		ulong length, elem_index

		push_scope
		retire {r0, r1}, {this, args}

		slot_call vector, get_length, {args}, {length}
		if {length == 2}
			static_call vector, get_element, {args, 0}, {index}
			if {index->obj_vtable == @class/class_boxed_long}
				static_call vector, get_element, {args, 1}, {seq}
				slot_function class, sequence
				static_call obj, inst_of, {seq, @_function_}, {elem_index}
				if {elem_index}
					static_call boxed_long, get_value, {index}, {elem_index}
					method_call sequence, get_length, {seq}, {length}
					if {elem_index >= 0 && elem_index < length}
						method_call sequence, ref_element, {seq, elem_index}, {value}
					else
						static_call error, create, {"(elem index seq) index out of bounds", args}, {value}
					endif
				else
					static_call error, create, {"(elem index seq) not a sequence", args}, {value}
				endif
			else
				static_call error, create, {"(elem index seq) not an index", args}, {value}
			endif
		else
			static_call error, create, {"(elem index seq) not enough args", args}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
