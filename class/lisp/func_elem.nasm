%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_elem
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, args, seq, value, index
		ulong length, elem_index

		push_scope
		retire {r0, r1}, {this, args}

		assign {0}, {value}
		slot_call vector, get_length, {args}, {length}
		if {length == 3}
			static_call vector, get_element, {args, 1}, {index}
			if {index->obj_vtable == @class/class_boxed_long}
				static_call vector, get_element, {args, 2}, {seq}
				static_call lisp, seq_is_seq, {this, seq}, {elem_index}
				if {elem_index}
					static_call boxed_long, get_value, {index}, {elem_index}
					method_call sequence, get_length, {seq}, {length}
					if {elem_index >= 0 && elem_index < length}
						method_call sequence, ref_element, {seq, elem_index}, {value}
					else
						static_call lisp, error, {this, "(elem index seq) index out of bounds", args}
					endif
				else
					static_call lisp, error, {this, "(elem index seq) not a sequence", args}
				endif
			else
				static_call lisp, error, {this, "(elem index seq) not an index", args}
			endif
		else
			static_call lisp, error, {this, "(elem index seq) not enough args", args}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
