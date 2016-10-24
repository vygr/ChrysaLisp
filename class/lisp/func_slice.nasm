%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_sequence.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_slice
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, args, seq, value
		ulong length, start, end

		push_scope
		retire {r0, r1}, {this, args}

		slot_call vector, get_length, {args}, {length}
		if {length == 3}
			static_call vector, get_element, {args, 0}, {seq}
			slot_function class, sequence
			static_call obj, inst_of, {seq, @_function_}, {length}
			if {length}
				static_call vector, get_element, {args, 1}, {value}
				if {value->obj_vtable == @class/class_boxed_long}
					static_call boxed_long, get_value, {value}, {start}
					static_call vector, get_element, {args, 2}, {value}
					gotoif {value->obj_vtable != @class/class_boxed_long}, index_error
					static_call boxed_long, get_value, {value}, {end}
					method_call sequence, get_length, {seq}, {length}
					if {start < 0}
						assign {length + start}, {start}
					endif
					assign {start + end}, {end}
					if {start > end}
						assign {start}, {length}
						assign {end}, {start}
						assign {length}, {end}
					endif
					if {start >= 0 && end <= length}
						method_call sequence, slice, {seq, start, end}, {value}
						eval {this, value}, {r0, r1}
						return
					else
						static_call error, create, {"(slice seq start len) index out of bounds", args}, {value}
					endif
				else
				index_error:
					static_call error, create, {"(slice seq start len) not an index", args}, {value}
				endif
			else
				static_call error, create, {"(slice seq start len) not a sequence", args}, {value}
			endif
		else
			static_call error, create, {"(slice seq start len) wrong number of args", args}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
