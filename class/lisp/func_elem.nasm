%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_func class/lisp/func_elem
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, args, seq, value
		int length, index

		push_scope
		retire {r0, r1}, {this, args}

		devirt_call vector, get_length, {args}, {length}
		if {length == 2}
			func_call vector, get_element, {args, 0}, {seq}
			func_path class, sequence
			func_call obj, inst_of, {seq, @_function_}, {value}
			if {value}
				func_call vector, get_element, {args, 1}, {value}
				if {value->obj_vtable == @class/class_boxed_long}
					func_call boxed_long, get_value, {value}, {index}
					virt_call sequence, get_length, {seq}, {length}
					if {index < 0}
						assign {length + index + 1}, {index}
					endif
					if {index >= 0 && index < length}
						virt_call sequence, ref_element, {seq, index}, {value}
						eval {this, value}, {r0, r1}
						return
					else
						func_call error, create, {"(elem seq index) index out of bounds", args}, {value}
					endif
				else
					func_call error, create, {"(elem seq index) not an index", args}, {value}
				endif
			else
				func_call error, create, {"(elem seq index) not a sequence", args}, {value}
			endif
		else
			func_call error, create, {"(elem seq index) not enough args", args}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_func_end
