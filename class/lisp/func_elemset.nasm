%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_boxed_long.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_elemset
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
	vpif {length == 3}
		func_call vector, get_element, {args, 1}, {seq}
		vpif {seq->obj_vtable == @class/class_vector}
			func_call vector, get_element, {args, 0}, {value}
			vpif {value->obj_vtable == @class/class_boxed_long}
				func_call boxed_long, get_value, {value}, {index}
				devirt_call vector, get_length, {seq}, {length}
				vpif {index < 0}
					assign {length + index + 1}, {index}
				endif
				vpif {index >= 0 && index < length}
					devirt_call vector, ref_element, {args, 2}, {value}
					func_call vector, set_element, {seq, value, index}
					func_call ref, ref, {value}
					expr {this, value}, {r0, r1}
					return
				else
					func_call error, create, {"(elem-set index list val) index out of bounds", args}, {value}
				endif
			else
				func_call error, create, {"(elem-set index list val) not an index", args}, {value}
			endif
		else
			func_call error, create, {"(elem-set index list val) not a list", args}, {value}
		endif
	else
		func_call error, create, {"(elem-set index list val) not enough args", args}, {value}
	endif

	expr {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
