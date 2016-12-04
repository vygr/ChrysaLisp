%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_string.ninc'
%include 'class/class_boxed_long.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_split
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args, str, value
	ulong length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	vpif {length == 2}
		func_call vector, get_element, {args, 0}, {str}
		vpif {str->obj_vtable == @class/class_string}
			func_call vector, get_element, {args, 1}, {value}
			vpif {value->obj_vtable == @class/class_boxed_long}
				func_call boxed_long, get_value, {value}, {value}
				func_call string, split, {str, value}, {value}
			else
				func_call error, create, {"(split str code) not a code", args}, {value}
			endif
		else
			func_call error, create, {"(split str code) not a string", args}, {value}
		endif
	else
		func_call error, create, {"(split str code) wrong number of args", args}, {value}
	endif

	expr {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
