%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_string.ninc'
%include 'class/class_boxed_long.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_code
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args, value
	pubyte charp
	ulong length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	if {length == 1}
		func_call vector, get_element, {args, 0}, {value}
		if {value->obj_vtable == @class/class_string}
			assign {&value->string_data}, {charp}
			func_call boxed_long, create, {}, {value}
			func_call boxed_long, set_value, {value, *charp}
		else
			func_call error, create, {"(code char) not a string", args}, {value}
		endif
	else
		func_call error, create, {"(code char) wrong number of args", args}, {value}
	endif

	expr {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
