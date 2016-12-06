%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_boxed_long.ninc'
%include 'class/class_unordered_map.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_rehash
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = enviroment

	ptr this, args, value
	ulong length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	vpif {length == 1}
		func_call vector, get_element, {args, 0}, {value}
		vpif {value->obj_vtable == @class/class_boxed_long}
			func_call boxed_long, get_value, {value}, {value}
			func_call unordered_map, copy, {this->lisp_enviroment, value}, {value}
			func_call ref, deref, {this->lisp_enviroment}
			func_call ref, ref, {value}
			assign {value}, {this->lisp_enviroment}
		else
			func_call error, create, {"(rehash num) not a number", args}, {value}
		endif
	else
		func_call error, create, {"(rehash num) wrong number of args", args}, {value}
	endif

	expr {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
