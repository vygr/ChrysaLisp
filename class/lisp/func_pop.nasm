%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_boxed_long.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_pop
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args, value
	int length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	vpif {length == 1}
		devirt_call vector, ref_element, {args, 0}, {args}
		vpif {args->obj_vtable == @class/class_vector}
			devirt_call vector, get_length, {args}, {length}
			vpif {length}
				func_call vector, get_back, {args}, {value}
				func_call vector, pop_back, {args}
			else
				assign {this->lisp_sym_nil}, {value}
				func_call ref, ref, {value}
			endif
		else
			func_call error, create, {"(pop list) not a list", args}, {value}
		endif
	else
		func_call error, create, {"(pop list) wrong number of args", args}, {value}
	endif

	expr {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
