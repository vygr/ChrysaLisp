%include 'inc/func.ninc'
%include 'inc/load.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_error.ninc'
%include 'class/class_symbol.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_inst_of
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = else value

	ptr this, args, class, value
	ulong length
	pubyte name_offset

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	vpif {length == 2}
		func_call vector, get_element, {args, 0}, {class}
		vpif {class->obj_vtable == @class/class_symbol}
			func_call sys_load, bind, {class->string_data}, {class}
			gotoifnot {class}, error
			func_call vector, get_element, {args, 1}, {args}
			func_call obj, inst_of, {args, class}, {value}
			vpif {value}
				assign {value - 1}, {name_offset}
				assign {value - *name_offset}, {value}
				func_call lisp, sym_intern_cstr, {this, value}, {value} 
			else
				assign {this->lisp_sym_nil}, {value}
				func_call ref, ref, {value}
			endif
		else
		error:
			func_call error, create, {"(inst-of class obj) not a class", args}, {value}
		endif
	else
		func_call error, create, {"(inst-of class obj) wrong number of args", args}, {value}
	endif

	expr {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
