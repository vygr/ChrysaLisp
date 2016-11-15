%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_string.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_cat
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	def_struct pdata
		ptr pdata_this
		ptr pdata_value
	def_struct_end

	ptr this, value, args
	ulong length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	vpif {length}
		func_call vector, get_element, {args, 0}, {value}
		vpif {value->obj_vtable == @class/class_vector}
			func_call vector, create, {}, {value}
			func_call vector, for_each, {args, 0, length, $callback, &this}, {_}
		elseif {value->obj_vtable == @class/class_string}
			func_call ref, ref, {value}
			func_call vector, for_each, {args, 1, length, $callback, &this}, {_}
		else
			func_call error, create, {"(cat seq ...) not sequence type", value}, {value}
		endif
	else
		func_call error, create, {"(cat seq ...) wrong number of args", args}, {value}
	endif

	expr {this, value}, {r0, r1}
	pop_scope
	return

callback:
	;inputs
	;r0 = predicate data pointer
	;r1 = element iterator
	;outputs
	;r1 = 0 if break, else not

	pptr iter
	ptr pdata, elem
	ulong length

	push_scope
	retire {r0, r1}, {pdata, iter}

	assign {*iter}, {elem}
	vpif {elem->obj_vtable == pdata->pdata_value->obj_vtable}
		switch
		case {elem->obj_vtable == @class/class_string}
			func_call string, append, {pdata->pdata_value, elem}, {elem}
			func_call ref, deref, {pdata->pdata_value}
			assign {elem}, {pdata->pdata_value}
			break
		default
			devirt_call vector, get_length, {elem}, {length}
			func_call vector, append, {pdata->pdata_value, elem, 0, length}
		endswitch
		expr {1}, {r1}
	else
		func_call ref, deref, {pdata->pdata_value}
		func_call error, create, {"(cat seq ...) not matching type", elem}, {pdata->pdata_value}
		expr {0}, {r1}
	endif

	pop_scope
	return

def_func_end
