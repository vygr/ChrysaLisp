%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_boxed_long.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_bxor
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args, value
	pptr iter
	long length, accum

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	if {length > 1}
		func_call vector, get_element, {args, 0}, {value}
		if {value->obj_vtable == @class/class_boxed_long}
			func_call boxed_long, get_value, {value}, {accum}
			func_call vector, for_each, {args, 1, length, $callback, &accum}, {iter}
			gotoif {iter}, error
			func_call boxed_long, create, {}, {value}
			func_call boxed_long, set_value, {value, accum}
		else
		error:
			func_call error, create, {"(bit-xor val val ...) vals are not all numbers", args}, {value}
		endif
	else
		func_call error, create, {"(bit-xor val val ...) not enough args", args}, {value}
	endif

	eval {this, value}, {r0, r1}
	pop_scope
	return

callback:
	;inputs
	;r0 = predicate data pointer
	;r1 = element iterator
	;outputs
	;r1 = 0 if break, else not

	plong pdata
	pptr iter
	long val

	push_scope
	retire {r0, r1}, {pdata, iter}

	if {(*iter)->obj_vtable == @class/class_boxed_long}
		func_call boxed_long, get_value, {*iter}, {val}
		assign {*pdata ^ val}, {*pdata}
		eval {1}, {r1}
	else
		eval {0}, {r1}
	endif

	pop_scope
	return

def_func_end
