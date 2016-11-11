%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_boxed_long.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_bshl
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = else value

	ptr this, args, value, num, shift
	ulong length, cnt

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	if {length == 2}
		func_call vector, get_element, {args, 0}, {num}
		if {num->obj_vtable == @class/class_boxed_long}
			func_call vector, get_element, {args, 1}, {shift}
			if {shift->obj_vtable == @class/class_boxed_long}
				func_call boxed_long, get_value, {num}, {length}
				func_call boxed_long, get_value, {shift}, {cnt}
				assign {length << cnt}, {length}
				func_call boxed_long, create, {}, {value}
				func_call boxed_long, set_value, {value, length}
			else
				func_call error, create, {"(bit-shl num cnt) not a count", args}, {value}
			endif
		else
			func_call error, create, {"(bit-shl num cnt) not a number", args}, {value}
		endif
	else
		func_call error, create, {"(bit-shl num cnt) wrong number of args", args}, {value}
	endif

	eval {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
