%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/func_def
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args, vars, value
	ulong length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	if {length == 2}
		func_call vector, get_element, {args, 0}, {vars}
		func_call vector, get_element, {args, 1}, {args}
		if {args->obj_vtable == @class/class_vector}
			devirt_call vector, get_length, {args}, {length}
			devirt_call vector, slice, {args, 0, length}, {args}
			func_call lisp, repl_eval_list, {this, args, 0}, {value}
			if {value->obj_vtable != @class/class_error}
				func_call ref, deref, {value}
				func_call lisp, env_bind, {this, vars, args, 0}, {value}
			endif
			func_call ref, deref, {args}
		else
			func_call error, create, {"(def vars vals) vals is not a list", args}, {value}
		endif
	else
		func_call error, create, {"(def vars vals) wrong numbers of args", args}, {value}
	endif

	eval {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
