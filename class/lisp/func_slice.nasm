%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_sequence.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/func_slice
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args, seq, value
	int length, start, end

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	if {length == 3}
		func_call vector, get_element, {args, 0}, {seq}
		func_path class, sequence
		func_call obj, inst_of, {seq, @_function_}, {value}
		if {value}
			func_call vector, get_element, {args, 1}, {value}
			if {value->obj_vtable == @class/class_boxed_long}
				func_call boxed_long, get_value, {value}, {start}
				func_call vector, get_element, {args, 2}, {value}
				gotoif {value->obj_vtable != @class/class_boxed_long}, index_error
				func_call boxed_long, get_value, {value}, {end}
				virt_call sequence, get_length, {seq}, {length}
				if {start < 0}
					assign {length + start + 1}, {start}
				endif
				if {end < 0}
					assign {length + end + 1}, {end}
				endif
				if {start >= 0 && end <= length}
					virt_call sequence, slice, {seq, start, end}, {value}
					eval {this, value}, {r0, r1}
					return
				else
					func_call error, create, {"(slice seq start end) index out of bounds", args}, {value}
				endif
			else
			index_error:
				func_call error, create, {"(slice seq start end) not an index", args}, {value}
			endif
		else
			func_call error, create, {"(slice seq start end) not a sequence", args}, {value}
		endif
	else
		func_call error, create, {"(slice seq start end) wrong number of args", args}, {value}
	endif

	eval {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
