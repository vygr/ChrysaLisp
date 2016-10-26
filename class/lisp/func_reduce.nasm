%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/func_reduce
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args, value, func, form, seq
	ulong length, seq_num, seq_length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	if {length == 2 || length == 3}
		func_call vector, get_element, {args, 0}, {func}
		func_call vector, get_element, {args, 1}, {seq}
		func_path class, sequence
		func_call obj, inst_of, {seq, @_function_}, {form}
		if {form}
			virt_call sequence, get_length, {seq}, {seq_length}
			if {(length == 2 && seq_length > 0) || length == 3}
				if {length == 3}
					devirt_call vector, ref_element, {args, 2}, {value}
					assign {0}, {seq_num}
				else
					virt_call sequence, ref_element, {seq, 0}, {value}
					assign {1}, {seq_num}
				endif
				breakif {seq_num >= seq_length}
				devirt_call vector, slice, {args, 0, 2}, {form}
				loop_start
					func_call vector, set_element, {form, value, 0}
					virt_call sequence, ref_element, {seq, seq_num}, {value}
					func_call vector, set_element, {form, value, 1}
					func_call lisp, repl_apply, {this, func, form}, {value}
					breakif {value->obj_vtable == @class/class_error}
					assign {seq_num + 1}, {seq_num}
				loop_until {seq_num == seq_length}
				func_call ref, deref, {form}
			else
				func_call error, create, {"(reduce func list {init}) not enough elements", args}, {value}
			endif
		else
			func_call error, create, {"(reduce func list {init}) not a sequence", seq}, {value}
		endif
	else
		func_call error, create, {"(reduce func list {init}) wrong number of args", args}, {value}
	endif

	eval {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
