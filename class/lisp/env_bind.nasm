%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/env_bind
	;inputs
	;r0 = lisp object
	;r1 = vars list
	;r2 = vals list
	;r3 = vals start index
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, vars, vals, symbol, value
	ushort len_vars, len_vals, index_vars, index_vals

	push_scope
	retire {r0, r1, r2, r3}, {this, vars, vals, index_vals}

	if {vars->obj_vtable == @class/class_vector}
		if {vals->obj_vtable == @class/class_vector}
			assign {0}, {index_vars}
			assign {this->lisp_sym_nil}, {value}
			func_call ref, ref, {value}
			devirt_call vector, get_length, {vars}, {len_vars}
			devirt_call vector, get_length, {vals}, {len_vals}
			loop_while {index_vars != len_vars}
				func_call ref, deref, {value}
				func_call vector, get_element, {vars, index_vars}, {symbol}
				if {symbol == this->lisp_sym_rest}
					assign {index_vars + 1}, {index_vars}
					if {index_vars == len_vars}
						func_call error, create, {"(bind vars vals): missing &rest var", vars}, {value}
						goto exit
					endif
					func_call vector, get_element, {vars, index_vars}, {symbol}
					devirt_call vector, slice, {vals, index_vals, len_vals}, {value}
					func_call lisp, env_def, {this, symbol, value}
					goto exit
				endif
				if {index_vals == len_vals}
					func_call error, create, {"(bind vars vals): not enough vals", vars}, {value}
					goto exit
				endif
				devirt_call vector, ref_element, {vals, index_vals}, {value}
				func_call lisp, env_def, {this, symbol, value}
				assign {index_vars + 1, index_vals + 1}, {index_vars, index_vals}
			loop_end
			breakif {index_vals == len_vals}
			func_call ref, deref, {value}
			func_call error, create, {"(bind vars vals): too many vals", vars}, {value}
		else
			func_call error, create, {"(bind vars vals): vals not a list", vals}, {value}
		endif
	else
		func_call error, create, {"(bind vars vals): vars not a list", vars}, {value}
	endif
exit:
	eval {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
