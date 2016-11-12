%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_unordered_map.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

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
	ushort len_vars, len_vals, index_vars, index_vals, state

	push_scope
	retire {r0, r1, r2, r3}, {this, vars, vals, index_vals}

	if {vars->obj_vtable == @class/class_vector}
		if {vals->obj_vtable == @class/class_vector}
			assign {0, 0}, {index_vars, state}
			assign {this->lisp_sym_nil}, {value}
			func_call ref, ref, {value}
			devirt_call vector, get_length, {vars}, {len_vars}
			devirt_call vector, get_length, {vals}, {len_vals}
			loop_while {index_vars != len_vars}
				func_call vector, get_element, {vars, index_vars}, {symbol}
				if {symbol == this->lisp_sym_rest}
					assign {1, index_vars + 1}, {state, index_vars}
				elseif {symbol == this->lisp_sym_optional}
					assign {2, index_vars + 1}, {state, index_vars}
				endif
				breakif {index_vars == len_vars}
				func_call vector, get_element, {vars, index_vars}, {symbol}
				func_call ref, deref, {value}
				switch
				case {state == 1}
					;rest
					devirt_call vector, slice, {vals, index_vals, len_vals}, {value}
					assign {len_vars, len_vals}, {index_vars, index_vals}
					break
				case {state == 2}
					;optional
					gotoif {index_vals != len_vals}, normal
					assign {this->lisp_sym_nil}, {value}
					func_call ref, ref, {value}
					assign {index_vars + 1}, {index_vars}
					break
				default
					;normal
					gotoif {index_vals == len_vals}, error
				normal:
					devirt_call vector, ref_element, {vals, index_vals}, {value}
					assign {index_vars + 1, index_vals + 1}, {index_vars, index_vals}
				endswitch
				func_call unordered_map, insert, {this->lisp_enviroment, symbol, value}, {_, _}
			loop_end
			breakif {index_vals == len_vals}
			func_call ref, deref, {value}
		error:
			func_call error, create, {"(bind vars vals): wrong number of vals", vals}, {value}
		else
			func_call error, create, {"(bind vars vals): vals not a list", vals}, {value}
		endif
	else
		func_call error, create, {"(bind vars vals): vars not a list", vars}, {value}
	endif

	eval {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
