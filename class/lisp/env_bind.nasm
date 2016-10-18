%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/env_bind
		;inputs
		;r0 = lisp object
		;r1 = vars list
		;r2 = vals list
		;r3 = vals start index_vals
		;outputs
		;r0 = lisp object
		;r1 = 0, vars list

		ptr this, vars, vals, symbol, value
		ushort len_vars, len_vals, index_vars, index_vals

		push_scope
		retire {r0, r1, r2, r3}, {this, vars, vals, index_vals}

		assign {0}, {value}
		if {vars->obj_vtable == @class/class_vector}
			if {vals->obj_vtable == @class/class_vector}
				assign {0}, {index_vars}
				slot_call vector, get_length, {vars}, {len_vars}
				slot_call vector, get_length, {vals}, {len_vals}
				loop_while {index_vars != len_vars}
					static_call vector, get_element, {vars, index_vars}, {symbol}
					if {symbol == this->lisp_sym_rest}
						assign {index_vars + 1}, {index_vars}
						if {index_vars == len_vars}
							static_call lisp, error, {this, "(bind vars vals): missing &rest var", vars}
							goto exit
						endif
						static_call vector, get_element, {vars, index_vars}, {symbol}
						static_call vector, slice, {vals, index_vals, len_vals}, {value}
						static_call lisp, env_def, {this, symbol, value}
						static_call ref, deref, {value}
						goto exit
					endif
					slot_call vector, get_element, {vals, index_vals}, {value}
					static_call lisp, env_def, {this, symbol, value}
					assign {index_vars + 1}, {index_vars}
					assign {index_vals + 1}, {index_vals}
				loop_end
				assign {vals}, {value}
				breakif {index_vals == len_vals}
				static_call lisp, error, {this, "(bind vars vals): too many vals", vars}
				assign {0}, {value}
			else
				static_call lisp, error, {this, "(bind vars vals): vals not a list", vals}
			endif
		else
			static_call lisp, error, {this, "(bind vars vals): vars not a list", vars}
		endif
	exit:
		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
