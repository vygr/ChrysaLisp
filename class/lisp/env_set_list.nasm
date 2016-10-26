%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_func class/lisp/env_set_list
		;inputs
		;r0 = lisp object
		;r1 = vars list
		;r2 = vals list
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, vars, vals, symbol, value
		ulong len1, len2

		push_scope
		retire {r0, r1, r2}, {this, vars, vals}

		if {vars->obj_vtable == @class/class_vector}
			if {vals->obj_vtable == @class/class_vector}
				devirt_call vector, get_length, {vars}, {len1}
				devirt_call vector, get_length, {vals}, {len2}
				if {len1 == len2}
					assign {this->lisp_sym_nil}, {value}
					func_call ref, ref, {value}
					assign {0}, {len1}
					loop_while {len1 != len2}
						func_call ref, deref, {value}
						func_call vector, get_element, {vars, len1}, {symbol}
						func_call vector, get_element, {vals, len1}, {value}
						func_call lisp, env_set, {this, symbol, value}, {value}
						breakif {value->obj_vtable == @class/class_error}
						assign {len1 + 1}, {len1}
					loop_end
				else
					func_call error, create, {"(set vars vals): not matching lengths", vars}, {value}
				endif
			else
				func_call error, create, {"(set vars vals): vals not a list", vals}, {value}
			endif
		else
			func_call error, create, {"(set vars vals): vars not a list", vars}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_func_end
