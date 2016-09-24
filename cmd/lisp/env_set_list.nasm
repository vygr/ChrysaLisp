%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/env_set_list
		;inputs
		;r0 = lisp globals
		;r1 = vars list
		;r2 = vals list
		;outputs
		;r0 = 0, vals list

		ptr lisp, vars, vals, symbol, value
		ulong len1, len2

		push_scope
		retire {r0, r1, r2}, {lisp, vars, vals}

		assign {0}, {value}
		if {vars->obj_vtable != @class/class_vector}
			static_call lisp, error, {lisp, "(set vars vals): vars not a list"}
		else
			if {vals->obj_vtable != @class/class_vector}
				static_call lisp, error, {lisp, "(set vars vals): vals not a list"}
			else
				static_call vector, get_length, {vars}, {len1}
				static_call vector, get_length, {vals}, {len2}
				if {len1 != len2}
					static_call lisp, error, {lisp, "(set vars vals): non matching lengths"}
				else
					assign {0}, {len1}
					loop_while {len1 != len2}
						static_call vector, get_element, {vars, len1}, {symbol}
						static_call vector, get_element, {vals, len1}, {value}
						static_call lisp, env_set, {lisp, symbol, value}
						assign {len1 + 1}, {len1}
					loop_end
					assign {vals}, {value}
				endif
			endif
		endif

		eval {value}, {r0}
		pop_scope
		return

	def_function_end
