%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/repl_eval
		;inputs
		;r0 = lisp globals
		;r1 = ast
		;outputs
		;r0 = 0, else value

		ptr lisp, ast, value, func, args
		ulong length

		push_scope
		retire {r0, r1}, {lisp, ast}

		;evaluate based on type
		if {ast->obj_vtable == @class/class_string}
			;symbol evals to its value
			static_call lisp, env_get, {lisp, ast}, {value}
			breakif {value}
			static_call lisp, error, {lisp, "variable not defined"}
		elseif {ast->obj_vtable == @class/class_boxed_ptr || ast->obj_vtable == @class/class_boxed_long}
			;function pointer or long evals to itself
			assign {ast}, {value}
			static_call ref, ref, {value}
		elseif {ast->obj_vtable == @class/class_vector}
			;list
			static_call vector, get_length, {ast}, {length}
			if {!length}
				;null list evals to nil
				assign {lisp->lisp_sym_nil}, {value}
				static_call ref, ref, {value}
			elseif {length == 1}
				;one entry evals to that entry
				static_call vector, get_element, {ast, 0}, {value}
				static_call lisp, repl_eval, {lisp, value}, {value}
			else
				;more than one entry calls first as function on remaining
				static_call vector, get_element, {ast, 0}, {func}
				static_call lisp, repl_eval, {lisp, func}, {func}
				assign {0}, {value}
				if (func)
					static_call vector, slice, {ast, 1, length}, {args}
					if {func->obj_vtable == @class/class_boxed_ptr}
						;built in function
						eval {lisp, args, func}, {r0, r1, r2}
						vp_call [r2 + boxed_ptr_value]
						retire {r0}, {value}
					else
						static_call lisp, error, {lisp, "lambda not implamented yet"}
					endif
					static_call ref, deref, {args}
					static_call ref, deref, {func}
				endif
			endif
		endif

		eval {value}, {r0}
		pop_scope
		return

	def_function_end
