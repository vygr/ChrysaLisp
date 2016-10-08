%include 'inc/func.inc'
%include 'class/class_boxed_ptr.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl_apply
		;inputs
		;r0 = lisp object
		;r1 = function
		;r2 = ast
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, func, ast, value

		push_scope
		retire {r0, r1, r2}, {this, func, ast}

		assign {0}, {value}
		if {func->obj_vtable == @class/class_boxed_ptr}
			;built in
			eval {this, ast, func}, {r0, r1, r2}
			vp_call [r2 + boxed_ptr_value]
			retire {r1}, {value}
		elseif {func->obj_vtable == @class/class_vector}
			;lambda
			ptr vars, vals, body, args
			ulong length
			push_scope
			static_call vector, get_length, {func}, {length}
			if {length == 3}
				static_call vector, get_element, {func, 0}, {args}
				if {args == this->lisp_sym_lambda}
					static_call vector, get_length, {ast}, {length}
					static_call vector, slice, {ast, 1, length}, {args}
					static_call lisp, repl_eval_list, {this, args, 0}, {vals}
					if {vals}
						static_call lisp, env_push, {this}
						static_call vector, get_element, {func, 1}, {vars}
						static_call lisp, env_def_list, {this, vars, vals}, {length}
						if {length}
							static_call vector, get_element, {func, 2}, {body}
							static_call lisp, repl_eval, {this, body}, {value}
						endif
						static_call lisp, env_pop, {this}
					endif
					static_call ref, deref, {args}
				else
					static_call lisp, error, {this, "(lambda vars body) not lambda", args}
				endif
			else
				static_call lisp, error, {this, "(lambda vars body) wrong numbers of args", func}
			endif
			pop_scope
		else
			static_call lisp, error, {this, "(lambda vars body) not a lambda list", func}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
