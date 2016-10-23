%include 'inc/func.inc'
%include 'class/class_boxed_ptr.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl_apply
		;inputs
		;r0 = lisp object
		;r1 = function
		;r2 = ast
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, func, ast, value

		push_scope
		retire {r0, r1, r2}, {this, func, ast}

		if {func->obj_vtable == @class/class_boxed_ptr}
			;built in or compiled function
			eval {this, ast, func}, {r0, r1, r2}
			vp_call [r2 + boxed_ptr_value]
			retire {r1}, {value}
		elseif {func->obj_vtable == @class/class_vector}
			;lambda
			ptr vars
			ulong length
			push_scope
			slot_call vector, get_length, {func}, {length}
			if {length == 3}
				static_call vector, get_element, {func, 0}, {vars}
				if {vars == this->lisp_sym_lambda}
					static_call lisp, env_push, {this}
					static_call vector, get_element, {func, 1}, {vars}
					static_call lisp, env_bind, {this, vars, ast, 0}, {value}
					if {value->obj_vtable != @class/class_error}
						static_call ref, deref, {value}
						static_call vector, get_element, {func, 2}, {value}
						static_call lisp, repl_eval, {this, value}, {value}
					endif
					static_call lisp, env_pop, {this}
				else
					static_call error, create, {"(lambda vars body) not lambda", vars}, {value}
				endif
			else
				static_call error, create, {"(lambda vars body) wrong numbers of args", func}, {value}
			endif
			pop_scope
		else
			static_call error, create, {"(lambda vars body) not a lambda list", func}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
