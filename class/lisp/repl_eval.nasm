%include 'inc/func.inc'
%include 'class/class_boxed_ptr.inc'
%include 'class/class_string.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl_eval
		;inputs
		;r0 = lisp object
		;r1 = ast
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, ast, value
		ulong length

		push_scope
		retire {r0, r1}, {this, ast}

		;evaluate based on type
		if {ast->obj_vtable == @class/class_string}
			;symbol evals to its value
			static_call lisp, env_get, {this, ast}, {value}
			breakif {value}
			static_call lisp, error, {this, "variable not defined", ast}
		elseif {ast->obj_vtable == @class/class_boxed_ptr || ast->obj_vtable == @class/class_boxed_long}
			;function pointer or long evals to itself
			assign {ast}, {value}
			static_call ref, ref, {value}
		elseif {ast->obj_vtable == @class/class_vector}
			;list
			static_call vector, get_length, {ast}, {length}
			ifnot {length}
				;null list evals to nil
				assign {this->lisp_sym_nil}, {value}
				static_call ref, ref, {value}
			elseif {length == 1}
				;one entry evals to that entry
				static_call vector, get_element, {ast, 0}, {value}
				static_call lisp, repl_eval, {this, value}, {value}
			else
				;more than one entry applys a function
				static_call lisp, repl_apply, {this, ast}, {value}
			endif
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
