%include 'inc/func.inc'
%include 'class/class_boxed_ptr.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_symbol.inc'
%include 'class/class_vector.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_pair.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl_eval
		;inputs
		;r0 = lisp object
		;r1 = ast
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, ast, value, func

		push_scope
		retire {r0, r1}, {this, ast}

		;evaluate based on type
		assign {ast->obj_vtable}, {value}
		switch
		case {value == @class/class_symbol}
			;eval to symbol value
			static_call lisp, env_get, {this, ast}, {value}
			breakif {value}
			static_call lisp, error, {this, "variable not defined", ast}
			break
		case {value == @class/class_boxed_ptr \
		 	|| value == @class/class_boxed_long \
			|| value == @class/class_pair \
			|| value == @class/class_unordered_set \
			|| value == @class/class_unordered_map}
			;eval to self
			assign {ast}, {value}
			static_call ref, ref, {value}
			break
		case {value == @class/class_vector}
			static_call vector, get_length, {ast}, {func}
			ifnot {func}
				;eval to nil
				assign {this->lisp_sym_nil}, {value}
				static_call ref, ref, {value}
			else
				;apply function
				static_call vector, get_element, {ast, 0}, {value}
				static_call lisp, repl_eval, {this, value}, {value}
				breakifnot {value}
				assign {value}, {func}
				static_call lisp, repl_apply, {this, func, ast}, {value}
				static_call ref, deref, {func}
			endif
			break
		default
			static_call lisp, error, {this, "unknown type", ast}
			assign {0}, {value}
		endswitch

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
