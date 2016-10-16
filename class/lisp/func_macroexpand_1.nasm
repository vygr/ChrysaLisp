%include 'inc/func.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_pair.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_macroexpand_1
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0 else value
		;r2 = 0 if expanded

		ptr this, args, value
		ulong length, flag

		push_scope
		retire {r0, r1}, {this, args}

		assign {0, 1}, {value, flag}
		slot_call vector, get_length, {args}, {length}
		if (length == 2)
			static_call vector, get_element, {args, 1}, {value}
			if {value->obj_vtable == @class/class_vector}
				static_call lisp, func_copy, {this, args}, {value}
				local_call check_macro_form, {this, &value}, {r0, r1}, {r0}, {flag}
			else
				static_call ref, ref, {value}
			endif
		else
			static_call lisp, error, {this, "(macroexpand-1 form) wrong number of args", args}
		endif

		eval {this, value, flag}, {r0, r1, r2}
		pop_scope
		return

	check_macro_form:
		;inputs
		;r0 = lisp object
		;r1 = iter to form
		;outputs
		;r0 = 0 if expanded

		pptr iter, miter
		ptr this, form, macro, args
		ulong length

		push_scope
		retire {r0, r1}, {this, iter}

		assign {*iter}, {form}
		slot_call vector, get_length, {form}, {length}
		if {length}
			static_call vector, get_element, {form, 0}, {macro}
			if {macro->obj_vtable == @class/class_symbol}
				static_call unordered_map, find, {this->lisp_macros, macro}, {miter, _}
				if {miter}
					static_call pair, get_second, {*miter}, {macro}
					static_call lisp, env_push, {this}
					static_call vector, get_element, {macro, 0}, {args}
					static_call lisp, env_def_list, {this, args, form, 1}, {form}
					if {form}
						static_call vector, get_element, {macro, 1}, {form}
						static_call lisp, repl_eval, {this, form}, {form}
					endif
					static_call lisp, env_pop, {this}
					if {form}
						static_call ref, deref, {*iter}
						assign {form}, {*iter}
					else
						debug_str "error expanding macro"
					endif
					eval {0}, {r0}
					return
				endif
			endif
			static_call vector, for_each, {form, 0, $callback, this}, {iter}
			assign {!iter}, {iter}
		endif

		eval {iter}, {r0}
		pop_scope
		return

	callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr pdata

		push_scope
		retire {r0, r1}, {iter, pdata}

		if {(*iter)->obj_vtable == @class/class_vector}
			local_call check_macro_form, {pdata, iter}, {r0, r1}, {r0}, {iter}
		endif

		eval {iter}, {r1}
		pop_scope
		return

	def_function_end
