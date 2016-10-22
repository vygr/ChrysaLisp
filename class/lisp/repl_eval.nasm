%include 'inc/func.inc'
%include 'class/class_symbol.inc'
%include 'class/class_vector.inc'
%include 'class/class_boxed_ptr.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl_eval
		;inputs
		;r0 = lisp object
		;r1 = form
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, form, value, func, args
		ulong length

		push_scope
		retire {r0, r1}, {this, form}

		;evaluate based on type
		assign {form->obj_vtable}, {func}
		switch
		case {func == @class/class_symbol}
			;eval to symbol value
			static_call lisp, env_get, {this, form}, {value}
			break
		case {func == @class/class_vector}
			slot_call vector, get_length, {form}, {length}
			ifnot {length}
				;eval to nil
				assign {this->lisp_sym_nil}, {value}
				static_call ref, ref, {value}
			else
				;apply function, eval args if needed
				static_call vector, get_element, {form, 0}, {func}
				static_call lisp, repl_eval, {this, func}, {value}
				breakif {value->obj_vtable == @class/class_error}
				assign {value}, {func}
				gotoif {func->obj_vtable != @class/class_boxed_ptr}, eval_args
				if {func->boxed_ptr_flags}
					static_call lisp, repl_apply, {this, func, form}, {value}
				else
				eval_args:
					static_call vector, slice, {form, 0, length}, {args}
					static_call lisp, repl_eval_list, {this, args, 1}, {value}
					if {value->obj_vtable != @class/class_error}
						static_call ref, deref, {value}
						static_call lisp, repl_apply, {this, func, args}, {value}
					endif
					static_call ref, deref, {args}
				endif
				static_call ref, deref, {func}
			endif
			break
		default
			;eval to self
			assign {form}, {value}
			static_call ref, ref, {value}
		endswitch

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
