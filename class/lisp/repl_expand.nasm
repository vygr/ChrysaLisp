%include 'inc/func.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_pair.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl_expand
		;inputs
		;r0 = lisp object
		;r1 = iter to form
		;outputs
		;r0 = lisp object
		;r1 = 0 if expanded

		pptr iter, miter
		ptr this, form, macro, args
		ulong length

		push_scope
		retire {r0, r1}, {this, iter}

		assign {*iter}, {form}
		if {form->obj_vtable == @class/class_vector}
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
						eval {this, 0}, {r0, r1}
						return
					endif
				endif
				static_call vector, for_each, {form, 0, $callback, this}, {iter}
				assign {!iter}, {iter}
			endif
		endif

		eval {this, iter}, {r0, r1}
		pop_scope
		return

	callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		vp_xchg r0, r1
		s_jmp lisp, repl_expand, {r0, r1}

	def_function_end
